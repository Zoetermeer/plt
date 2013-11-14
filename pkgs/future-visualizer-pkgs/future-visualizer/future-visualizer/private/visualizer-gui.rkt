#lang racket/gui 
(require framework 
         data/interval-map 
         mrlib/hierlist
         "visualizer-drawing.rkt" 
         "visualizer-data.rkt" 
         "gui-helpers.rkt" 
         "graph-drawing.rkt"
         "display.rkt" 
         "constants.rkt" 
         "pict-canvas.rkt")    

(provide show-profiler) 

;;rebuild-mouse-index : frame-info trace (listof segment) -> interval-map of (range --> interval-map)
(define (rebuild-mouse-index frameinfo tr segs) 
  (let ([ym (make-interval-map)]) 
    (for ([tl (in-list (trace-proc-timelines tr))]) 
      (define xm (make-interval-map)) 
      (define-values (miny maxy) 
        (cond 
          [(equal? (process-timeline-proc-id tl) 'gc) 
           (values 0 (frame-info-adjusted-height frameinfo))] 
          [else 
           (define midy (calc-row-mid-y (process-timeline-proc-index tl)
                                        (frame-info-row-height frameinfo) 
                                        tr)) 
           (values (floor (- midy (/ MIN-SEG-WIDTH 2))) 
                   (floor (+ midy (/ MIN-SEG-WIDTH 2))))]))
        (interval-map-set! ym 
                           miny 
                           maxy 
                           xm) 
        (for ([seg (in-list (filter (λ (s) 
                                      (equal? (event-proc-id (segment-event s)) 
                                              (process-timeline-proc-id tl))) 
                                    segs))]) 
          (interval-map-set! xm 
                             (segment-x seg) 
                             (+ (segment-x seg) (segment-width seg)) 
                             seg)))
    ym)) 

;;display-evt-details : segment trace message message message message message message -> void
(define (display-evt-details seg 
                             tr
                             selected-label 
                             time-label 
                             fid-label 
                             pid-label 
                             data-label1 
                             data-label2) 
  (if seg 
      (let ([evt (segment-event seg)]) 
        (send selected-label set-label (format "Event: ~a" (event-type evt))) 
        (send time-label set-label (format "Time: +~a" (get-time-string (- (event-start-time evt) 
                                                                           (trace-start-time tr))))) 
        (send fid-label set-label (format "Future ID: ~a" (if (not (event-future-id evt)) 
                                                              "None (top-level event)" 
                                                              (event-future-id evt)))) 
        (send pid-label set-label (format "Process ID: ~a" 
                                          (let ([id (event-proc-id evt)]) 
                                            (if (equal? id 'gc) 
                                                RT-THREAD-ID 
                                                id))))
        (case (event-type evt) 
          [(start-work start-0-work gc) 
           (send data-label1 set-label (format "Duration: ~a" (get-time-string (- (event-end-time evt) 
                                                                                  (event-start-time evt)))))] 
          [(block sync) 
           (if (event-prim-name evt) 
             (send data-label1 set-label (format "Primitive: ~a" (symbol->string (event-prim-name evt)))) 
             (send data-label1 set-label ""))
           (define label2-txt 
             (cond 
               ;If the log was unexpectedly truncated, some worker blocks/syncs may not have been linked 
               ;with their runtime counterparts -- check to make sure prim-name is present
               [(event-prim-name evt)
                (cond  
                  [(touch-event? evt) (format "Touching future ~a" (event-user-data evt))]
                  [(allocation-event? evt) (format "Size: ~a" (event-user-data evt))] 
                  [(jitcompile-event? evt) (format "Jitting: ~a" (event-user-data evt))] 
                  [else ""])] 
               [else ""]))
           (send data-label2 set-label label2-txt)] 
          [(create) 
           (send data-label1 set-label (format "Creating future ~a" (event-user-data evt)))] 
          [(touch) 
           (send data-label1 set-label (format "Touching future ~a" (event-user-data evt)))]
          [else 
           (send data-label1 set-label "")
           (send data-label2 set-label "")]))  
      (begin 
        (send selected-label set-label "") 
        (send time-label set-label "") 
        (send fid-label set-label "") 
        (send pid-label set-label "") 
        (send data-label1 set-label "") 
        (send data-label2 set-label ""))))

(define (get-window-size) 
  (define-values (screen-w screen-h) (get-display-size)) 
  (values (min screen-w DEF-WINDOW-WIDTH) 
          (min screen-h DEF-WINDOW-HEIGHT)))

;;render-timeline-tab : tab-panel% (listof place-node) uint -> void
(define (render-timeline-tab tp pns index)
  (define pn (list-ref pns index))
  (cond 
    [(or (null? (place-node-logs pn)) 
         (null? (filter future-event? (place-node-logs pn)))) 
      (void)]
    [else 
      (define the-trace (build-trace (sort/index (place-node-logs pn))))  
      (define-values (winw winh) (get-window-size))
      ;The event segment we are currently mousing over
      (define hover-seg #f) 
      ;The event segment we last clicked on (tacked) -- if any
      (define tacked-seg #f)
      ;Table for broadcasting selection events to other controls
      (define listener-table (make-listener-table))
      (define main-panel (new panel:horizontal-dragable% 
                              [parent tp]))
      
      (define left-panel (new panel:horizontal-dragable% [parent main-panel] 
                              [stretchable-width #t]))

      (define summary-panel 
        (new pict-canvas% 
          [parent left-panel]
          [pict-builder (λ (vregion) 
            (program-summary-pict the-trace vregion))]
          [style '(hscroll vscroll)] 
          [stretchable-width #t]))
      
      (define right-panel (new panel:vertical-dragable% 
                              [parent main-panel] 
                              [stretchable-width #t]))
      (define graphic-panel (new panel:horizontal-dragable% 
                                 [parent right-panel] 
                                 [stretchable-height #t] 
                                 [stretchable-width #t]))
      (define timeline-container (new vertical-panel% 
                                      [parent graphic-panel] 
                                      [stretchable-width #t] 
                                      [stretchable-height #t])) 
      (define graph-container (new vertical-panel% 
                                   [parent graphic-panel]
                                   [stretchable-width #t]
                                   [stretchable-height #t])) 
      (define timeline-header (section-header timeline-container "Execution Timeline" 'horizontal)) 
      (define graph-header (section-header graph-container "Future Creation Tree" 'horizontal))
      
      ;Calculate required sizes, mouse hover index, and create timeline pict container
      (define-values (frameinfo segments) (calc-segments the-trace))
      (define timeline-mouse-index (rebuild-mouse-index frameinfo the-trace segments))   
      (define timeline-panel 
        (new pict-canvas% 
          [parent timeline-container]
          [pict-builder (λ (vregion) (timeline-pict-for-trace-data vregion the-trace frameinfo segments))] 
          [hover-handler (λ (x y vregion) 
                           (let ([seg (find-seg-for-coords x y timeline-mouse-index)])
                             (cond 
                               [(equal? seg hover-seg) #f] 
                               [else 
                                (set! hover-seg seg) 
                                (post-event listener-table 'segment-hover timeline-panel seg) 
                                (if (not seg) #t seg)])))]
          [click-handler (λ (x y vregion) 
                           (let ([seg (find-seg-for-coords x y timeline-mouse-index)]) 
                             (set! tacked-seg seg)  
                             (post-event listener-table 'segment-click timeline-panel seg) 
                             seg))] 
          [overlay-builder (λ (vregion scale-factor) 
                             (timeline-overlay vregion 
                                               tacked-seg 
                                               hover-seg 
                                               frameinfo 
                                               the-trace))]
          [style '(hscroll vscroll)] 
          [stretchable-width #t]))
      ;; TODO sometimes the sizes passed to the scrollbars are so big we blow up!
      (send timeline-panel init-auto-scrollbars 
            (frame-info-adjusted-width frameinfo) 
            (frame-info-adjusted-height frameinfo) 
            0.0 
            0.0)
      (send timeline-panel show-scrollbars #t #t)
      
      ;Calculate for and create creation graph pict container
      (define creation-tree-layout (draw-tree (trace-creation-tree the-trace) 
                                              #:node-width CREATE-GRAPH-NODE-DIAMETER 
                                              #:padding CREATE-GRAPH-PADDING 
                                              #:zoom CREATE-GRAPH-DEFAULT-ZOOM))
      
      (define hovered-graph-node #f)
      (define creategraph-panel 
        (new pict-canvas% 
           [parent graph-container]
           [pict-builder (λ (vregion) 
                           (draw-creategraph-pict 
                              vregion 
                              creation-tree-layout))]
           [hover-handler (λ (x y vregion) 
                            (define hovered 
                              (find-node-for-coords x 
                                                    y 
                                                    (graph-layout-nodes creation-tree-layout))) 
                            (cond 
                              [(eq? hovered hovered-graph-node) #f] 
                              [else 
                               (set! hovered-graph-node 
                                     (find-node-for-coords x 
                                                           y 
                                                           (graph-layout-nodes creation-tree-layout))) 
                               (if (not hovered-graph-node) #t hovered-graph-node)]))]
           [click-handler (λ (x y vregion)
                            (define fid (find-fid-for-coords 
                                          x 
                                          y 
                                          (graph-layout-nodes creation-tree-layout)
                                          vregion))
                            (cond 
                              [(not fid) #f] 
                              [else
                               (define seg (first-seg-for-fid fid segments))
                               (set! tacked-seg seg) 
                               (send timeline-panel redraw-everything)
                               (post-event listener-table 'segment-click timeline-panel seg) 
                               #t]))]
           [overlay-builder (λ (vregion scale-factor) 
                              (graph-overlay-pict hovered-graph-node 
                                                  the-trace 
                                                  creation-tree-layout 
                                                  vregion 
                                                  scale-factor))]
           [style '(hscroll vscroll)] 
           [stretchable-width #t]))   
        
      (send creategraph-panel show-scrollbars #t #t)
      (send creategraph-panel init-auto-scrollbars 
            (inexact->exact (floor (graph-layout-width creation-tree-layout))) 
            (inexact->exact (floor (graph-layout-height creation-tree-layout)))
            0.0 
            0.0) 
      
      
      (define graph-footer (new horizontal-panel% 
                                [parent graph-container] 
                                [stretchable-width #t]
                                [stretchable-height #f]
                                [style '(border)]))
      (define cg-zoom-level CREATE-GRAPH-DEFAULT-ZOOM)
      
      ;;Handles a change event for the creation graph zoom slider 
      ;;on-zoom : slider% event% -> void
      (define (on-zoom slider event)
        (send creategraph-panel set-scale-factor! (zoom-level->factor (send slider get-value)))
        (send creategraph-panel redraw-everything))
        
      (define zoom-slider (new slider% 
                               [parent graph-footer] 
                               [label "Zoom:"] 
                               [min-value CREATE-GRAPH-MIN-ZOOM] 
                               [max-value CREATE-GRAPH-MAX-ZOOM] 
                               [init-value CREATE-GRAPH-DEFAULT-ZOOM]
                               [style '(horizontal plain)]
                               [callback on-zoom]))
      
      ;Wire up events so selection, etc. in one panel is communicated to others 
      (define (on-future-selected fid) 
        0)
      
      (define (on-segment-hover seg) 
        0)
      
      (define (on-segment-click seg) 
        0)
      
      (define (on-segment-unclick seg) 
        0)
      
      ;Wire up event handlers for selection, etc.
      (add-receiver listener-table 'future-selected timeline-panel on-future-selected)
      (add-receiver listener-table 'segment-hover creategraph-panel on-segment-hover) 
      (add-receiver listener-table 'segment-click creategraph-panel on-segment-click)
      #;(add-receiver listener-table 'segment-click right-bot-panel update-event-details-panel) 
      #;(add-receiver listener-table 'segment-hover right-bot-panel update-event-details-panel)
      #;(add-receiver listener-table 'segment-unclick right-bot-panel update-event-details-panel)
      (add-receiver listener-table 'segment-unclick creategraph-panel on-segment-unclick)
      
      ;Additional menus/items 
      #;(define showing-create-graph #t)
      #;(define view-menu (new menu% [label "View"] [parent (send f get-menu-bar)]))
      #;(new menu-item% 
           [label "Hide Creation Tree"] 
           [parent view-menu] 
           [callback (λ (item evt) 
                       (if showing-create-graph 
                           (begin 
                             (send graphic-panel delete-child graph-container) 
                             (send item set-label "Show Creation Tree")) 
                           (begin 
                             (send graphic-panel add-child graph-container) 
                             (send item set-label "Hide Creation Tree"))) 
                       (set! showing-create-graph (not showing-create-graph)))])
      (send main-panel set-percentages `(,(/ 1 5) ,(/ 4 5)))]))

;;render-overview : tab-panel% place-node -> void
(define (render-overview tp pn-tree)
  (new pict-canvas% 
    [parent tp]
    [pict-builder (λ (vregion) (place-topology-map vregion pn-tree))]
    [style '(hscroll vscroll)]
    [stretchable-width #t]))

(define (show-profiler logs)
  (define topo (place-topology logs))
  (define places (flatten-topology topo))

  (define-values (winw winh) (get-window-size))
  (define f (new frame:standard-menus%
                  [label "Parallel Profile"]
                  [width winw]
                  [height winh]))
  (define tabp (new tab-panel%
                  [choices (cons "Overview" (map (λ (pn) (format "Place ~a" (place-node-id pn))) places))]
                  [parent (send f get-area-container)]
                  [callback (λ (tp evt) 
                              (define choice (send tp get-selection))
                              (for-each (λ (c) (send tp delete-child c)) (send tp get-children)) 
                              (case choice 
                                [(0) (render-overview tp topo)]
                                [else (render-timeline-tab tp places (- choice 1))]))]))
  (render-overview tabp topo)
  (send f show #t))
