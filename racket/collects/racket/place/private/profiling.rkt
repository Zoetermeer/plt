#lang racket/base
(require (prefix-in pl- '#%place)
         racket/runtime-path
         (for-syntax racket/base)
         (only-in racket/list partition))
(provide create-tracing-place
         parallel-profiling?
         start-polling-thread
         request-trace
         announce-place-created
         i-am-finished
         parent-is
         set-current-place-id!
         log)

(define REQUEST-TRACE-MSG 'send-trace)
(define SEND-UPSTREAM-MSG 'send-upstream)
(define ARE-YOU-FLUSHED-MSG 'are-you-flushed?)
(define (is-trace-request? msg) (equal? REQUEST-TRACE-MSG msg))
(define (is-send-upstream-req? msg) (equal? SEND-UPSTREAM-MSG msg))
(define (is-flushed-inquiry? msg) (equal? ARE-YOU-FLUSHED-MSG msg))

(define current-place-id 1)
(define next-id (+ current-place-id 1))
(define (set-current-place-id! v)
  (set! current-place-id v))

(define (log fmt . args)
  (printf "(Place ~a) ~a\n" current-place-id (apply format `(,fmt ,@args))))

(define parallel-profiling? (make-parameter #f))
(define-runtime-path tracing-place-module "tracing-place.rkt")

(define-struct future-event 
  (future-id 
    process-id 
    what 
    time 
    prim-name 
    user-data) #:prefab)

(define-struct gc-info (major? 
                        pre-used 
                        pre-admin 
                        code-page-total 
                        post-used 
                        post-admin 
                        start-time 
                        end-time 
                        start-real-time 
                        end-real-time) #:prefab)

;Is e a visualizer-related log message?
(define-syntax (log-msg? stx)
  (syntax-case stx ()
    [(_ e) 
     #'(cond 
         [(vector? e)
          (define v (vector-ref e 2))
          (or (future-event? v) (gc-info? v))]
         [else #f])]))

;;create-tracing-place : path symbol port port port -> place-descriptor
(define (create-tracing-place modpath fn in out err)
  (define-values (pdesc pin pout perr)
    (pl-dynamic-place tracing-place-module
                      'place-main
                      in
                      out
                      err))
  (define-values (our-pchan their-pchan) (pl-place-channel))
  (announce-place-created our-pchan)

  (pl-place-channel-put pdesc 
                        (begin0 
                          next-id 
                          (set! next-id (+ next-id 1))))
  (pl-place-channel-put pdesc their-pchan)
  (pl-place-channel-put pdesc modpath)
  (pl-place-channel-put pdesc fn)  
  (values pdesc pin pout perr))



;Polling thread code
;There are two kinds of polling threads:
; Primary: the polling thread running in the parent place
; Secondary: polling threads running in child places
;Each channel name uses a prefix to indicate which type 
;of polling thread requires it.
(define all:polling-thread #f)
(define all:log-recv (make-log-receiver (current-logger) 'debug))
(define all:new-children-chan (make-channel))
(define primary:request-trace-chan (make-channel))
(define secondary:finished-chan (make-channel))
(define secondary:get-parent-chan-chan (make-channel))
(define secondary:parent-chan #f)

;;announce-place-created : place-channel -> void
;;Runs on main thread
(define (announce-place-created child-place-chan)
  (channel-put all:new-children-chan child-place-chan))

;;Runs on main thread
;;But only in a child place
(define (i-am-finished)
  (pl-place-channel-put 
    secondary:parent-chan
    (channel-get secondary:finished-chan)))

;;parent-is : place-channel -> void
;;Runs on main thread
;;But only in a child place
(define (parent-is parent)
  (channel-put secondary:get-parent-chan-chan parent))
  
;;Runs on main thread
;;But only in the parent place
(define (request-trace)
  (channel-get primary:request-trace-chan))

;;Runs on main thread
;;Called in all places
(define (start-polling-thread #:in-parent-place? [in-parent-place? #t])
  (unless (and all:polling-thread (thread-running? all:polling-thread))
    (set! all:polling-thread 
      (thread (poll in-parent-place?)))))

;;poll : bool -> (-> void)
(define (poll primary-polling-thread?)
  (λ () 
    (let loop ([children '()]
               [my-log '()])
      (sync 
        (apply 
          choice-evt 
          `(,(apply 
                choice-evt 
                (map (λ (child)
                      (if (pl-place-channel? child)
                        (handle-evt
                          child
                          (λ (messages)
                            (log "child finished")
                            (loop (for/list ([child′ (in-list children)])
                                    (if (equal? child child′)
                                      messages
                                      child′))
                                  my-log)))
                        never-evt))
                      children))
              ,(handle-evt all:log-recv
                  (λ (new-entry)
                    (loop children
                          (if (log-msg? new-entry)
                            (cons (vector-ref new-entry 2) my-log)
                            my-log))))
              ,(handle-evt all:new-children-chan
                  (λ (val) 
                    (loop (cons val children) my-log)))
              ,@(if primary-polling-thread?
                  `(,(handle-evt 
                        (channel-put-evt primary:request-trace-chan (cons my-log children))
                        (λ (_) (void))))
                  `(,(handle-evt secondary:get-parent-chan-chan
                        (λ (val)
                          (log "got a parent")
                          (set! secondary:parent-chan val)
                          (loop children my-log)))
                    ,(handle-evt (channel-put-evt secondary:finished-chan (cons my-log children))
                        (λ (_) (void)))))))))))










(define-syntax (if-not stx)
  (syntax-case stx ()
    [(_ e other)
     #'(begin 
        (let ([ev e])
          (if ev ev other)))]))

;The "child" polling thread
;(the thread running in any non-parent place)
;(define finished-chan (make-channel))
;(define get-parent-chan-chan (make-channel))
;(define parent-chan #f)
;(define log-recv (make-log-receiver (current-logger) 'debug))
#;(define child-polling-thread
  (λ () 
    ;;children : (listof (or/c place-channel (listof future-event)))
    ;;my-log : (listof future-event)
    (let loop ([children '()]
               [my-log '()])
      (sync
        (apply 
          choice-evt 
            (map (λ (child)
                    (if (pl-place-channel? child)
                      (handle-evt
                        child
                        (λ (messages)
                          (loop (for/list ([child′ (in-list children)])
                                  (if (equal? child child2)
                                    messages
                                    child′))
                                my-log)))
                      never-evt))
                  children)
            (handle-evt log-recv 
              (λ (val)
                (loop children 
                      (if (log-msg? new-entry)
                          (cons (vector-ref new-entry 2) my-log)
                          my-log))))
            (handle-evt get-parent-chan-chan
              (λ (val)
                (set! parent-chan val)
                (loop children my-log)))
            (handle-evt finished-chan
              (λ (val)
                (pl-place-channel-put parent-chan (cons my-log children)))))))))

;The "parent" thread (polling thread running 
;in the parent place
;(define parent-chan (make-channel))
;(define request-trace-chan (make-channel))
#;(define parent-polling-thread
  (λ ()
    (let loop ([children '()]
               [my-log '()])
      (sync 
        (apply 
          choice-evt 
          (map (λ (child)
                    (if (pl-place-channel? child)
                      (handle-evt
                        child
                        (λ (messages)
                          (loop (for/list ([child′ (in-list children)])
                                  (if (equal? child child2)
                                    messages
                                    child′))
                                my-log)))
                      never-evt))
                  children)
          (handle-evt (channel-put-evt request-trace-chan (cons my-log children))
            (λ (_)
              (void)))
          (handle-evt log-recv
            (λ (new-entry)
              (loop children 
                    (if (log-msg? new-entry)
                        (cons (vector-ref new-entry 2) my-log)
                        my-log)))))))))







;The old, dual-purpose thread function (defunct)
#;(define polling-thread 
  (thread 
   (λ ()
     (let loop (;; children : (listof (or/c chan (listof log-message)))
                [children '()]
                [my-log '()])
       (sync
        (apply
         choice-evt
         (map (λ (child)
                (if (pl-place-channel? child)
                    (handle-evt
                     child
                     (λ (messages) 
                       (loop (for/list ([child2 (in-list children)])
                               (if (equal? child child2)
                                   messages
                                   child2))
                             my-log)))
                    never-evt))
              children))
        (handle-evt (channel-put-evt request-trace-chan children)
                    (λ (_)
                      (log "trace request rcvd")
                      (void)))        
        ;; change to work only when parent-chan is the place channel of the parent
        (handle-evt (if-not the-parent-chan never-evt) 
                    (λ (val) 
                      ;TODO: return child logs on trace-request
                      (put the-parent-chan (cons my-log children))))
        (handle-evt log-recv 
                    (λ (new-entry)
                      (loop children 
                            (if (log-msg? new-entry)
                                (cons (vector-ref new-entry 2) my-log)
                                my-log))))
        (handle-evt polling-for-children-chan
                    (λ (val) 
                      (loop (cons val children) my-log)))
        (handle-evt (channel-put-evt 
                      wait-for-finish-chan
                      (let-values ([(_ child-logs) (partition pl-place-channel? children)])
                        (cons my-log child-logs)))
                    (λ (_) (void)))
        #;(handle-evt wait-for-finish-chan
                    (λ (val)
                      (log "finished") 
                      (define-values (child-chans child-logs) 
                        (partition pl-place-channel? children)) 
                      ;(for-each (λ (c) (put c parent-chan)) children)
                      ;(put parent-chan child-chans)
                      (put the-parent-chan (cons my-log child-logs)))))))))
