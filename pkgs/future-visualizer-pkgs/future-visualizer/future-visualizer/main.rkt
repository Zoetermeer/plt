#lang racket/base 
(require (for-syntax racket/base) 
         racket/contract 
         pict
         racket/bool 
         future-visualizer/trace
         "private/visualizer-gui.rkt"  
         "private/visualizer-drawing.rkt") 

(provide visualize-futures 
         (contract-out 
          #;[show-visualizer (->* () (#:timeline (listof indexed-future-event?)) void?)]
          [show-profiler (list? . -> . void?)]
          [visualize-futures-thunk ((-> any/c) . -> . any/c)]
          [timeline-pict (->i ([indexed-fevents (listof indexed-future-event?)]) 
                              (#:x [x (or/c #f exact-nonnegative-integer?)] 
                               #:y [y (or/c #f exact-nonnegative-integer?)] 
                               #:width [width (or/c #f exact-nonnegative-integer?)]
                               #:height [height (or/c #f exact-nonnegative-integer?)] 
                               #:selected-event-index [i (or/c #f exact-nonnegative-integer?)])
                              #:pre 
                              (x y width height)
                              (implies (or x y width height)
                                       (and x y width height))
                              [p pict?])]
          [creation-tree-pict (->i ([indexed-fevents (listof indexed-future-event?)]) 
                                    (#:x [x (or/c #f exact-nonnegative-integer?)] 
                                     #:y [y (or/c #f exact-nonnegative-integer?)] 
                                     #:width [width (or/c #f exact-nonnegative-integer?)] 
                                     #:height [height (or/c #f exact-nonnegative-integer?)] 
                                     #:node-width [node-width (or/c #f exact-nonnegative-integer?)]
                                     #:padding [padding (or/c #f exact-nonnegative-integer?)] 
                                     #:zoom [zoom (between/c 1 5)]) 
                                    #:pre 
                                    (x y width height) 
                                    (implies (or x y width height) 
                                             (and x y width height)) 
                                    [p pict?])]))

(define-syntax-rule (visualize-futures e ...)
  (show-profiler (parallel-profile e ...)))

;;visualize-futures-thunk : (-> any/c) -> any/c
(define (visualize-futures-thunk thunk)
  (show-profiler (parallel-profile (thunk))))






