#lang racket/base
(require racket/contract
         "private/profiling.rkt"
         "private/visualizer-data.rkt")
(provide (struct-out future-event)
         (struct-out gc-info)
         (struct-out indexed-future-event)
         trace-futures
         parallel-profile
         (contract-out
          [start-future-tracing! (-> void?)]
          [stop-future-tracing! (-> void?)]
          [timeline-events (-> (listof indexed-future-event?))]
          [trace-futures-thunk ((-> any/c) . -> . (listof indexed-future-event?))]))

(define-syntax-rule (trace-futures e ...)
  (begin (start-future-tracing!)
         (begin (begin e ...)
                (stop-future-tracing!)
                (timeline-events))))

;;trace-futures-thunk : (-> any) -> (listof indexed-future-event)
(define (trace-futures-thunk thunk)
  (start-future-tracing!)
  (begin
    (thunk)
    (stop-future-tracing!)
    (timeline-events)))

(define-syntax-rule (parallel-profile e ...)
  (begin (start-future-tracing!)
         (parameterize ([parallel-profiling? #t])
           (begin 
             (define-values (pthd chan) (spawn-polling-thread (make-channel))) 
             (begin e ...)
             (stop-future-tracing!)
             (log "getting traces...\n")
             (request-trace pthd chan)))))
