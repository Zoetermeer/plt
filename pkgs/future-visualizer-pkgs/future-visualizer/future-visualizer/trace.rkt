#lang racket/base
(require racket/contract
         (only-in racket/place parallel-profiling?)
         racket/place/private/profiling
         "private/visualizer-data.rkt"
         (only-in '#%futures reset-future-logs-for-tracing!))
(provide (struct-out future-event)
         (struct-out gc-info)
         (struct-out indexed-future-event)
         #;trace-futures
         parallel-profile
         (contract-out
          #;[start-future-tracing! (-> void?)]
          #;[stop-future-tracing! (-> void?)]
          #;[timeline-events (-> (listof indexed-future-event?))]
          #;[trace-futures-thunk ((-> any/c) . -> . (listof indexed-future-event?))]))

#;(define-syntax-rule (trace-futures e ...)
  (begin (start-future-tracing!)
         (begin (begin e ...)
                (stop-future-tracing!)
                (timeline-events))))

;;trace-futures-thunk : (-> any) -> (listof indexed-future-event)
#;(define (trace-futures-thunk thunk)
  (start-future-tracing!)
  (begin
    (thunk)
    (stop-future-tracing!)
    (timeline-events)))

(define-syntax-rule (parallel-profile e ...)
  (begin (reset-future-logs-for-tracing!)
         (parameterize ([parallel-profiling? #t])
           (begin 
             (start-polling-thread)
             (begin e ...)
             (request-trace)))))

