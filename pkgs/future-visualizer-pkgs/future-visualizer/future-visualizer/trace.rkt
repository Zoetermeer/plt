#lang racket/base
(require racket/contract
         (only-in racket/place parallel-profiling?)
         racket/place/private/profiling
         "private/visualizer-data.rkt"
         (only-in '#%futures reset-future-logs-for-tracing!))
(provide (struct-out future-event)
         (struct-out gc-info)
         (struct-out indexed-future-event)
         parallel-profile)

(define-syntax-rule (parallel-profile e ...)
  (begin (reset-future-logs-for-tracing!)
         (parameterize ([parallel-profiling? #t])
           (begin 
             (start-polling-thread)
             (begin e ...)
             (request-trace)))))

