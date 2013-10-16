#lang racket
(require future-visualizer/trace
         "trace-threads.rkt")
(provide place-main)

(define (place-main pch)
  (define trace-communicator-chan (place-channel-get pch))
  (define mod (place-channel-get pch))
  (define fn (place-channel-get pch))
  (define thd-chan (make-channel))
  (thread (tracing-poll trace-communicator-chan thd-chan))
  
  (define mod-fn (dynamic-require mod fn))
  (start-future-tracing!)
  (mod-fn pch))