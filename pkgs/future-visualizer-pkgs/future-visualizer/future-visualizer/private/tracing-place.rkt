#lang racket
(require future-visualizer/trace 
         "profiling.rkt")
(provide place-main)

(define (place-main pch)
  (log "Tracing place began\n")
  (set-current-place-id! (place-channel-get pch))
  (define trace-communicator-chan (place-channel-get pch))
  (define mod (place-channel-get pch))
  (define fn (place-channel-get pch))
  (define-values (th ch) (spawn-polling-thread trace-communicator-chan))
  
  (define mod-fn (dynamic-require mod fn))
  (start-future-tracing!)
  (parallel-profiling? #t)
  (mod-fn pch))