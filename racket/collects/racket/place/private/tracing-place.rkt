#lang racket
(require "profiling.rkt"
         (only-in '#%futures 
                  reset-future-logs-for-tracing!))
(provide place-main)

(define (place-main pch)
  (set-current-place-id! (place-channel-get pch))
  (log "Tracing place began\n")

  (define parent-chan (place-channel-get pch))
  (start-polling-thread #:in-parent-place? #f)
  (parent-is parent-chan)

  (define mod (place-channel-get pch))
  (define fn (place-channel-get pch))
  (define mod-fn (dynamic-require mod fn))
  
  (reset-future-logs-for-tracing!)
  (parallel-profiling? #t)
  (mod-fn pch)

  (i-am-finished))






