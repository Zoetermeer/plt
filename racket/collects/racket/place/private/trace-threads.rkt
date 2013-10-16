#lang racket/base
(require racket/place
         (only-in future-visualizer/trace timeline-events))
(provide tracing-poll)

(define REQUEST-TRACE-MSG 'send-trace)
(define PLACE-CREATED-MSG 'place-created)
(define (is-trace-request? msg) (equal? REQUEST-TRACE-MSG msg))
(define (is-place-created-msg? msg) (equal? PLACE-CREATED-MSG msg))

;;tracing-poll : place-channel evt -> void
(define (tracing-poll parent-chan main-thread-evt)
  (λ () 
    (define child-chans '())
    (define evt (choice-evt parent-chan main-thread-evt))
    (let loop ([msg (sync evt)])
      (cond
        [(is-trace-request? msg) 
         (define logs 
           (for/list ([ch (in-list child-chans)])
             (place-channel-put ch REQUEST-TRACE-MSG)
             (place-channel-get ch)))
         (place-channel-put 
          parent-chan
          (cons (timeline-events) logs))]
        [(place-channel? msg) 
         ;New place created, stash
         (set! child-chans (cons msg child-chans))
         (loop (sync evt))]))))