#lang racket/base
(require (prefix-in pl- '#%place)
         racket/runtime-path
         (for-syntax racket/base)
         (only-in "visualizer-data.rkt" 
                  timeline-events 
                  stop-future-tracing!
                  indexed-future-event-fevent))
(provide tracing-poll
         create-tracing-place
         parallel-profiling?
         request-trace
         announce-place-created
         spawn-polling-thread
         set-current-place-id!
         log)

(define REQUEST-TRACE-MSG 'send-trace)
(define (is-trace-request? msg) (equal? REQUEST-TRACE-MSG msg))

(define current-place-id 1)
(define next-id (+ current-place-id 1))
(define (set-current-place-id! v)
  (set! current-place-id v))

(define (log fmt . args)
  (printf "(Place ~a) ~a\n" current-place-id (apply format `(,fmt ,@args))))

(define polling-thread-chan #f)
(define parallel-profiling? (make-parameter #f))
(define-runtime-path tracing-place-module "tracing-place.rkt")

;;create-tracing-place : path symbol port port port -> place-descriptor
(define (create-tracing-place modpath fn in out err)
  (define-values (chan pin pout perr)
    (pl-dynamic-place tracing-place-module
                      'place-main
                      in
                      out
                      err))
  (define-values (their-trace-chan our-trace-chan) (pl-place-channel))
  (announce-place-created our-trace-chan polling-thread-chan)
  (pl-place-channel-put chan (begin0 
                               next-id 
                               (set! next-id (+ next-id 1))))
  (pl-place-channel-put chan their-trace-chan)
  (pl-place-channel-put chan modpath)
  (pl-place-channel-put chan fn)  
  (values chan pin pout perr))

;;spawn-polling-thread! : place-channel -> thread channel
(define (spawn-polling-thread parent-chan)
  (define ch (make-channel))
  (set! polling-thread-chan ch)
  (values (thread (tracing-poll parent-chan ch))
          ch))

;;announce-place-created : channel place-channel -> void
(define (announce-place-created child-chan polling-chan)
  (channel-put polling-chan child-chan))
  
(define (request-trace poll-thd chan)
  (cond 
    [poll-thd
     (channel-put chan REQUEST-TRACE-MSG)
     (log "requesting trace...\n")
     (channel-get chan)]
    [else '()]))

(define-struct future-event (future-id process-id what time prim-name user-data) #:prefab)
(define-struct gc-info (major? pre-used pre-admin code-page-total post-used post-admin start-time end-time start-real-time end-real-time) #:prefab)

(define-syntax (log-msg? stx)
  (syntax-case stx ()
    [(_ e) 
     #'(cond 
         [(vector? e)
          (define v (vector-ref e 2))
          (or (future-event? v) (gc-info? v))]
         [else #f])]))


;;tracing-poll : place-channel evt -> void
(define (tracing-poll parent-chan main-thread-evt)
  (log "tracing thread started\n")
  ;If this is the 'parent' thread, we are using a regular 
  ;channel to send back logs
  (define parent-chan-put 
    (cond 
      [(pl-place-channel? parent-chan) pl-place-channel-put]
      [else channel-put]))
  (define log-recv (make-log-receiver (current-logger) 'debug))
  (define evt (choice-evt parent-chan main-thread-evt log-recv))
  (λ () 
    (let loop ([child-chans '()]
               [log-msgs '()]
               [msg (sync/timeout 0.2 evt)])
      (log "Thread synced...~a child channels\n" (length child-chans))
      (cond
        [(log-msg? msg)
         (loop child-chans (cons (vector-ref msg 2) log-msgs) (sync/timeout 0.2 evt))]
        [(is-trace-request? msg) 
         (log "Polling thread received a trace request.\n")
         (stop-future-tracing!)
         (define child-logs 
           (for/list ([ch (in-list child-chans)])
             (pl-place-channel-put ch REQUEST-TRACE-MSG)
             (log "waiting for logs from child...\n")
             (begin0 
               (pl-place-channel-get ch)
               (log "received\n"))))
         (define logs (timeline-events))
         (log "About to send back ~a future events" (length logs))
         (parent-chan-put 
          parent-chan
          (cons log-msgs child-logs))]
        [(pl-place-channel? msg) 
         ;New place created, hang onto its channel
         (log "Polling thread detected new place creation\n")
         (loop (cons msg child-chans) log-msgs (sync/timeout 0.2 evt))]
        [else 
         (log "Polling thread saw some unidentified event (or timeout)...\n")
         (loop child-chans log-msgs (sync/timeout 0.2 evt))]))))