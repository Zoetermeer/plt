#lang racket/base
(require (prefix-in pl- '#%place)
         racket/runtime-path
         (for-syntax racket/base)
         (only-in racket/list partition))
(provide #;tracing-poll
         create-tracing-place
         parallel-profiling?
         request-trace
         send-trace-upstream
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

(define polling-thread-chan (make-channel))
(define polling-for-parent-chan (make-channel))
(define polling-for-children-chan (make-channel))
(define log-recv (make-log-receiver (current-logger) 'debug))

(define parallel-profiling? (make-parameter #f))
(define-runtime-path tracing-place-module "tracing-place.rkt")

(define-struct future-event (future-id process-id what time prim-name user-data) #:prefab)
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

;;create-tracing-place : path symbol port port port -> place-descriptor
(define (create-tracing-place modpath fn in out err)
  (define-values (pdesc pin pout perr)
    (pl-dynamic-place tracing-place-module
                      'place-main
                      in
                      out
                      err))
  (define-values (their-trace-chan our-trace-chan) (pl-place-channel))
  (announce-place-created our-trace-chan)
  (put pdesc (begin0 
               next-id 
               (set! next-id (+ next-id 1))))
  (put pdesc their-trace-chan)
  (put pdesc modpath)
  (put pdesc fn)  
  (values pdesc pin pout perr))

;;spawn-polling-thread! : place-channel -> void
;; THREAD: runs on place's main thread
#;(define (spawn-polling-thread parent-chan)
  (set! polling-thread (thread (tracing-poll)))
  (channel-put polling-thread-chan parent-chan))

;;announce-place-created : channel -> void
(define (announce-place-created child-chan)
  (channel-put polling-for-children-chan child-chan))

(define (i-am-finished)
  (channel-put polling-thread-chan SEND-UPSTREAM-MSG)
  (thread-wait polling-thread))

;;parent-is : place-channel -> void
(define (parent-is parent)
  (channel-put polling-for-parent-chan parent))

(define (send-trace-upstream ch)
  (channel-put ch SEND-UPSTREAM-MSG))
  
(define (request-trace chan)
  (channel-put chan REQUEST-TRACE-MSG)
  (channel-get chan))

(define-syntax (log-msg? stx)
  (syntax-case stx ()
    [(_ e) 
     #'(cond 
         [(vector? e)
          (define v (vector-ref e 2))
          (or (future-event? v) (gc-info? v))]
         [else #f])]))

(define (main-thread-terminated? evt)
  (thread? evt))

(define (trace-from-child? evt)
  (list? evt))

(define (child-place-spawned? evt)
  (and (pair? evt)
    (pl-place-channel? (car evt))
    (pl-place-channel? (cdr evt))))

(define (put ch v)
  (if (pl-place-channel? ch)
    (pl-place-channel-put ch v)
    (channel-put ch v)))

(define polling-thread 
  (thread 
   (λ ()
     (let loop (;; children : (listof (or/c chan (listof log-message)))
                [children '()]
                [my-log '()]
                [parent-chan #f])
        (log "loop")
       (sync
        (apply
         choice-evt
         (map (λ (child)
                (if (pl-place-channel? child)
                    (handle-evt
                     child
                     (λ (messages) 
                      (log "receive")
                       (loop (for/list ([child2 (in-list children)])
                               (if (equal? child child2)
                                   messages
                                   child2))
                             my-log
                             parent-chan)))
                    never-evt))
              children)) 
        (handle-evt (if parent-chan parent-chan never-evt) 
                    (λ (val) 
                      (cond
                        ;TODO: return child logs on trace-request
                        [(is-trace-request? val) 
                          (put parent-chan (cons my-log children))]
                        [(pl-place-channel? val) 
                         (loop children my-log val)])))
        (handle-evt log-recv 
                    (λ (new-entry)
                      (log "future log msg")
                      (loop children 
                            (if (log-msg? new-entry)
                                (cons new-entry my-log)
                                my-log)
                            parent-chan)))
        (handle-evt polling-for-parent-chan
                    (λ (val)
                      (loop children my-log val)))
        (handle-evt polling-for-children-chan
                    (λ (val) 
                      (loop (cons val children) my-log parent-chan)))
        (handle-evt polling-thread-chan
                    (λ (val) 
                      (cond 
                        [(is-send-upstream-req? val) 
                          (log "Send upstream request")
                         ;Send my parent channel down to all children
                         ;Send my children up to my parent
                         ;Send my logs back to my parent
                         (define-values (child-chans child-logs) 
                           (partition pl-place-channel? children)) 
                         (for-each (λ (c) (put c parent-chan)) children)
                         ;(put parent-chan child-chans)
                         (put parent-chan (cons my-log child-logs))]))))))))
