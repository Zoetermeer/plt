#lang racket/base
(require (prefix-in pl- '#%place)
         racket/runtime-path
         (for-syntax racket/base)
         (only-in racket/list partition))
(provide create-tracing-place
         parallel-profiling?
         start-polling-thread
         request-trace
         announce-place-created
         i-am-finished
         parent-is)

(define parallel-profiling? (make-parameter #f))
(define-runtime-path tracing-place-module "tracing-place.rkt")

(define-struct future-event 
  (future-id 
    process-id 
    what 
    time 
    prim-name 
    user-data) #:prefab)

(define-struct gc-info 
  (major? 
   pre-used 
   pre-admin 
   code-page-total 
   post-used 
   post-admin 
   start-time 
   end-time 
   start-real-time 
   end-real-time) #:prefab)

;Is e a visualizer-related log message?
(define-syntax (log-msg? stx)
  (syntax-case stx ()
    [(_ e) 
     #'(cond 
         [(vector? e)
          (define v (vector-ref e 2))
          (or (future-event? v) (gc-info? v))]
         [else #f])]))

;;create-tracing-place : path symbol port port port -> place-descriptor
(define (create-tracing-place modpath fn in out err)
  (define-values (pdesc pin pout perr)
    (pl-dynamic-place tracing-place-module
                      'place-main
                      in
                      out
                      err))
  (define-values (our-pchan their-pchan) (pl-place-channel))
  (announce-place-created our-pchan)

  (pl-place-channel-put pdesc their-pchan)
  (pl-place-channel-put pdesc modpath)
  (pl-place-channel-put pdesc fn)  
  (values pdesc pin pout perr))



;Polling thread code
;There are two kinds of polling threads:
; Primary: the polling thread running in the parent place
; Secondary: polling threads running in child places
;Each channel name uses a prefix to indicate which type 
;of polling thread requires it.
(define all:polling-thread #f)
(define all:log-recv (make-log-receiver (current-logger) 'debug))
(define all:new-children-chan (make-channel))
(define primary:request-trace-chan (make-channel))
(define secondary:finished-chan (make-channel))
(define secondary:get-parent-chan-chan (make-channel))
(define secondary:parent-chan #f)

;;announce-place-created : place-channel -> void
;;Runs on main thread
(define (announce-place-created child-place-chan)
  (channel-put all:new-children-chan child-place-chan))

;;Runs on main thread
;;But only in a child place
(define (i-am-finished)
  (pl-place-channel-put 
    secondary:parent-chan
    (channel-get secondary:finished-chan))
  (pl-place-channel-get secondary:parent-chan))

;;parent-is : place-channel -> void
;;Runs on main thread
;;But only in a child place
(define (parent-is parent)
  (channel-put secondary:get-parent-chan-chan parent))
  
;;Runs on main thread
;;But only in the parent place
(define (request-trace)
  (channel-get primary:request-trace-chan))

;;Runs on main thread
;;Called in all places
(define (start-polling-thread #:in-parent-place? [in-parent-place? #t])
  (unless (and all:polling-thread (thread-running? all:polling-thread))
    (set! all:polling-thread 
      (thread (poll in-parent-place?)))))

;;poll : bool -> (-> void)
(define (poll primary-polling-thread?)
  (λ () 
    (let loop ([children '()]
               [my-log '()])
      (sync 
        (apply 
          choice-evt 
          `(,(apply 
                choice-evt 
                (map (λ (child)
                      (if (pl-place-channel? child)
                        (handle-evt
                          child
                          (λ (messages)
                            (loop (for/list ([child′ (in-list children)])
                                    (if (equal? child child′)
                                      (begin 
                                        (pl-place-channel-put child 'received)
                                        messages)
                                      child′))
                                  my-log)))
                        never-evt))
                      children))
              ,(handle-evt all:log-recv
                  (λ (new-entry)
                    (loop children
                          (if (log-msg? new-entry)
                            (cons (vector-ref new-entry 2) my-log)
                            my-log))))
              ,(handle-evt all:new-children-chan
                  (λ (val) 
                    (loop (cons val children) my-log)))
              ,@(if primary-polling-thread?
                  `(,(handle-evt 
                        (channel-put-evt primary:request-trace-chan 
                          (cons my-log children))
                        (λ (_) (void))))
                  `(,(handle-evt secondary:get-parent-chan-chan
                        (λ (val)
                          (set! secondary:parent-chan val)
                          (loop children my-log)))
                    ,(handle-evt (channel-put-evt secondary:finished-chan (cons my-log children))
                        (λ (_) (void)))))))))))
