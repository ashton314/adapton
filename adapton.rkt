#lang racket/base

(require racket/set)

(module+ test
  (require rackunit))

;;; microAdapton implementation

(struct adapton (thunk result sub super clean?) #:transparent #:mutable)

(define (make-athunk thunk)
  (adapton thunk 'empty (mutable-set) (mutable-set) #f))

(define (adapton-add-dcg-edge! a-super a-sub)
  (set-add! (adapton-sub a-super) a-sub)
  (set-add! (adapton-super a-sub) a-super))

(define (adapton-del-dcg-edge! a-super a-sub)
  (set-remove! (adapton-sub a-super) a-sub)
  (set-remove! (adapton-super a-sub) a-super))

(define (adapton-compute a)
  (if (adapton-clean? a)
      ;; use the cache
      (adapton-result a)
      ;; otherwise, recompute
      (begin
        ;; throw away old dependencies
        (for ([sub (adapton-sub a)])
          (adapton-del-dcg-edge! a sub))
        ;; mark this node as "clean": if we re-encounter this node
        ;; somehow in computing its thunk, we won't keep spiraling out
        ;; of control
        (set-adapton-clean?! a #t)
        (set-adapton-result! a ((adapton-thunk a)))
        (adapton-compute a))))

(define (adapton-dirty! a)
  (when (adapton-clean? a)
    ;; set this dirty first; prevents re-traversals of call graph
    (set-adapton-clean?! a #f)
    (for ([s (adapton-super a)])
      (adapton-dirty! s))))

(define (adapton-ref val)
  (letrec ([a (adapton
               (λ () (adapton-result a))
               val
               (mutable-set)
               (mutable-set)
               #t)])
    a))

(define (adapton-ref-set! a val)
  (set-adapton-result! a val)
  (adapton-dirty! a))

;;; miniAdapton interface

(define adapton-force
  ;; Use this in place of `adapton-compute': it keeps track of the
  ;; adapton computation we're in and manages the DCG.
  (let ([currently-adapting #f])
    (λ (a)
      (let ([prev-adapting currently-adapting])
        (set! currently-adapting a)
        (let ([result (adapton-compute a)])
          (set! currently-adapting prev-adapting)
          (when currently-adapting (adapton-add-dcg-edge! currently-adapting a))
          result)))))

(define-syntax adapt
  ;; Convenience macro for wrapping expressions in adapton thunks
  (syntax-rules ()
    [(_ expr)
     (make-athunk (λ () expr))]))

;; TODO

;;; Auxillary functions

(define (memoize f)
  ;; Given a function, return a memoized variant
  (let ([cache (make-hash)])
    (λ x
      (hash-ref cache x
                (λ () (let ([r (apply f x)]) (hash-set! cache x r) r))))))

(module+ test
  (test-case "memoize"))
