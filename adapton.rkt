#lang racket/base

(require racket/set)

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
