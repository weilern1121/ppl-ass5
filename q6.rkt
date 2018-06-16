#lang racket
(require racket/sandbox)
(require racket/exn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: The lazy lists interface ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cons-lzl cons)

(define empty-lzl empty)

(define empty-lzl? empty?)

(define head car)

(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Auxiliary functions for testing ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: check-inf-loop(mission)
; Purpose: check if the result is infinite loop,
;          if so, return 'infinite
;          otherwise the actual result
; Type: [[Empty -> T1] -> Union(T1, Symbol)]
(define check-inf-loop
  (lambda (mission)
    (with-handlers ([exn:fail:resource?
                     (lambda (e)
                       (if (equal? (exn->string e)
                                   "with-limit: out of time\n")
                           'infinite
                           'error))])
      (call-with-limits 1 #f mission))))

; A function that creates an infinite loop
(define (inf x) (inf x))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3: The assignment ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: all-subs(long)
; Type: [List(T) -> LZL(List(T))]
; Purpose: compute all lists that can be obtained 
; from long by removing items from it.
; Pre-conditions: -
; Tests:
; (take (all-subs '(1 2 3)) 8) ->
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define all-subs
  (lambda (long)
    (if (null? long)
        (list null)
        (subsets long)
        )))

(define (subsets long)
  (if (null? long)
      (list null)
      (let ((rest (subsets (cdr long))))
        (append rest (map (lambda (x)
                            (cons (car long) x))
                          rest)))))

;;;;;;;;;;;;;;;;;;;;;
; Part 4: The tests ;
;;;;;;;;;;;;;;;;;;;;;
;(take (all-subs '(1 2 3)) 8)
;; Make sure to add take or another utility to test here
;; If the results are obained in a different order, change the test accordingly.
(check-inf-loop (lambda () (take (all-subs '(1 2 3)) 8)))
(check-inf-loop (lambda () (take (all-subs '())1)))
(check-inf-loop (lambda () (take (all-subs '(a b c d))16)))
(check-inf-loop (lambda () (take (all-subs '('() '(a b)))4)))
(check-inf-loop (lambda () (take (all-subs '(1 (take (all-subs '(2 3)) 4)))4)))
(check-inf-loop (lambda () (take (all-subs '(a 1 @))8)))

;; Write more tests - at least 5 tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5: The tests expected results;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(take (all-subs '(1 2 3)) 8)
#|
> (check-inf-loop (lambda () (take (all-subs '(1 2 3)) 8))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
|#

;(take (all-subs '())1)
#|
> (check-inf-loop (lambda () (take (all-subs '())1)))
'(())
|#

;(take (all-subs '(a b c d))16)
#|
> (check-inf-loop (lambda () (take (all-subs '(a b c d))16)
'(() (d) (c) (c d) (b) (b d) (b c) (b c d) (a) (a d) (a c) (a c d) (a b) (a b d) (a b c) (a b c d))
|#

;(take (all-subs '('() '(a b)))4)
#|
> (check-inf-loop (take (all-subs '('() '(a b)))4)))
'(() ('(a b)) ('()) ('() '(a b)))
|#

;(take (take (all-subs '(1 (take (all-subs '(2 3)) 4)))4)
#|
> (check-inf-loop (take (all-subs '(1 (take (all-subs '(2 3)) 4)))4)))
'(() ((take (all-subs '(2 3)) 4)) (1) (1 (take (all-subs '(2 3)) 4)))
|#

;(take (take (all-subs '(a 1 @))8)
#|
> (check-inf-loop (take (all-subs '(a 1 @))8)))
'(() (@) (1) (1 @) (a) (a @) (a 1) (a 1 @))
|#
