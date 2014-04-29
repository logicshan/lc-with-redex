#lang racket
(require redex)
(require (only-in "curried-lc-grammar.ss"
		  curried-lc-grammar))
;; (require (only-in "normal-order-beta-reductor-version-1.ss"
;; 		  normal-order-β-reductor-version-1))
;; (require (only-in "normal-order-beta-reductor-version-2.ss"
;; 		  normal-order-β-reductor-version-2))

;(require "normal-order-beta-reductor-version-2.ss")
(require "normal-order-beta-reductor-version-1.ss")
(require (only-in "call-by-value-beta-reductor.rkt"
		  call-by-value-β-reductor))
(require (only-in "call-by-name-beta-reductor.rkt"
		  call-by-name-β-reductor))

(define call-by-name call-by-name-β-reductor)
(define call-by-value call-by-value-β-reductor)
(define normal-order normal-order-β-reductor-version-1)

(define church-numeral
  (λ (n)
    (if (= n 0)
	(term (λ (s) (λ (z) z)))
	(term (λ (s) (λ (z) (s ,(bind-exp
				  (car
				   (match-bindings
				    (car
				     (redex-match
				      curried-lc-grammar
				      (λ _ (λ _ ‹term›))
				      (church-numeral (- n 1))))))))))))))

(define (succ n) (+ n 1))
(define natify
  (λ (church-numeral)
     (((eval church-numeral) succ) 0)))
(define (red-natify t)
  (natify (car (apply-reduction-relation* normal-order t))))
(define boolify
  (λ (b)
     (((eval b) #t) #f)))
(define (red-boolify t)
  (boolify (car (apply-reduction-relation* normal-order t))))

(define SUCC (term (λ (n) (λ (f) (λ (x) (f ((n f) x)))))))
(define PLUS (term (λ (m) (λ (n) (λ (f) (λ (x) ((m f) ((n f) x))))))))
(define TRUE (term (λ (x) (λ (y) x))))
(define FALSE (term (λ (x) (λ (y) y))))
(define IFTHENELSE (term (λ (p) (λ (a) (λ (b) ((p a) b))))))
(define ISZERO (term (λ (n) ((n (λ (x) ,FALSE)) ,TRUE))))
(define Phi  (term (λ (p) (λ (z) ((z (,SUCC (p ,TRUE))) (p ,TRUE))))))
(define PRED (term (λ (n) (((n ,Phi) (λ (z) ((z ,(church-numeral 0))
					 ,(church-numeral 0)))) ,FALSE))))
(define Y (term (λ (g) ((λ (x) (g (x x))) (λ (x) (g (x x)))))))
(define Z (term (λ (f) ((λ (x) (f (λ (y) ((x x) y))))
			(λ (x) (f (λ (y) ((x x) y))))))))
(define SUM (term (λ (r) (λ (n) (((,IFTHENELSE (,ISZERO n)) ,(church-numeral 0)) ((,PLUS n) (r (,PRED n))))))))

(define omiga (term (λ (x) (x x))))
(define Omiga (term (,omiga ,omiga)))


;; (test-equal (red-boolify (term (,ISZERO ,(church-numeral 0))))
;; 	    #t)
;; (test-equal (red-natify (term (,SUCC ,(church-numeral 10)))) 11)
;; (test-equal (red-natify (term ((,PLUS ,(church-numeral 2)) ,(church-numeral 3))))
;; 	    5)
;; (test-equal (red-natify (term (,PRED ,(church-numeral 10)))) 9)

(define sum (term (,Y ,SUM)))
(define sumz (term (,Z ,SUM)))
(traces normal-order (term (,sumz ,(church-numeral 3))))

;(stepper normal-order (term ((,Y ,SUM) ,(church-numeral 1))))


;; (test-equal (red-natify (term ((,Y ,SUM) ,(church-numeral 3))))
;; 	    6)

;(traces normal-order Omi)

;; (traces normal-order
;; 	(term ((λ (x) y) ,Omi)))

;; (traces normal-order
;; 	(term (((,IFTHENELSE ,FALSE) ,(church-numeral 1)) ,(church-numeral 0))))

;; (traces normal-order
;; 	(term (,ISZERO ,(church-numeral 0))))
;; (traces normal-order
;; 	(term ((λ (x) x) (λ (x) x))))
;; (traces normal-order
;; 	(term x))
;; (stepper call-by-name
;; 	 (term (((,SUCC ((,PLUS ,(church-numeral 2)) ,(church-numeral 3))) f) x)))
;; (traces normal-order
;; 	(term ((,PLUS ,(church-numeral 2)) ,(church-numeral 3))))
