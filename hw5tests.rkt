;; CSE413 21sp, Programming Languages & Implementation, Homework 5 tests
;; Name: Shao-Hua Lo
;; UW-Netid: shl0905

#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want to add more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   

   ;; racketlist->mupllist
   (check-equal? (racketlist->mupllist
                  (list (int 1) (int 2) (int 3)))
                  (apair (int 1) (apair (int 2) (apair (int 3) (munit))))
                  "list function case")

   (check-equal? (racketlist->mupllist
                  (cons (int 1) (cons (int 2) null)))
                 (apair (int 1) (apair (int 2) (munit)))
                 "cons function case")

   (check-equal? (racketlist->mupllist null) (munit) "empty list")

   ;; mupplist->racketlist
   (check-equal? (mupllist->racketlist (munit)) null "empty MUPL-list -> racket list")

   (check-equal? (mupllist->racketlist (apair (int 1) (apair (int 2) (munit))))
                 (list (int 1) (int 2))
                 "list function case")

   (check-equal? (mupllist->racketlist (apair (int 1) (apair (int 2) (munit))))
                 (cons (int 1) (cons (int 2) null))
                 "cons function case")

   ;; int
   (check-equal? (eval-exp (int 3)) (int 3) "int evaluates to itself")

   ;; munit
   (check-equal? (eval-exp (munit)) (munit) "munit evaluates to itself")

   ;; closure
   (check-equal? (eval-exp (closure '() (fun null "x"
                                             (add (var "x") (var "x")))))
                 (closure '() (fun null "x"
                                   (add (var "x") (var "x"))))
                 "closure evaluates to itself")

   ;; add
   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")
   (check-equal? (eval-exp (add (int 2) (eval-exp (add (int 3) (int 4))))) (int 9) "nested add")

  

   ;; isgreater
   (check-equal? (eval-exp (isgreater (int 3) (int 4))) (int 0) "simple comparison")
   (check-equal? (eval-exp (isgreater (add (int 1) (int 2)) (int 1))) (int 1) "nested comparison")

   ;; ifnz
   (check-equal? (eval-exp (ifnz (int 0) (int 1) (int 2))) (int 2) "simple ifnz test")
   (check-equal? (eval-exp (ifnz (add (int -1) (int 10))
                                (add (int 1) (int 9))
                                (add (int 11) (int 9))))
                (int 10)
                "nested arguments ifnz test")

   ;; mlet
   (check-equal? (eval-exp (mlet "x" (int 1)
                                 (add (var "x") (int 10))))
                 (int 11)
                 "simple mlet test")
   (check-equal? (eval-exp (mlet "y" (isgreater (int 10000) (int 100))
                                 (add (var "y") (int 10))))
                 (int 11)
                 "nested mlet test")

   
   ;; call
   (check-equal? (eval-exp (call (fun null "x"
                                      (add (var "x") (int 99)))
                                 (int 1)))
                 (int 100)
                 "simple anonymous function call test")

   ;; apair
   (check-equal? (eval-exp (apair (add (int 1) (int 2)) (apair (int 3) (munit))))
                 (racketlist->mupllist (list (int 3) (int 3)))
                 "simple apair test: mlist = ((1 + 2), 3) -> list = (3, 3)")
   

   ;; first
   (check-equal? (eval-exp (first (racketlist->mupllist (list (int 1) (int 2) (int 3))))) 
                 (int 1)
                 "simple first test: mlist = (1, 2, 3)")

   (check-equal? (eval-exp (first (racketlist->mupllist (list (add (int 1) (int 2)) (int 6)))))
                 (int 3)
                 "nested first test: mlist = ((1 + 2), 6)")

   ;; second
   (check-equal? (eval-exp (second (racketlist->mupllist (list (int 1) (int 2)))))
                 (racketlist->mupllist (list (int 2)))
                 "mlist = (1, 2)")

   (check-equal? (eval-exp (first (second (racketlist->mupllist (list (int 1) (int 2)
                                                                      (add (int 1) (int 2))
                                                                      (int 4)
                                                                      (int 5))))))
                 (int 2)
                 "mlist = (1, 2, (1+2), 4, 5)")

   
   ;; ismunit
   (check-equal? (eval-exp (ismunit (munit))) (int 1) "simple ismunit test")
   
   (check-equal? (eval-exp (ismunit (closure '() (fun null "x" (munit)))))
                 (int 0)
                 "simple ismunit test")

   ;; ifmunit
   (check-equal? (eval-exp (ifmunit (munit) (int 1) (int 0))) (int 1) "simple ifmunit test")
   (check-equal? (eval-exp (ifmunit (apair (int 1) (munit))
                                    (add (int 1) (int 10))
                                    (add (int -1) (int 1))))
                 (int 0)
                 "nested ifmunit test")

   ;; mlet*
   (check-equal? (eval-exp (mlet*
                            (list (cons "x" (int 10)))
                            (add (int 10) (var "x"))))
                 (int 20)
                 "mlet* test")

   (check-equal? (eval-exp (mlet*
                            (list (cons "x" (int 10)) (cons "y" (int 1)))
                            (add (var "x") (var "y"))))
                 (int 11)
                 "multi-var mlet* test")


   ;; ifeq
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
                 (int 4)
                 "ifeq test")
   (check-equal? (eval-exp (ifeq (add (int 1) (int 1)) (int 2) (int 3) (int 4)))
                 (int 3)
                 "ifeq test")

   ;; mupl-filter
   (check-equal? (closure? (eval-exp mupl-filter))
                 #t
                 "check if mupl-filter is a curried fun, next test continues on testing curried feature")

   (check-equal? (closure? (eval-exp (call mupl-filter (fun null "x" (int 1)))))
                 #t
                 "check if mupl-filter is a curried fun, if yes, return a closure")

   (check-equal? (eval-exp (call (call mupl-filter (fun null "x" (ifnz (var "x") (int 1) (int 0))))
                                 (racketlist->mupllist (list (int 0) (int 2) (int 0) (int 0) (int 6)))))
                 (racketlist->mupllist (list (int 2) (int 6)))
                 "filter out zero elements in mlist = (0, 2, 0, 0, 6)")


   ;; mupl-all-gt
   (check-equal? (eval-exp (call (call mupl-all-gt (int 3))
                  (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5)))))
                 (racketlist->mupllist (list (int 4) (int 5)))
                 "mupl-all-gt test on mlist = (1, 2, 3, 4, 5) & threshold = 3")

   (check-equal? (eval-exp (call (call mupl-all-gt (int 2))
                  (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5)))))
                 (racketlist->mupllist (list (int 3) (int 4) (int 5)))
                 "mupl-all-gt test on mlist(1, 2, 3, 4, 5) & threshold = 2")

   (check-equal? (eval-exp (call (call mupl-all-gt (int 5))
                  (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5)))))
                 (munit)
                 "mupl-all-gt test on mlist(1, 2, 3, 4, 5) & threshold = 5")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
