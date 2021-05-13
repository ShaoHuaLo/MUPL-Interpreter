;; CSE413 21sp, Programming Languages & Implementation, Homework 5
;; Name: Shao-Hua Lo
;; UW-Netid: shl0905

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string)      #:transparent) ;; a variable, e.g., (var "foo")
(struct int  (num)         #:transparent) ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)       #:transparent) ;; add two expressions
(struct isgreater (e1 e2)  #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3)    #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body)  #:transparent) ;; a local binding (let var = e in body)
(struct apair   (e1 e2)    #:transparent) ;; make a new pair
(struct first   (e)        #:transparent) ;; get first part of a pair
(struct second  (e)        #:transparent) ;; get second part of a pair
(struct munit   ()         #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)        #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun)  #:transparent)

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (cond [(null? lst) (munit)]
        [#t (apair (car lst) (racketlist->mupllist (cdr lst)))]))




(define (mupllist->racketlist ml)
  (cond [(munit? ml) '()]
        [#t (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]))



;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        
        ;[(closure? e) e]
        [(closure? e) (eval-under-env (closure-fun e) (closure-env e))]
        [(int? e) e]
        [(munit? e) e]
        [(fun? e) (closure env e)]

        ;; isgreater 
        [(isgreater? e)
         (let ([num1 (eval-under-env (isgreater-e1 e) env)]
               [num2 (eval-under-env (isgreater-e2 e)  env)])
           (if (and (int? num1) (int? num2))
               (if (> (int-num num1) (int-num num2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater given a non integer condition")))]

        ;; ifnz
        [(ifnz? e)
         (let ([v (eval-under-env (ifnz-e1 e) env)])
           (if (int? v)
               (if (= 0 (int-num v))
                   (eval-under-env (ifnz-e3 e) env)
                   (eval-under-env (ifnz-e2 e) env))
               (error "Invalid argument, ifnz applied to only int type")))]
        
        ;; fun
        [(fun? e) (closure env e)]
        
        ;; mlet
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
               [env (cons (cons (mlet-var e) v) env)])
         (eval-under-env (mlet-body e) env))]

        ;; call
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)])
           (if (closure? cl)
               (let* ([fn (closure-fun cl)]
                      [v (eval-under-env (call-actual e) env)]
                      [env (cons (cons (fun-formal fn) v) (closure-env cl))])
                 (if (fun-nameopt fn)
                   (let ([env (cons (cons (fun-nameopt fn) cl) env)])
                     (eval-under-env (fun-body fn) env))
                   (eval-under-env (fun-body fn) env)))
               (error "First param for call is not a closure")))]

        ;; apair
        [(apair? e) (apair
                     (eval-under-env (apair-e1 e) env)
                     (eval-under-env (apair-e2 e) env))]

        ;; first
        [(first? e)
         (let ([p (eval-under-env (first-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "argument of a pair is expected")))]
        

        ;; second
        [(second? e)
         (let ([p (eval-under-env (second-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "snd expects to get a pair")))]

        ;;ismunit
        [(ismunit? e)
         (if (munit? (eval-under-env (ismunit-e e) env))
             (int 1)
             (int 0))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

;;; ifmunit
(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))
 
;;; mlet*
(define (mlet* bs e2) 
  (if (null? (cdr bs))
      (mlet (car (car bs)) (cdr (car bs)) e2)
      (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))))


;;; ifeq
(define (ifeq e1 e2 e3 e4)
  (mlet*
   (list (cons "_x" e1) (cons "_y" e2))
   (ifnz (add
          (isgreater (var "_x") (var "_y"))
          (isgreater (var "_y") (var "_x")))
           e4
           e3)))
  
;; Problem 4

(define mupl-filter
   (fun null "predicate" 
       (fun "filter-recur" "mlist"
           (ifmunit (var "mlist")
                    (munit)
                    (mlet "val" (call (var "predicate") (first (var "mlist")))
                     (ifnz (var "val")
                           (apair (first (var "mlist"))
                                  (call (var "filter-recur") (second (var "mlist"))))
                           (call (var "filter-recur") (second (var "mlist")))))))))
                                          
(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "threshold"
             (mlet "predicate" (fun null "number"
                                    (isgreater (var "number") (var "threshold")))
                   (call (var "filter") (var "predicate"))))))


;; Challenge Problem (extra credit)

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
