#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))


                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))


                              ;; -----------------------

                              ))))


                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else (cons (- num1top (* num1bot num2)) num1bot))


                              ;; -----------------------
                              ))))


                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) 


                              ;; ----------------------- 
                            ))))))))


                
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                        (if (zero? (car num1))
                            (bool-val #t)
                            (bool-val #f))


                          ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      (if-exp (exp1 exp2 conds exps exp3);;;;;;;;;;;;;;;;;;;;;; problem with conds
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of conds env)))
                (cond
                  ((expval->bool val1) (value-of exp2 env))
                  ((expval->bool val2) (value-of exps env))
                  (else (value-of exp3)))))

      (list-exp ()
                (list-val '()))

      (cons-exp (exp1 lst)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of lst env)))
                  (let ((num1 (expval->num val1))
                        (lst1 (expval->list val2)))
                    (list-val
                     (cons num1 lst1)))))

      (sum-exp (lst)
               (let ((val (value-of lst env)))
                 (let ((lst1 (expval->list val)))
                   (num-val
                    (sum-elements lst1 0)))))

      (rational-exp (num1 num2)
                   (if (zero? num2)
                       (report-division-by-zero)
                       (rational-val
                        (cons num1 num2))))

      (simpl-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (cond
                       ((number? num1) (num-val num1))
                       (else
                        (let ((num1top (car num1))
                              (num1bot (cdr num1)))
                          (let ((gcd (get-gcd num1top num1bot 1 2)))
                            (rational-val
                             (cons (/ num1top gcd) (/ num1bot gcd))))))))))

      


      ;; -----------------------

      )))

      ;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS HERE ;;;;;;;;;;;;;;;;;;;;


      ;; usage: iteratively sums all the numbers in a list
      (define (sum-elements lst sum)
        (if (null? lst)
            sum
            (sum-elements (cdr lst) (+ (car lst) sum))))

      
      (define report-division-by-zero
        (lambda ()
          (eopl:error "divison by zero")))

      ;; usage: returns true if num1 is divisible by num2. returns false otherwise
      (define (divisible? num1 num2)
        (if (zero? (remainder num1 num2)) #t #f))

      ;;usage: returns true if both num1 and num2 are divisible by divider
      (define (common-divider? num1 num2 divider)
        (if
         (and
          (divisible? num1 divider)
          (divisible? num2 divider))
         #t
         #f))

      ;;usage returns gcd of num1 and num2
      (define get-gcd
        (lambda (num1 num2 gcd counter)
          (cond
            ((= 1 num1) gcd)
            ((= 1 num2) gcd)
            ((common-divider? num1 num2 counter)
             (get-gcd (/ num1 counter) (/ num2 counter) (* gcd counter) 2))
            ((divisible? num1 counter)
             (get-gcd (/ num1 counter) num2 gcd 2))
            ((divisible? num2 counter)
             (get-gcd num1 (/ num2 counter) gcd 2))
            (else
             (get-gcd num1 num2 gcd (+ counter 1))))))
             