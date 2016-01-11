;;;; ==================================================
;;;; RUnit (or; RunIt):
;;;;     A set of simple unit testing macros for Racket
;;;;
;;;;     Version 1.0.0
;;;;     Aaron L. Zeng <a2z@mit.edu>
;;;; ==================================================
;;;;
;;;; Each test case consists of a form that should evaluate to a truthy value.
;;;; If any of the test cases evaluates falsey, then the value of [test-begin]
;;;; is #f.  Otherwise [test-begin] returns #t.
;;;;
;;;; Examples:
;;;;
;;;;   (test-begin
;;;;     (= 4 (+ 2 2))
;;;;     (= 5 (+ 2 3)))                  ===> #t
;;;;
;;;;   (test-begin
;;;;     (= 4 (+ 2 3))
;;;;     (= 5 (+ 2 3)))                  ===> #f
;;;;
;;;;   [test-begin] also prints out each form which produced a falsey value.

(module runit racket

(provide test-begin
         test-progn
         float-=)

;;; ANSI Escape Codes for colors
(define ansi-bright-red "\x1b[31;1m")
(define ansi-green      "\x1b[32m")
(define ansi-reset      "\x1b[0m")

;;; Evaluate the test form.  If it returns truthy, return `(#t ,form)
;;; where [form] is the original test form. Otherwise return `(#f ,form).
(define-syntax-rule (test-form body)
  (if body
      (list #t 'body)
      (list #f 'body)))

;;; Do the same thing as [and], but don't short circuit.
(define (and-strict . bools)
  (cond
    ((null? bools) #t)                  ;identity under [and]
    ((not (car bools)) #f)
    (else (apply and-strict (cdr bools)))))

;;; Print out the result of a test, given as a boolean.
;;; Failed tests are red; passed tests are green.
(define (print-result-colorful bool)
  (if bool
      (printf "~a~a~a~%~%" ansi-green "PASSED" ansi-reset)
      (printf "~a~a~a~%~%" ansi-bright-red "FAILED" ansi-reset)))

;;; A paperthin wrapper around test-progn for ANSI colors.
(define-syntax-rule (test-begin forms ...)
  (print-result-colorful
   (test-progn forms ...)))

;;; Evaluate multiple forms and report if any of them fail.
;;; Prints out all of the forms that fail.
(define-syntax test-progn
  (syntax-rules ()
    ((test-progn test-name)
     (begin
       (printf "Done testing ~a.~%" test-name)
       (if (boolean? test-name)               ;user probably forgot to put name
           (printf "~aWARN~a: test name (~a) is a boolean.  First form may not have been tested correctly." ansi-bright-red ansi-reset test-name)
           #f)
       #t))
    ((test-progn test-name expr1 expr2 ...)
     (let ((result (test-form expr1)))
       (and-strict
        (if (car result)
            #t
            (begin
              (printf "~aFAILED~a ~a: expected~%\t~a~%~%"
                      ansi-bright-red
                      ansi-reset
                      test-name
                      (cadr result))
              #f))
        (test-progn test-name expr2 ...))))))

;;; Eh, good enough.
(define epsilon 1e-4)

;;; #t iff [x] and [y] are within [epsilon] of each other.
(define (float-= x y)
  (< (abs (- x y))
     epsilon))

)
