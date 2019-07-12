;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for sets
;;;Date: May  1, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for sets.
;;;
;;;Copyright (c) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;Copyright (c) 2011, 2012 Ian Price <ianprice90@googlemail.com>
;;;All rights reserved.
;;;
;;;Redistribution and use  in source and binary forms, with  or without modification,
;;;are permitted provided that the following conditions are met:
;;;
;;;1.  Redistributions  of source code must  retain the above copyright  notice, this
;;;   list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary form must  reproduce the above copyright notice, this
;;;   list of  conditions and  the following disclaimer  in the  documentation and/or
;;;   other materials provided with the distribution.
;;;
;;;3. The name of  the author may not be used to endorse  or promote products derived
;;;   from this software without specific prior written permission.
;;;
;;;THIS SOFTWARE  IS PROVIDED  BY THE  AUTHOR ``AS  IS'' AND  ANY EXPRESS  OR IMPLIED
;;;WARRANTIES,   INCLUDING,  BUT   NOT  LIMITED   TO,  THE   IMPLIED  WARRANTIES   OF
;;;MERCHANTABILITY AND FITNESS FOR A PARTICULAR  PURPOSE ARE DISCLAIMED.  IN NO EVENT
;;;SHALL  THE  AUTHOR  BE  LIABLE  FOR ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,
;;;EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE,  DATA,  OR  PROFITS;  OR  BUSINESS
;;;INTERRUPTION) HOWEVER CAUSED AND ON ANY  THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;;STRICT LIABILITY, OR  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY
;;;OUT  OF THE  USE OF  THIS SOFTWARE,  EVEN IF  ADVISED OF  THE POSSIBILITY  OF SUCH
;;;DAMAGE.
;;;


;;;; units and module header

(require-library (mmck pfds)
		 (mmck checks))

(module (test-sets)
    ()
  (import (scheme)
	  (only (chicken base)
		let-values
		let*-values
		current-error-port)
	  (only (chicken condition)
		condition-case
		handle-exceptions
		print-error-message
		condition)
	  (only (chicken sort)
		sort)
	  (mmck pfds)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing sets\n")


;;;; helpers

(define-syntax test-eqv
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body (=> eqv?) ?result))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body => ?result))))

(define-syntax test-exn
  (syntax-rules ()
    ((_ ?condition-kind ?body)
     (check
	 (condition-case
	     ?body
	   ((?condition-kind)
	    #t)
	   (()
	    #f))
       => #t))
    ))

(define-syntax test-no-exn
  (syntax-rules ()
    ((_ ?body)
     (check
	 (condition-case
	     (begin
	       ?body
	       #t)
	   (()
	    #f))
       => #t))
    ))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))

(define-syntax test
  (syntax-rules ()
    ((test body)
     (test-eqv #t (and body #t)))))

(define-syntax test-not
  (syntax-rules ()
    ((test-not body)
     (test-eqv #f body))))

;;; --------------------------------------------------------------------

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))

(define (for-all pred ell)
  (if (pair? ell)
      (if (pred (car ell))
	  (for-all pred (cdr ell))
	#f)
    #t))

(define (exists pred ell)
  (and (pair? ell)
       (or (pred (car ell))
	   (exists pred (cdr ell)))))

(define (list-sort less? seq)
  (sort seq less?))


(parameterise ((check-test-name	'core))

  (check
      (set? (make-set <))
    => #t)

  (check
      (set? 123)
    => #f)

;;; --------------------------------------------------------------------

  (let ((empty (make-set string<?))
        (set (fold-left set-insert
	       (make-set string<?)
	       (list "foo" "bar" "baz" "quux" "zot"))))
    (test-predicate set? empty)
    (test-predicate set? set)
    (test-eqv 0 (set-size empty))
    (test-eqv 5 (set-size set))
    (test-eqv #f (set-member? empty "foo"))
    (test-eqv #t (set-member? (set-insert empty "foo") "foo"))
    (test-eqv #t (set-member? set "foo"))
    (test-eqv #f (set-member? (set-remove set "foo") "foo"))
    (test-no-exn (set-remove empty "anything"))
    (test-no-exn (set-insert set "quux"))
    (test-eqv (set-size (set-insert empty "foo"))
	      (set-size (set-insert (set-insert empty "foo") "foo")))
    (test-eqv (set-size (set-remove set "foo"))
	      (set-size (set-remove (set-remove set "foo") "foo"))))

  #t)


(parameterise ((check-test-name	'equality))

  (let* ((empty (make-set string<?))
         (set1  (list->set '("foo" "bar" "baz") string<?))
         (set2  (list->set '("foo" "bar" "baz" "quux" "zot") string<?))
         (sets  (list empty set1 set2)))
    (test (for-all (lambda (x) (set=? x x)) sets))
    (test (for-all (lambda (x) (subset? x x)) sets))
    (test-not (exists (lambda (x) (proper-subset? x x)) sets))
    (test (set<? empty set1))
    (test (set<? set1 set2))
    (test (set=? (set-insert set1 "quux")
		 (set-remove set2 "zot"))))

  #t)


(parameterise ((check-test-name	'operations))

  (let* ((empty (make-set <))
         (set1 (list->set '(0 2 5 7 12 2 3 62 5) <))
         (set2 (list->set '(94 33 44 2 73 55 48 92 98 29
			       28 98 55 20 69 5 33 53 89 50)
                          <))
         (sets (list empty set1 set2)))
    (test (for-all (lambda (x) (set=? x (set-union x x))) sets))
    (test (for-all (lambda (x) (set=? x (set-intersection x x))) sets))
    (test (for-all (lambda (x) (set=? empty (set-difference x x))) sets))
    (test (for-all (lambda (x) (set=? x (set-union empty x))) sets))
    (test (for-all (lambda (x) (set=? empty (set-intersection empty x))) sets))
    (test (for-all (lambda (x) (set=? x (set-difference x empty))) sets))
    (test (for-all (lambda (x) (set=? empty (set-difference empty x))) sets))

    (test (set=? (set-union set1 set2) (set-union set2 set1)))
    (test (set=? (set-union set1 set2)
		 (list->set '(0 2 3 69 7 73 12 20 89 28
                                29 94 5 33 98 92 44 48 50 53
                                55 62)
			    <)))

    (test (set=? (set-intersection set1 set2) (set-intersection set2 set1)))
    (test (set=? (set-intersection set1 set2)
		 (list->set '(2 5) <)))
    (test (set=? (set-difference set1 set2)
		 (list->set '(0 3 12 62 7) <)))
    (test (set=? (set-difference set2 set1)
		 (list->set '(33 98 69 73 44 48 92 50 20 53
				 55 89 28 29 94)
			    <))))

  #t)


(parameterise ((check-test-name	'conversion))

  (test-eqv '() (set->list (make-set <)))
  (test-eqv 0 (set-size (list->set '() <)))
  (test-equal (string->list "abcdefghijklmno")
              (list-sort char<?
                         (set->list
                          (list->set (string->list "abcdefghijklmno") char<?))))
  (test-equal '(0) (set->list (fold-left set-insert (make-set <) '(0 0 0 0))))

  #t)


(parameterise ((check-test-name	'fold))

  (test-eqv 0 (set-fold + 0 (list->set '() <)))
  (test-eqv 84 (set-fold + 0 (list->set '(3 12 62 7) <)))
  (test-eqv 499968 (set-fold * 1 (list->set '(3 12 62 7 8 4) <)))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
