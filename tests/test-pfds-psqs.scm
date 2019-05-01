;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for psqs
;;;Date: May  1, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for psqs.
;;;
;;;Copyright (c) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(module (test-psqs)
    ()
  (import (scheme)
	  (only (chicken base)
		let-values
		let*-values
		add1)
	  (only (chicken sort)
		sort)
	  (only (chicken condition)
		condition-case
		handle-exceptions
		print-error-message
		condition
		condition->list)
	  (rename (mmck pfds)
		  (make-fingertree	%make-fingertree)
		  (list->fingertree	%list->fingertree))
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing psqs\n")


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
	   (() #f))
       => #t))
    ))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))

(define-syntax test-not
  (syntax-rules ()
    ((_ ?body)
     (check ?body => #f))))

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

(define (alist->psq alist key<? priority<?)
  (foldl (lambda (kv psq)
           (psq-set psq (car kv) (cdr kv)))
         (make-psq key<? priority<?)
         alist))

(define (list-sort less? seq)
  (sort seq less?))

(define (filter pred ell)
  (let loop ((nil '())
	     (ell (reverse ell)))
    (if (pair? ell)
	(loop (if (pred (car ell))
		  (cons (car ell) nil)
		nil)
	      (cdr ell))
      nil)))


(parameterise ((check-test-name	'core))

  (check
      (psq? (make-psq < <))
    => #t)

  (check
      (psq? 123)
    => #f)

;;; --------------------------------------------------------------------

  (test-predicate psq? (make-psq string<? <))
  (test-predicate psq-empty? (make-psq string<? <))
  (test-predicate zero? (psq-size (make-psq string<? <)))

  #t)


(parameterise ((check-test-name	'set))

  (let* ((empty (make-psq char<? <))
         (psq1  (psq-set empty #\a 10))
         (psq2  (psq-set psq1 #\b 33))
         (psq3  (psq-set psq2 #\c 3))
         (psq4  (psq-set psq3 #\a 12)))
    (test-eqv 10 (psq-ref psq1 #\a))
    (test-exn pfds-assertion-violation (psq-ref psq1 #\b))
    (test-eqv 1 (psq-size psq1))

    (test-eqv 10 (psq-ref psq2 #\a))
    (test-eqv 33 (psq-ref psq2 #\b))
    (test-not (psq-contains? psq2 #\c))
    (test-eqv 2 (psq-size psq2))

    (test-eqv 10 (psq-ref psq3 #\a))
    (test-eqv 33 (psq-ref psq3 #\b))
    (test-eqv 3  (psq-ref psq3 #\c))
    (test-eqv 3 (psq-size psq3))

    (test-eqv 12 (psq-ref psq4 #\a))
    (test-eqv 33 (psq-ref psq4 #\b))
    (test-eqv 3  (psq-ref psq4 #\c))
    (test-eqv 3 (psq-size psq4)))

  #t)


(parameterise ((check-test-name	'delete))

  (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3))
                           char<?
                           <))
         (psq2 (psq-delete psq1 #\c))
         (psq3 (psq-delete psq2 #\b))
         (psq4 (psq-delete psq3 #\a))
         (psq5 (psq-delete psq1 #\d)))
    (test-eqv #t (psq-contains? psq1 #\c))
    (test-not (psq-contains? psq2 #\c))
    (test-eqv #t (psq-contains? psq2 #\b))
    (test-not (psq-contains? psq3 #\b))
    (test-eqv #t (psq-contains? psq3 #\a))
    (test-predicate psq-empty? psq4)
    (test-eqv (psq-size psq1)
	      (psq-size psq5)))

  #t)


(parameterise ((check-test-name	'update))

  (let* ((empty (make-psq char<? <))
         (psq1  (psq-update empty #\a add1 10))
         (psq2  (psq-update psq1 #\b add1 33))
         (psq3  (psq-update psq2 #\c add1 3))
         (psq4  (psq-update psq3 #\a add1 0))
         (psq5  (psq-update psq3 #\c add1 0)))
    (test-eqv 11 (psq-ref psq3 #\a))
    (test-eqv 34 (psq-ref psq3 #\b))
    (test-eqv 4  (psq-ref psq3 #\c))

    (test-eqv 12 (psq-ref psq4 #\a))
    (test-eqv 34 (psq-ref psq4 #\b))
    (test-eqv 4  (psq-ref psq4 #\c))
    (test-eqv 3  (psq-size psq4))

    (test-eqv 11 (psq-ref psq5 #\a))
    (test-eqv 34 (psq-ref psq5 #\b))
    (test-eqv 5  (psq-ref psq5 #\c))
    (test-eqv 3  (psq-size psq5)))

  #t)


(parameterise ((check-test-name	'operations))

  (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3) (#\d . 23) (#\e . 7))
                           char<?
                           <))
         (psq2 (psq-delete-min psq1))
         (psq3 (psq-delete-min (psq-set psq2 #\b 9)))
         (psq4 (make-psq < <)))
    (test-eqv #\c (psq-min psq1))
    (test-eqv #\e (psq-min psq2))
    (test-exn pfds-assertion-violation (psq-delete-min psq4))
    (test-eqv #\a (psq-min (psq-set psq1 #\a 0)))
    (call-with-values
	(lambda ()
	  (psq-pop psq3))
      (lambda (min rest)
	(test-eqv #\b min)
	(test-eqv #\a (psq-min rest)))))

  #t)


(parameterise ((check-test-name	'operations))

  (let* ((alist '((#\f . 24) (#\u . 42) (#\p . 16) (#\s . 34) (#\e . 17)
		  (#\x . 45) (#\l . 14) (#\z . 5) (#\t . 45) (#\r . 41)
		  (#\k . 32) (#\w . 14) (#\d . 12) (#\c . 16) (#\m . 20) (#\j . 25)))
	 (alist-sorted (list-sort (lambda (x y)
				    (char<? (car x) (car y)))
				  alist))
	 (psq  (alist->psq alist char<? <)))
    (test-equal alist-sorted
		(psq-at-most psq +inf.0))
    (test-equal '() (psq-at-most psq 0))
    (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
		  (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
		(psq-at-most psq 20))
    (test-equal alist-sorted
		(psq-at-most-range psq +inf.0 #\x00 #\xFF))
    ;; with bounds outwith range in psq, is the same as psq-at-most
    (test-equal '() (psq-at-most-range psq 0 #\x00 #\xFF))
    (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
		  (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
		(psq-at-most-range psq 20 #\x00 #\xFF))
    (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
		  (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
		(psq-at-most psq 20))
    (test-equal (filter (lambda (x)
			  (and (char<=? #\e (car x))
			       (char<=? (car x) #\u)))
		  alist-sorted)
		(psq-at-most-range psq +inf.0 #\e #\u))
    (test-equal '() (psq-at-most-range psq 0 #\e #\u))
    (test-equal '((#\e . 17) (#\l . 14) (#\m . 20) (#\p . 16))
		(psq-at-most-range psq 20 #\e #\u))
    ;; inclusiveness check
    (test-equal '((#\t . 45))
		(psq-at-most-range psq 80 #\t #\t))
    ;; if lower bound is higher than upper, then nothing
    (test-equal '() (psq-at-most-range psq 80 #\t #\r)))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
