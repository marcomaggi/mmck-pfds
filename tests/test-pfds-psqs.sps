;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: PFDS
;;;Contents: generic library tests
;;;Date: Tue Aug 13, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>
;;;Edited by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Author: Ian Price <ianprice90@googlemail.com>
;;;
;;;This program is free software,  you can redistribute it and/or modify
;;;it under the terms of the new-style BSD license.
;;;
;;;You should  have received a copy  of the BSD license  along with this
;;;program.  If not, see <http://www.debian.org/misc/bsd.license>.
;;;


#!r6rs
(import (vicare)
  (pfds psqs)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: priority search queues\n")


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
    ((_ ?predicate ?body)
     (check
	 (guard (E ((?predicate E)
		    #t)
		   (else #f))
	   ?body)
       => #t))))

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


(parametrise ((check-test-name	'core))

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


(parametrise ((check-test-name	'set))

  (let* ((empty (make-psq char<? <))
         (psq1  (psq-set empty #\a 10))
         (psq2  (psq-set psq1 #\b 33))
         (psq3  (psq-set psq2 #\c 3))
         (psq4  (psq-set psq3 #\a 12)))
    (test-eqv 10 (psq-ref psq1 #\a))
    (test-exn assertion-violation? (psq-ref psq1 #\b))
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


(parametrise ((check-test-name	'delete))

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


(parametrise ((check-test-name	'update))

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


(parametrise ((check-test-name	'operations))

  (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3) (#\d . 23) (#\e . 7))
                           char<?
                           <))
         (psq2 (psq-delete-min psq1))
         (psq3 (psq-delete-min (psq-set psq2 #\b 9)))
         (psq4 (make-psq < <)))
    (test-eqv #\c (psq-min psq1))
    (test-eqv #\e (psq-min psq2))
    (test-exn assertion-violation? (psq-delete-min psq4))
    (test-eqv #\a (psq-min (psq-set psq1 #\a 0)))
    (call-with-values
	(lambda ()
	  (psq-pop psq3))
      (lambda (min rest)
	(test-eqv #\b min)
	(test-eqv #\a (psq-min rest)))))

  #t)


(parametrise ((check-test-name	'operations))

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
    (test-equal (filter (lambda (x) (char<=? #\e (car x) #\u)) alist-sorted)
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

;;; end of file
