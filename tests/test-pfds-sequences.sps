;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: PFDS
;;;Contents: generic library tests
;;;Date: Tue Aug 13, 2013
;;;
;;;Abstract
;;;
;;;	Note: at  the moment, sequences  are a trivial  instantiation of
;;;	fingertrees, and so  are pretty much covered  by the fingertrees
;;;	tests.
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
  (pfds sequences)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: sequences\n")


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

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))


(parametrise ((check-test-name	'core))

  (check
      (sequence? (sequence))
    => #t)

  (check
      (sequence? 123)
    => #f)

  (check
      (sequence? (sequence 1 2 3))
    => #t)

;;; --------------------------------------------------------------------

  (let ((s (sequence 'zero 'one 'two)))
    (test-eqv 'zero (sequence-ref s 0))
    (test-eqv 'two (sequence-ref s 2))
    (test-exn assertion-violation? (sequence-ref s -1))
    (test-exn assertion-violation? (sequence-ref s 3)))

  #t)


;;;; done

(check-report)

;;; end of file
