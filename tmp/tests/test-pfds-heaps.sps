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
  (pfds heaps)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: heaps\n")


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
      (heap? (heap <))
    => #t)

  (check
      (heap? 123)
    => #f)

  (check
      (heap? (heap < 1 2 3))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (heap->list (heap < 1 2 3))
    => '(1 2 3))

  (check
      (heap->list (heap <))
    => '())

;;; --------------------------------------------------------------------

  (test-predicate heap? (make-heap <))
  (test-predicate heap-empty? (make-heap <))
  (test-eqv 0 (heap-size (heap <)))

  #t)


(parametrise ((check-test-name	'insertion))

  (let ((h1 (heap < 7 1 13 9 5 3 11))
        (h2 (heap < 4 2 8 10 6 0 12)))
    (test-equal (+ 1 (heap-size h1))
		(heap-size (heap-insert h1 0)))
    (test-equal (+ 1 (heap-size h1))
		(heap-size (heap-insert h1 1)))
    (test-equal '(1 2 3 5 7 9 11 13)
		(heap->list (heap-insert h1 2)))
    (test-equal '(1 3 4 5 7 9 11 13)
		(heap->list (heap-insert h1 4)))
    (test-equal '(1 3 5 7 9 11 12 13)
		(heap->list (heap-insert h1 12)))
    (test-equal '(1 3 5 7 9 11 13 100)
		(heap->list (heap-insert h1 100)))
    (test-equal '(-2 0 2 4 6 8 10 12)
		(heap->list (heap-insert h2 -2)))
    (test-equal '(0 0 2 4 6 8 10 12)
		(heap->list (heap-insert h2 0)))
    (test-equal '(0 2 4 6 8 8 10 12)
		(heap->list (heap-insert h2 8))))

  #t)


(parametrise ((check-test-name	'deletion))

  (let ((h1 (heap < 7 1 13 9 5 3 11))
        (h2 (heap < 4 2 8 6 0)))
    (test-equal (- (heap-size h1) 1)
		(heap-size (heap-delete-min h1)))
    (test-equal 1 (heap-min h1))
    (test-equal 0 (heap-min h2))
    (test-equal 1 (heap-min (heap-delete-min (heap-insert h1 -10))))
    (test-equal 3 (heap-size (heap-delete-min (heap-delete-min h2))))
    (test-equal 4 (heap-min (heap-delete-min (heap-delete-min h2))))
    (test-equal '(7 9 11 13)
		(heap->list
		 (heap-delete-min (heap-delete-min (heap-delete-min h1)))))
    (test-exn heap-empty-condition? (heap-pop (make-heap <)))
    (test-exn heap-empty-condition? (heap-delete-min (make-heap <)))
    (test-exn heap-empty-condition? (heap-min (make-heap <))))


  #t)


(parametrise ((check-test-name	'deletion))

  (let ((l1 '(129 109 146 175 229 48 225 239 129 41
		  38 13 187 15 207 70 64 198 79 125))
        (l2 '(72 17 220 158 164 133 20 78 96 230 25
		 19 13 17 58 223 37 214 94 195 93 174)))
    (test-equal '() (heap-sort < '()))
    (test-equal (list-sort < l1)
		(heap-sort < l1))
    (test-equal (list-sort < l2)
		(heap-sort < l2)))

  #t)


;;;; done

(check-report)

;;; end of file
