;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for heaps
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for heaps.
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

(module (test-heaps)
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
(check-display "*** testing heaps\n")


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

(define (list-sort less? seq)
  (sort seq less?))


(parameterise ((check-test-name	'core))

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


(parameterise ((check-test-name	'insertion))

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


(parameterise ((check-test-name	'deletion))

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
    (test-exn pfds-heap-empty-condition (heap-pop (make-heap <)))
    (test-exn pfds-heap-empty-condition (heap-delete-min (make-heap <)))
    (test-exn pfds-heap-empty-condition (heap-min (make-heap <))))

  #t)


(parameterise ((check-test-name	'deletion))

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

#| end of MODULE |# )

;;; end of file
