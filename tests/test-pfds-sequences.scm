;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for sequence
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for sequence.
;;;
;;;	Note: at  the moment, sequences  are a trivial instantiation  of fingertrees,
;;;	and so are pretty much covered by the fingertrees tests.
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

(module (test-sequence)
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
	  (mmck pfds)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing sequence\n")


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


(parameterise ((check-test-name	'core))

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
    (test-exn pfds-assertion-violation (sequence-ref s -1))
    (test-exn pfds-assertion-violation (sequence-ref s 3)))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
