;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for queues
;;;Date: May  1, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for queues.
;;;
;;;Copyright (c) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2011 Ian Price <ianprice90@googlemail.com>
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

(module (test-queues)
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
(check-display "*** testing queues\n")


;;;; helpers

(define-syntax test-eqv
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body (=> eqv?) ?result))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body => ?result))))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))

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

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))


(parameterise ((check-test-name	'core))

  (check
      (queue? (make-queue))
    => #t)

  (check
      (queue? 123)
    => #f)

;;; --------------------------------------------------------------------

  (test-predicate queue? (make-queue))
  (test-predicate queue-empty? (make-queue))
  (test-eqv 0 (queue-length (make-queue)))

  #t)


(parameterise ((check-test-name	'enqueue))

  (let ((queue (enqueue (make-queue) 'foo)))
    (test-predicate queue? queue)
    (test-eqv #t (not (queue-empty? queue)))
    (test-eqv 1 (queue-length queue))
    (test-eqv 10 (queue-length
                  (foldl (lambda (val queue)
                           (enqueue queue val))
                         (make-queue)
                         '(0 1 2 3 4 5 6 7 8 9)))))

  #t)


(parameterise ((check-test-name	'enqueue))

  (let ((empty (make-queue))
        (queue1 (enqueue (make-queue) 'foo))
        (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
    (let-values (((item queue) (dequeue queue1)))
      (test-eqv 'foo item)
      (test-predicate queue? queue)
      (test-predicate queue-empty? queue))
    (let*-values (((first queue*) (dequeue queue2))
                  ((second queue) (dequeue queue*)))
      (test-eqv 'foo first)
      (test-eqv 'bar second)
      (test-eqv 1 (queue-length queue*))
      (test-eqv 0 (queue-length queue)))
    (test-exn pfds-queue-empty-condition (dequeue empty)))

  #t)


(parameterise ((check-test-name	'ordering))

  (let* ((list '(bar quux foo zot baz))
         (queue (list->queue list)))
    (test-eqv 5 (queue-length queue))
    (test-equal list (queue->list queue)))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
