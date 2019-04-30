;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for deques
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for deques.
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

(module (test-deques)
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
(check-display "*** testing deques\n")


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

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
    (foldl kons (kons (car list) knil) (cdr list))))


(parameterise ((check-test-name	'conditions))

  (check-for-true	(deque-empty-condition? (condition '(pfds-deque-empty-condition))))
  (check-for-false	(deque-empty-condition? (condition '(exn location here))))

  #t)


(parameterise ((check-test-name	'core))

  (test-predicate deque? (make-deque))
  (test-predicate deque-empty? (make-deque))
  (test-eqv 0 (deque-length (make-deque)))

  #t)


(parameterise ((check-test-name	'insert))

  (let ((deq (enqueue-front (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))

  (let ((deq (enqueue-rear (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))

  (test-eqv 5 (deque-length
               (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4)))))

  #t)


(parameterise ((check-test-name	'remove))

  (let ((deq (enqueue-front (make-deque) 'foo)))
    (let-values (((item0 deque0) (dequeue-front deq))
		 ((item1 deque1) (dequeue-rear deq)))
      (test-eqv 'foo item0)
      (test-eqv 'foo item1)
      (test-predicate deque-empty? deque0)
      (test-predicate deque-empty? deque1)))

  (let ((deq (foldl (lambda (item deque)
		      (enqueue-rear deque item))
		    (make-deque)
		    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-front deq))
		  ((item1 deque1) (dequeue-front deque0))
		  ((item2 deque2) (dequeue-front deque1)))
      (test-eqv 0 item0)
      (test-eqv 1 item1)
      (test-eqv 2 item2)
      (test-eqv 3 (deque-length deque2))))

  (let ((deq (foldl (lambda (item deque)
		      (enqueue-rear deque item))
		    (make-deque)
		    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
		  ((item1 deque1) (dequeue-rear deque0))
		  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 5 item0)
      (test-eqv 4 item1)
      (test-eqv 3 item2)
      (test-eqv 3 (deque-length deque2))))

  (test-eqv #t
	    (condition-case (let ((empty (make-deque)))
			      (dequeue-front empty)
			      #f)
	      ((pfds-deque-empty-condition)	#t)
	      (()				#f)))
  (test-eqv #t
	    (condition-case (let ((empty (make-deque)))
			      (dequeue-rear empty)
			      #f)
	      ((pfds-deque-empty-condition)	#t)
	      (()				#f)))

  (values))


(parameterise ((check-test-name	'mixed))

  (let ((deque (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4)))))
    (let*-values (((item0 deque) (dequeue-front deque))
                  ((item1 deque) (dequeue-front deque))
                  ((item2 deque) (dequeue-front deque))
                  ((item3 deque) (dequeue-front deque))
                  ((item4 deque) (dequeue-front deque)))
      (test-eqv 4 item0)
      (test-eqv 2 item1)
      (test-eqv 0 item2)
      (test-eqv 1 item3)
      (test-eqv 3 item4)))

  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
                  ((item1 deque1) (dequeue-front deque0))
                  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 2 item0)
      (test-eqv 0 item1)
      (test-eqv 1 item2)
      (test-predicate deque-empty? deque2)))

  #t)


(parameterise ((check-test-name	'list))

  (let ((id-list (lambda (list)
		   (deque->list (list->deque list))))
	(l1 '())
	(l2 '(1 2 3))
	(l3 '(4 5 6 7 8 9 10))
	(l4 (string->list "abcdefghijklmnopqrstuvwxyz")))
    (test-equal l1 (id-list l1))
    (test-equal l2 (id-list l2))
    (test-equal l3 (id-list l3))
    (test-equal l4 (id-list l4)))

  #t)


;;;; done

(check-report)

#| end of module |# )

;;; end of file
