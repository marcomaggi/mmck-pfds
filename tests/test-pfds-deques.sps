;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: PFDS
;;;Contents: generic library tests
;;;Date: Sat Aug 10, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>
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
  (pfds deques)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: deques\n")


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

  (test-predicate deque? (make-deque))
  (test-predicate deque-empty? (make-deque))
  (test-eqv 0 (deque-length (make-deque)))

  #t)


(parametrise ((check-test-name	'insert))

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


(parametrise ((check-test-name	'remove))

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

  (let ((empty (make-deque)))
    (test-eqv #t
	      (guard (exn ((deque-empty-condition? exn) #t)
			  (else #f))
		(dequeue-front empty)
		#f))
    (test-eqv #t
	      (guard (exn ((deque-empty-condition? exn) #t)
			  (else #f))
		(dequeue-rear empty)
		#f)))

  #t)


(parametrise ((check-test-name	'mixed))

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


(parametrise ((check-test-name	'list))

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

;;; end of file