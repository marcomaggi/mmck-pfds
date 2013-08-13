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
(import (except (vicare)
		make-queue)
  (pfds queues)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: queues\n")


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


(parametrise ((check-test-name	'enqueue))

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


(parametrise ((check-test-name	'enqueue))

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
    (test-eqv #t
              (guard (exn ((queue-empty-condition? exn) #t)
                          (else #f))
                (dequeue empty)
                #f)))

  #t)


(parametrise ((check-test-name	'ordering))

  (let* ((list '(bar quux foo zot baz))
         (queue (list->queue list)))
    (test-eqv 5 (queue-length queue))
    (test-equal list (queue->list queue)))

  #t)


;;;; done

(check-report)

;;; end of file
