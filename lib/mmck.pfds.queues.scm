;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: module deques
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the module deques: purely functional deques.
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


;;;; commentary
;;
;; A scheme translation of "Simple and Efficient Purely Functional
;; Queues and Deques" by Chris Okazaki
;;


;;;; documentation
;;
;; make-queue : () -> queue
;; returns a queue containing no items
;;
;; queue? : any -> boolean
;; tests if an object is a queue
;;
;; queue-length : queue -> non-negative integer
;; returns the number of items in the queue
;;
;; queue-empty? : queue -> boolean
;; returns true if there are no items in the queue, false otherwise
;;
;; enqueue : queue any -> queue
;; returns a new queue with the enqueued item at the end
;;
;; dequeue : queue -> value queue
;; returns two values, the item at the front of the queue, and a new
;; queue containing the all the other items
;; raises a &queue-empty condition if the queue is empty
;;
;; queue-empty-condition? : object -> boolean
;; tests if an object is a &queue-empty condition
;;
;; queue->list : queue -> listof(any)
;; returns a queue containing all the items in the list. The order of
;; the elements in the queue is the same as the order of the elements
;; in the list.
;;
;; list->queue : listof(any) -> queue
;; returns a list containing all the items in the queue. The order of
;; the items in the list is the same as the order in the queue.
;; For any list l, (equal? (queue->list (list->queue l)) l) is #t.
;;


(declare (unit mmck.pfds.queues)
	 (uses mmck.pfds.private.helpers)
	 (uses mmck.pfds.private.coops)
	 (uses mmck.pfds.private.lazy-lists)
	 (emit-import-library mmck.pfds.queues))

(module (mmck.pfds.queues)
    (make-queue
     queue?
     queue-length
     queue-empty?
     enqueue
     dequeue
     queue-empty-condition?
     list->queue
     queue->list)
  (import (scheme)
	  (mmck pfds private helpers)
	  (except (mmck pfds private coops)
		  <queue>)
	  (mmck pfds private lazy-lists))


;;;; helpers

(define (rotate l r a)
  (if (empty? l)
      (cons* (head r) a)
      (cons* (head l)
             (rotate (tail l)
                     (tail r)
                     (cons* (head r) a)))))


;;;; implementation

(define-class <queue>
    (<standard-object>)
  ((length	#:reader queue-length)
   (l		#:reader queue-l)
   (r		#:reader queue-r)
   (l^		#:reader queue-l^)))

(define (%make-queue len l r l^)
  (make <queue>
    'length len 'l l 'r r 'l^ l^))

(define (queue? obj)
  (is-a? obj <queue>))

(define (make-queue)
  (%make-queue 0 '() '() '()))

(define (enqueue queue item)
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (makeq (+ len 1) l (cons* item r) l^)))

(define (dequeue queue)
  (assert-queue-not-empty 'dequeue queue)
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (values (head l)
            (makeq (- len 1) (tail l) r l^))))

(define (makeq length l r l^)
  (if (empty? l^)
      (let ((l* (rotate l r '())))
        (%make-queue length l* '() l*))
      (%make-queue length l r (tail l^))))

(define (queue-empty? queue)
  (zero? (queue-length queue)))

(define (list->queue list)
  (fold-left enqueue (make-queue) list))

(define (queue->list queue)
  (let loop ((rev-list '()) (queue queue))
    (if (queue-empty? queue)
        (reverse rev-list)
        (let-values (((val queue) (dequeue queue)))
          (loop (cons val rev-list)
                 queue)))))


;;;; exceptional conditions

(define queue-empty-condition?
  (condition-predicate 'pfds-queue-empty-condition))

(define (assert-queue-not-empty who queue)
  (when (queue-empty? queue)
    (raise
     (condition
       `(exn location ,who
	     message "empty queue, there are no elements to remove"
	     arguments ,(list queue))
       '(pfds-queue-empty-condition)))))


;;;; done

#| end of module |# )

;;; end of file

