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
;;;Copyright (c) 2012 Ian Price <ianprice90@googlemail.com>
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


;;;; documentation:
;;
;; make-deque : () -> deque
;; returns a deque containing to items
;;
;; deque? : any -> boolean
;; tests if an object is a deque
;;
;; deque-length : deque -> non-negative integer
;; returns the number of items in the deque
;;
;; deque-empty? : deque -> boolean
;; returns true if there are no items in the deque, false otherwise
;;
;; enqueue-front : deque any -> deque
;; returns a new deque with the inserted item at the front
;;
;; enqueue-rear : deque any -> deque
;; returns a new deque with the inserted item at the rear
;;
;; dequeue-front : deque -> any queue
;; returns two values, the item at the front of the deque, and a new
;; deque containing all the other items
;; raises a &deque-empty condition if the deque is empty
;;
;; dequeue-rear : deque -> any queue
;; returns two values, the item at the rear of the deque, and a new
;; deque containing all the other items
;; raises a &deque-empty condition if the deque is empty
;;
;; deque-empty-condition? : object -> boolean
;; tests if an object is a &deque-empty condition
;;
;; deque->list : deque -> listof(any)
;; returns a list containing all the elements of the deque. The order
;; of the elements in the list is the same as the order they would be
;; dequeued from the front of the deque.
;;
;; list->deque : listof(any) -> deque
;; returns a deque containing all of the elements in the list. The
;; order of the elements in the deque is the same as the order of the
;; elements in the list.
;;


(declare (unit mmck.pfds.deques)
	 (uses mmck.pfds.private.helpers)
	 (uses mmck.pfds.private.coops)
	 (uses mmck.pfds.private.lazy-lists)
	 (emit-import-library mmck.pfds.deques))

(module (mmck.pfds.deques)
    (make-deque
     deque?
     deque-length
     deque-empty?
     enqueue-front
     enqueue-rear
     dequeue-front
     dequeue-rear
     deque-empty-condition?
     deque->list
     list->deque)
  (import (scheme)
	  (mmck pfds private helpers)
	  (mmck pfds private coops)
	  (mmck pfds private lazy-lists))


;;;; implementation

(define c 2)

(define (rot1 n l r)
  (if (>= n c)
      (cons* (head l)
             (rot1 (- n c) (tail l) (drop c r)))
      (rot2 l (drop n r) '())))

(define (rot2 l r a)
  (if (empty? l)
      (append* (rev r) a)
      (cons* (head l)
             (rot2 (tail l)
                   (drop c r)
                   (append* (rev (take c r)) a)))))

(define-class deque
    (<standard-class>)
  ((length	#:reader deque-length)
   (lenL	#:reader deque-lenL)
   (lenR	#:reader deque-lenR)
   (l		#:reader deque-l)
   (r		#:reader deque-r)
   (l^		#:reader deque-l^)
   (r^		#:reader deque-r^)))

(define (%make-deque len lenL lenR l r l^ r^)
  (make deque
    'length len 'lenL lenL 'lenR lenR 'l l 'r r 'l^ l^ 'r^ r^))

(define (deque? obj)
  (is-a? obj deque))

(define (make-deque)
  (%make-deque 0 0 0 '() '() '() '()))

(define (deque-empty? deque)
  (zero? (deque-length deque)))

(define (enqueue-front deque item)
  (let ((len (deque-length deque))
        (l (deque-l deque))
        (r (deque-r deque))
        (lenL (deque-lenL deque))
        (lenR (deque-lenR deque))
        (l^ (deque-l^ deque))
        (r^ (deque-r^ deque)))
    (makedq (+ 1 len) (+ 1 lenL) lenR (cons* item l) r (tail l^) (tail r^))))

(define (enqueue-rear deque item)
  (let ((len (deque-length deque))
        (l (deque-l deque))
        (r (deque-r deque))
        (lenL (deque-lenL deque))
        (lenR (deque-lenR deque))
        (l^ (deque-l^ deque))
        (r^ (deque-r^ deque)))
    (makedq (+ 1 len) lenL (+ 1 lenR) l (cons* item r) (tail l^) (tail r^))))

(define (dequeue-front deque)
  (assert-deque-not-empty 'deque-front deque)
  (let ((len (deque-length deque))
        (lenL (deque-lenL deque))
        (lenR (deque-lenR deque))
        (l (deque-l deque))
        (r (deque-r deque))
        (l^ (deque-l^ deque))
        (r^ (deque-r^ deque)))
    (if (empty? l)
        (values (head r) (make-deque))
      (values (head l)
              (makedq (- len 1)
                      (- lenL 1)
                      lenR
                      (tail l)
                      r
                      (tail (tail l^))
                      (tail (tail r^)))))))

(define (dequeue-rear deque)
  (assert-deque-not-empty 'deque-front deque)
  (let ((len (deque-length deque))
        (lenL (deque-lenL deque))
        (lenR (deque-lenR deque))
        (l (deque-l deque))
        (r (deque-r deque))
        (l^ (deque-l^ deque))
        (r^ (deque-r^ deque)))
    (if (empty? r)
        (values (head l) (make-deque))
        (values (head r)
                (makedq (- len 1)
                        lenL
                        (- lenR 1)
                        l
                        (tail r)
                        (tail (tail l^))
                        (tail (tail r^)))))))



(define (makedq len lenL lenR l r l^ r^)
  (cond ((> lenL (+ 1 (* c lenR)))
         (let* ((n  (floor (/ (+ lenL lenR) 2)))
                (l* (take n l))
                (r* (rot1 n r l)))
           (%make-deque len n (- len n) l* r* l* r*)))
        ((> lenR (+ 1 (* c lenL)))
         (let* ((n  (floor (/ (+ lenL lenR) 2)))
                (l* (rot1 n l r))
                (r* (take n r)))
           (%make-deque len (- len n) n l* r* l* r*)))
        (else
         (%make-deque len lenL lenR l r l^ r^))))

(define (list->deque l)
  (fold-left enqueue-rear (make-deque) l))

(define (deque->list deq)
  (define (recur deq l)
    (if (deque-empty? deq)
        l
        (let-values ([(last deq*) (dequeue-rear deq)])
          (recur deq* (cons last l)))))
  (recur deq '()))


;;;; exceptional conditions

(define deque-empty-condition?
  (condition-predicate 'pfds-deque-empty-condition))

(define (assert-deque-not-empty who deque)
  (when (deque-empty? deque)
    (raise
     (condition
       `(exn location ,who
	     message "empty deque, there are no elements to remove"
	     arguments ,(list deque))
       '(pfds-deque-empty-condition)))))


;;;; done

#| end of module |# )

;;; end of file
