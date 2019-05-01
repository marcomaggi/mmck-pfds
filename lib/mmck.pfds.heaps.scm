;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: module deques
;;;Date: May  1, 2019
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


;;;; documentation
;;
;; make-heap : (any any -> bool) -> heap
;; returns a new empty heap which uses the ordering procedure.
;;
;; heap : (any any -> bool) any ... -> heap
;; return a new heap, ordered by the procedure argument, that contains
;; all the other arguments as elements.
;;
;; heap? : any -> bool
;; returns #t if the argument is a heap, #f otherwise.
;;
;; heap-size : heap -> non-negative integer
;; returns the number of elements in the heap.
;;
;; heap-empty? : heap -> bool
;; returns #t if the heap contains no elements, #f otherwise.
;;
;; heap-min : heap -> any
;; returns the minimum element in the heap, according the heap's
;; ordering procedure. If there are no elements, a
;; &heap-empty-condition is raised.
;;
;; heap-delete-min : heap -> heap
;; returns a new heap containing all the elements of the heap
;; argument, except for the minimum argument, as determined by the
;; heap's ordering procedure. If there are no elements, a
;; &heap-empty-condition is raised.
;;
;; heap-pop : any + heap
;; returns two values: the the minimum value, and a heap obtained by
;; removing the minimum value from the original heap. If the heap is
;; empty, a &heap-empty-condition is raised.
;;
;; heap-insert : heap any -> heap
;; returns the new heap obtained by adding the element to those in the
;; argument heap.
;;
;; heap->list : heap -> Listof(any)
;; returns the heap containing all the elements of the heap. The
;; elements of the list are ordered according to the heap's ordering
;; procedure.
;;
;; list->heap : Listof(any) (any any -> boolean) -> heap
;; returns the heap containing all the elements of the list, and using
;; the procedure argument to order the elements.
;;
;; heap-merge : heap heap -> heap
;; returns the heap containing all the elements of the argument
;; heaps. The argument heaps are assumed to be using the same ordering
;; procedure.
;;
;; heap-sort : (any any -> bool) list -> list
;; returns a new list that is a permutation of the argument list, such
;; that all the elements are ordered by the given procedure.
;;
;; heap-ordering-procedure : heap -> (any any -> boolean)
;; returns the ordering procedure used internally by the heap.
;;
;; heap-empty-condition? : any -> bool
;; returns #t if argument is a &heap-empty condition, #f otherwise.
;;


(declare (unit mmck.pfds.heaps)
	 (uses mmck.pfds.private.helpers)
	 (uses mmck.pfds.private.coops)
	 (emit-import-library mmck.pfds.heaps))

(module (mmck.pfds.heaps)
    (heap
     make-heap
     heap?
     heap-size
     heap-empty?
     heap-min
     heap-delete-min
     heap-insert
     heap-pop
     heap->list
     list->heap
     heap-merge
     heap-sort
     heap-ordering-procedure
     heap-empty-condition?)
  (import (scheme)
	  (mmck pfds private helpers)
	  (mmck pfds private coops))


;;;; implementation

(define-class <node>
    (<standard-object>)
  ((size	#:reader node-size)
   (height	#:reader node-height)
   (value	#:reader node-value)
   (left	#:reader node-left)
   (right	#:reader node-right)))

(define (%make-node size height value left right)
  (make <node>
    'size size 'height height 'value value 'left left 'right right))

(define (node? obj)
  (is-a? obj <node>))

(define-class <leaf>
    (<standard-object>))

(define (make-leaf)
  (make <leaf>))

(define (leaf? obj)
  (is-a? obj <leaf>))

(define (height x)
  (if (leaf? x)
      0
      (node-height x)))

(define (size x)
  (if (leaf? x)
      0
      (node-size x)))

(define (make-node v l r)
  (define sl (height l))
  (define sr (height r))
  (define m (+ 1 (min sl sr)))
  (define sz (+ 1 (size l) (size r)))
  (if (< sl sr)
      (%make-node sz m v r l)
      (%make-node sz m v l r)))

(define (singleton v)
  (%make-node 1 0 v (make-leaf) (make-leaf)))

(define (insert tree value prio<?)
  (merge-trees tree (singleton value) prio<?))

(define (delete-min tree prio<?)
  (merge-trees (node-left tree)
               (node-right tree)
               prio<?))

(define (merge-trees tree1 tree2 prio<?)
  (cond ((leaf? tree1) tree2)
        ((leaf? tree2) tree1)
        ((prio<? (node-value tree2)
                 (node-value tree1))
         (make-node (node-value tree2)
                    (node-left tree2)
                    (merge-trees tree1
                                 (node-right tree2)
                                 prio<?)))
        (else
         (make-node (node-value tree1)
                    (node-left tree1)
                    (merge-trees (node-right tree1)
                                 tree2
                                 prio<?)))))


;; outside interface
(define-class <heap>
    (<standard-object>)
  ((tree		#:reader heap-tree)
   (ordering-predicate	#:reader heap-ordering-predicate)))

(define heap-ordering-procedure heap-ordering-predicate)

(define (%make-heap tree ordering-predicate)
  (make <heap>
    'tree tree 'ordering-predicate ordering-predicate))

(define (heap? obj)
  (is-a? obj <heap>))

(define (make-heap priority<?)
  (%make-heap (make-leaf) priority<?))

(define (heap < . vals)
  (list->heap vals <))

(define (heap-size heap)
  (size (heap-tree heap)))

(define (heap-empty? heap)
  (leaf? (heap-tree heap)))

(define (raise-empty-condition who . irritants)
  (raise (condition
           '(pfds-heap-empty-condition)
           `(exn location  ,who
		 message   "There is no minimum element."
		 arguments ,irritants))))

(define (heap-min heap)
  (when (heap-empty? heap)
    (raise-empty-condition 'heap-min heap))
  (node-value (heap-tree heap)))

(define (heap-delete-min heap)
  (when (heap-empty? heap)
    (raise-empty-condition 'heap-delete-min heap))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (delete-min (heap-tree heap) <) <)))

(define (heap-pop heap)
  (when (heap-empty? heap)
    (raise-empty-condition 'heap-pop heap))
  (let* ((tree (heap-tree heap))
         (top  (node-value tree))
         (<    (heap-ordering-predicate heap))
         (rest (delete-min tree <)))
    (values top
            (%make-heap rest <))))

(define (heap-insert heap value)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (insert (heap-tree heap) value <) <)))

(define (heap->list heap)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (let loop ((tree (heap-tree heap)) (list '()))
      (if (leaf? tree)
          (reverse list)
          (loop (delete-min tree <)
                (cons (node-value tree) list))))))

(define (list->heap list <)
  (%make-heap
   (fold-left (lambda (h item)
                (insert h item <))
              (make-leaf)
              list)
   <))

(define (heap-merge heap1 heap2)
  (define < (heap-ordering-predicate heap1))
  (%make-heap
   (merge-trees (heap-tree heap1)
                (heap-tree heap2)
                <)
   <))

(define (heap-sort < list)
  (heap->list (list->heap list <)))

(define (make-heap-empty-condition)
  (condition '(pfds-heap-empty-condition)))

(define heap-empty-condition?
  (condition-predicate 'pfds-heap-empty-condition))


;;;; done

#| end of module |# )

;;; end of file

