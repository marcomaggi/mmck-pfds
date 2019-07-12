;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: module sets
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the module sets: purely functional sets.
;;;
;;;Copyright (c) 2019 Marco Maggi <mrc.mgg@gmail.com>
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
;; set? : any -> boolean
;; returns #t if the object is a set, #f otherwise
;;
;; make-set : (any any -> boolean) -> set
;; returns a new empty set ordered by the < procedure
;;
;; set-member? : set any -> boolean
;; returns true if element is in the set
;;
;; set-insert : set any -> set
;; returns a new set created by inserting element into the set argument
;;
;; set-remove : set element -> set
;; returns a new set created by removing element from the set
;;
;; set-size : set -> non-negative integer
;; returns the number of elements in the set
;;
;; set<? : set set -> boolean
;; returns #t if set1 is a proper subset of set2, #f otherwise. That
;; is, if all elements of set1 are in set2, and there is at least one
;; element of set2 not in set1.
;;
;; set<=? : set set -> boolean
;; returns #t if set1 is a subset of set2, #f otherwise, i.e. if all
;; elements of set1 are in set2.
;;
;; set=? : set set -> boolean
;; returns #t if every element of set1 is in set2, and vice versa, #f
;; otherwise.
;;
;; set>=? : set set -> boolean
;; returns #t if set2 is a subset of set1, #f otherwise.
;;
;; set>? : set set -> boolean
;; returns #t if set2 is a proper subset of set1, #f otherwise.
;;
;; subset? : set set -> boolean
;; same as set<=?
;;
;; proper-subset? : set set -> boolean
;; same as set<?
;;
;; set-map : (any -> any) set -> set
;; returns the new set created by applying proc to each element of the set
;;
;; set-fold : (any any -> any) any set -> any
;; returns the value obtained by iterating the procedure over each
;; element of the set and an accumulator value. The value of the
;; accumulator is initially base, and the return value of proc is used
;; as the accumulator for the next iteration.
;;
;; list->set : Listof(any) (any any -> any) -> set
;; returns the set containing all the elements of the list, ordered by <.
;;
;; set->list : set -> Listof(any)
;; returns all the elements of the set as a list
;;
;; set-union : set set -> set
;; returns the union of set1 and set2, i.e. contains all elements of
;; set1 and set2.
;;
;; set-intersection : set set -> set
;; returns the intersection of set1 and set2, i.e. the set of all
;; items that are in both set1 and set2.
;;
;; set-difference : set set -> set
;; returns the difference of set1 and set2, i.e. the set of all items
;; in set1 that are not in set2.
;;
;; set-ordering-procedure : set -> (any any -> boolean)
;; returns the ordering procedure used internall by the set.


(declare (unit mmck.pfds.sets)
	 (uses mmck.pfds.helpers)
	 (uses mmck.pfds.bbtrees)
	 (emit-import-library mmck.pfds.sets))

(module (mmck.pfds.sets)
    (set?
     make-set
     set-member?
     set-insert
     set-remove
     set-size
     set<?
     set<=?
     set=?
     set>=?
     set>?
     subset?
     proper-subset?
     set-map
     set-fold
     list->set
     set->list
     set-union
     set-intersection
     set-difference
     set-ordering-procedure)
  (import (scheme)
	  (mmck pfds helpers)
	  (mmck pfds bbtrees))


;;;; implementation

(define dummy #f)

;;; basic sets
(define-record-type <set>
  (%make-set tree)
  set?
  (tree		set-tree))

(define (set-ordering-procedure set)
  (bbtree-ordering-procedure (set-tree set)))

(define (make-set <)
  (%make-set (make-bbtree <)))

;; provide a (make-equal-set) function?

(define (set-member? set element)
  (bbtree-contains? (set-tree set) element))

(define (set-insert set element)
  (%make-set (bbtree-set (set-tree set) element dummy)))

(define (set-remove set element)
  (%make-set (bbtree-delete (set-tree set) element)))

(define (set-size set)
  (bbtree-size (set-tree set)))

;;; set equality
(define (set<=? set1 set2)
  (let ((t (set-tree set2)))
    (bbtree-traverse (lambda (k _ l r b)
                       (and (bbtree-contains? t k)
                            (l #t)
                            (r #t)))
                     #t
                     (set-tree set1))))

(define (set<? set1 set2)
  (and (< (set-size set1)
          (set-size set2))
       (set<=? set1 set2)))

(define (set>=? set1 set2)
  (set<=? set2 set1))

(define (set>? set1 set2)
  (set<? set2 set1))

(define (set=? set1 set2)
  (and (set<=? set1 set2)
       (set>=? set1 set2)))

(define subset? set<=?)

(define proper-subset? set<?)

;;; iterators
(define (set-map proc set)
  ;; currently restricted to returning a set with the same ordering, I
  ;; could weaken this to, say, comparing with < on the object-hash,
  ;; or I make it take a < argument for the result set.
  (let ((tree (set-tree set)))
    (%make-set
     (bbtree-fold (lambda (key _ tree)
                    (bbtree-set tree (proc key) dummy))
                  (make-bbtree (bbtree-ordering-procedure tree))
                  tree))))

(define (set-fold proc base set)
  (bbtree-fold (lambda (key value base)
                 (proc key base))
               base
               (set-tree set)))

;;; conversion
(define (list->set list <)
  (fold-left (lambda (tree element)
               (set-insert tree element))
             (make-set <)
             list))

(define (set->list set)
  (set-fold cons '() set))

;;; set operations
(define (set-union set1 set2)
  (%make-set (bbtree-union (set-tree set1) (set-tree set2))))

(define (set-intersection set1 set2)
  (%make-set (bbtree-intersection (set-tree set1) (set-tree set2))))

(define (set-difference set1 set2)
  (%make-set (bbtree-difference (set-tree set1) (set-tree set2))))


;;;; done

#| end of module |# )

;;; end of file

