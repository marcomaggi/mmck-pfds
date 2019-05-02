;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: module psqs
;;;Date: May  1, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the module psqs.
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


;;;; Documentation
;;
;; Priority search queues are a combination of two common abstract
;; data types: finite maps, and priority queues. As such, it provides
;; for access, insertion, removal and update on arbitrary keys, as
;; well as for easy removal of the element with the lowest priority.
;;
;; Note: where a procedure takes a key or priority these are expected
;; to be compatible with the relevant ordering procedures on the psq.
;;
;;;; Basic operations
;;
;; make-psq : < < -> psq
;; takes a two ordering procedures, one for keys, and another for
;; priorities, and returns an empty priority search queue
;;
;; psq? : obj -> boolean
;; returns #t if the object is a priority search queue, #f otherwise.
;;
;; psq-empty? : psq -> boolean
;; returns #t if the priority search queue contains no elements, #f
;; otherwise.
;;
;; psq-size : psq -> non-negative integer
;; returns the number of associations in the priority search queue
;;
;;;; Finite map operations
;;
;; psq-ref : psq key -> priority
;; returns the priority of a key if it is in the priority search
;; queue. If the key is not in the priority queue an
;; pfds-assertion-violation is raised.
;;
;; psq-set : psq key priority -> psq
;; returns the priority search queue obtained from inserting a key
;; with a given priority. If the key is already in the priority search
;; queue, it updates the priority to the new value.
;;
;; psq-update : psq key (priority -> priority) priority -> psq
;; returns the priority search queue obtained by modifying the
;; priority of key, by the given function. If the key is not in the
;; priority search queue, it is inserted with the priority obtained by
;; calling the function on the default value.
;;
;; psq-delete : psq key -> psq
;; returns the priority search queue obtained by removing the
;; key-priority association from the priority search queue. If the key
;; is not in the queue, then the returned search queue will be the
;; same as the original.
;;
;; psq-contains? : psq key -> boolean
;; returns #t if there is an association for the given key in the
;; priority search queue, #f otherwise.
;;
;;;; Priority queue operations
;;
;; psq-min : psq -> key
;;
;; returns the key of the minimum association in the priority search
;; queue. If the queue is empty, an assertion violation is raised.
;;
;; psq-delete-min : psq -> psq
;; returns the priority search queue obtained by removing the minimum
;; association in the priority search queue. If the queue is empty, an
;; assertion violation is raised.
;;
;; psq-pop : psq -> key + psq
;; returns two values: the minimum key and the priority search queue
;; obtained by removing the minimum association from the original
;; queue. If the queue is empty, an assertion violation is raised.
;;
;;;; Ranged query functions
;;
;; psq-at-most : psq priority -> ListOf(key . priority)
;; returns an alist containing all the associations in the priority
;; search queue with priority less than or equal to a given value. The
;; alist returned is ordered by key according to the predicate for the
;; psq.
;;
;; psq-at-most-range : psq priority key key -> ListOf(key . priority)
;; Similar to psq-at-most, but it also takes an upper and lower bound,
;; for the keys it will return. These bounds are inclusive.
;;


(declare (unit mmck.pfds.psqs)
	 (uses mmck.pfds.helpers)
	 (uses mmck.pfds.coops)
	 (emit-import-library mmck.pfds.psqs))

(module (mmck.pfds.psqs)
    (make-psq
     psq?
     psq-empty?
     psq-size
     ;; map operations
     psq-ref
     psq-set
     psq-update
     psq-delete
     psq-contains?
     ;; priority queue operations
     psq-min
     psq-delete-min
     psq-pop
     ;; ranged query operations
     psq-at-most
     psq-at-most-range)
  (import (except (scheme) min)
	  (mmck pfds helpers)
	  (mmck pfds coops))


;;; record types

(define-class <void>
    (<standard-object>))

(define (make-void)
  (make <void>))

(define (void? obj)
  (is-a? obj <void>))

;;; --------------------------------------------------------------------

(define-class <winner>
    (<standard-object>)
  ((key			#:reader winner-key)
   (priority		#:reader winner-priority)
   (loser-tree		#:reader winner-loser-tree)
   (maximum-key		#:reader winner-maximum-key)))

(define (make-winner key priority loser-tree maximum-key)
  (make <winner>
    'key key 'priority priority 'loser-tree loser-tree 'maximum-key maximum-key))

(define (winner? obj)
  (is-a? obj <winner>))

;;; --------------------------------------------------------------------

(define-class <start>
    (<standard-object>))

(define (make-start)
  (make <start>))

(define (start? obj)
  (is-a? obj <start>))

;;; --------------------------------------------------------------------

(define-class <loser>
    (<standard-object>)
  ((size	#:reader loser-size)
   (key		#:reader loser-key)
   (priority	#:reader loser-priority)
   (left	#:reader loser-left)
   (split-key	#:reader loser-split-key)
   (right	#:reader loser-right)))

(define (make-loser key priority left split-key right)
  (make <loser>
    'size (+ (size left) (size right) 1)
    'key key
    'priority priority
    'left left
    'split-key split-key
    'right right))

(define (loser? obj)
  (is-a? obj <loser>))


;;; functions

(define (maximum-key psq)
  (winner-maximum-key psq))

(define max-key maximum-key)

(define empty (make-void))

(define (singleton key priority)
  (make-winner key priority (make-start) key))

(define (play-match psq1 psq2 key<? prio<?)
  (cond ((void? psq1) psq2)
        ((void? psq2) psq1)
        ((not (prio<? (winner-priority psq2)
                      (winner-priority psq1)))
         (let ((k1 (winner-key psq1))
               (p1 (winner-priority psq1))
               (t1 (winner-loser-tree psq1))
               (m1 (winner-maximum-key psq1))
               (k2 (winner-key psq2))
               (p2 (winner-priority psq2))
               (t2 (winner-loser-tree psq2))
               (m2 (winner-maximum-key psq2)))
           (make-winner k1
                        p1
                        (balance k2 p2 t1 m1 t2 key<? prio<?)
                        m2)))
        (else
         (let ((k1 (winner-key psq1))
               (p1 (winner-priority psq1))
               (t1 (winner-loser-tree psq1))
               (m1 (winner-maximum-key psq1))
               (k2 (winner-key psq2))
               (p2 (winner-priority psq2))
               (t2 (winner-loser-tree psq2))
               (m2 (winner-maximum-key psq2)))
           (make-winner k2
                        p2
                        (balance k1 p1 t1 m1 t2 key<? prio<?)
                        m2)))))

(define (second-best ltree key key<? prio<?)
  (if (start? ltree)
      (make-void)
      (let ((k (loser-key ltree))
            (p (loser-priority ltree))
            (l (loser-left ltree))
            (m (loser-split-key ltree))
            (r (loser-right ltree)))
        (if (not (key<? m k))
            (play-match (make-winner k p l m)
                        (second-best r key key<? prio<?)
                        key<?
                        prio<?)
            (play-match (second-best l m key<? prio<?)
                        (make-winner k p r key)
                        key<?
                        prio<?)))))

(define (delete-min psq key<? prio<?)
  ;; maybe void psqs should return void?
  (second-best (winner-loser-tree psq) (winner-maximum-key psq) key<? prio<?))

(define (psq-case psq empty-k singleton-k match-k key<?)
  (if (void? psq)
      (empty-k)
      (let ((k1 (winner-key psq))
            (p1 (winner-priority psq))
            (t  (winner-loser-tree psq))
            (m  (winner-maximum-key psq)))
        (if (start? t)
            (singleton-k k1 p1)
            (let ((k2 (loser-key t))
                  (p2 (loser-priority t))
                  (l  (loser-left t))
                  (s  (loser-split-key t))
                  (r  (loser-right t)))
              (if (not (key<? s k2))
                  (match-k (make-winner k2 p2 l s)
                           (make-winner k1 p1 r m))
                  (match-k (make-winner k1 p1 l s)
                           (make-winner k2 p2 r m))))))))

(define (lookup psq key default key<?)
  (psq-case psq
            (lambda () default)
            (lambda (k p)
              (if (or (key<? k key) (key<? key k))
                  default
                  p))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (lookup w1 key default key<?)
                  (lookup w2 key default key<?)))
            key<?))

(define (update psq key f default key<? prio<?)
  (psq-case psq
            (lambda () (singleton key (f default)))
            (lambda (k p)
              (cond ((key<? key k)
                     (play-match (singleton key (f default))
                                 (singleton k p)
                                 key<?
                                 prio<?))
                    ((key<? k key)
                     (play-match (singleton k p)
                                 (singleton key (f default))
                                 key<?
                                 prio<?))
                    (else
                     (singleton key (f p)))))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (update w1 key f default key<? prio<?)
                              w2
                              key<?
                              prio<?)
                  (play-match w1
                              (update w2 key f default key<? prio<?)
                              key<?
                              prio<?)))
            key<?))

(define (insert psq key val key<? prio<?)
  (psq-case psq
            (lambda () (singleton key val))
            (lambda (k p)
              (cond ((key<? key k)
                     (play-match (singleton key val)
                                 (singleton k p)
                                 key<?
                                 prio<?))
                    ((key<? k key)
                     (play-match (singleton k p)
                                 (singleton key val)
                                 key<?
                                 prio<?))
                    (else
                     (singleton key val))))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (insert w1 key val key<? prio<?) w2 key<? prio<?)
                  (play-match w1 (insert w2 key val key<? prio<?) key<? prio<?)))
            key<?))

(define (delete psq key key<? prio<?)
  (psq-case psq
            (lambda () empty)
            (lambda (k p)
              (if (or (key<? k key)
                      (key<? key k))
                  (singleton k p)
                  empty))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (delete w1 key key<? prio<?) w2 key<? prio<?)
                  (play-match w1 (delete w2 key key<? prio<?) key<? prio<?)))
            key<?))

(define (min tree)
  (when (void? tree)
    (pfds-assertion-violation 'psq-min
                         "Can't take the minimum of an empty priority search queue"))
  (winner-key tree))

(define (pop tree key<? prio<?)
  (when (void? tree)
    (pfds-assertion-violation 'psq-pop
                         "Can't pop from an empty priority search queue"))
  (values (winner-key tree)
          (delete-min tree key<? prio<?)))

;; at-most and at-most-range are perfect examples of when to use
;; dlists, but we do not do that here
(define (at-most psq p key<? prio<?)
  (define (at-most psq accum)
    (if (and (winner? psq)
             (prio<? p (winner-priority psq)))
        accum
        (psq-case psq
                  (lambda () accum)
                  (lambda (k p) (cons (cons k p) accum))
                  (lambda (m1 m2)
                    (at-most m1 (at-most m2 accum)))
                  key<?)))
  (at-most psq '()))

(define (at-most-range psq p lower upper key<? prio<?)
  (define (within-range? key)
    ;; lower <= k <= upper
    (not (or (key<? key lower) (key<? upper key))))
  (define (at-most psq accum)
    (if (and (winner? psq)
             (prio<? p (winner-priority psq)))
        accum
        (psq-case psq
                  (lambda () accum)
                  (lambda (k p)
                    (if (within-range? k)
                        (cons (cons k p) accum)
                        accum))
                  (lambda (m1 m2)
                    (let ((accum* (if (key<? upper (max-key m1))
                                      accum
                                      (at-most m2 accum))))
                      (if (key<? (max-key m1) lower)
                          accum*
                          (at-most m1 accum*))))
                  key<?)))
  (at-most psq '()))

;;; Maintaining balance
(define weight 4) ; balancing constant

(define (size ltree)
  (if (start? ltree)
      0
      (loser-size ltree)))

(define (balance key priority left split-key right key<? prio<?)
  (let ((l-size (size left))
        (r-size (size right)))
    (cond ((< (+ l-size r-size) 2)
           (make-loser key priority left split-key right))
          ((> r-size (* weight l-size))
           (balance-left key priority left split-key right key<? prio<?))
          ((> l-size (* weight r-size))
           (balance-right key priority left split-key right key<? prio<?))
          (else
           (make-loser key priority left split-key right)))))

(define (balance-left key priority left split-key right key<? prio<?)
  (if (< (size (loser-left right))
         (size (loser-right right)))
      (single-left key priority left split-key right key<? prio<?)
      (double-left key priority left split-key right key<? prio<?)))

(define (balance-right key priority left split-key right key<? prio<?)
  (if (< (size (loser-right left))
         (size (loser-left left)))
      (single-right key priority left split-key right key<? prio<?)
      (double-right key priority left split-key right key<? prio<?)))

(define (single-left key priority left split-key right key<? prio<?)
  (let ((right-key (loser-key right))
        (right-priority (loser-priority right))
        (right-left (loser-left right))
        (right-split-key (loser-split-key right))
        (right-right (loser-right right)))
    ;; test
    (if (and (not (key<? right-split-key right-key))
             (not (prio<? right-priority priority)))
        (make-loser key
                    priority
                    (make-loser right-key right-priority left split-key right-left)
                    right-split-key
                    right-right
                    )
        (make-loser right-key
                    right-priority
                    (make-loser key priority left split-key right-left)
                    right-split-key
                    right-right))))

(define (double-left key priority left split-key right key<? prio<?)
  (let ((right-key (loser-key right))
        (right-priority (loser-priority right))
        (right-left (loser-left right))
        (right-split-key (loser-split-key right))
        (right-right (loser-right right)))
    (single-left key
                 priority
                 left
                 split-key
                 (single-right right-key
                               right-priority
                               right-left
                               right-split-key
                               right-right
                               key<?
                               prio<?)
                 key<?
                 prio<?)))

(define (single-right key priority left split-key right key<? prio<?)
  (let ((left-key (loser-key left))
        (left-priority (loser-priority left))
        (left-left (loser-left left))
        (left-split-key (loser-split-key left))
        (left-right (loser-right left)))
    (if (and (key<? left-split-key left-key)
             (not (prio<? left-priority priority)))
        (make-loser key
                    priority
                    left-left
                    left-split-key
                    (make-loser left-key left-priority left-right split-key right))
        (make-loser left-key
                    left-priority
                    left-left
                    left-split-key
                    (make-loser key priority left-right split-key right)))))

(define (double-right key priority left split-key right key<? prio<?)
  (let ((left-key (loser-key left))
        (left-priority (loser-priority left))
        (left-left (loser-left left))
        (left-split-key (loser-split-key left))
        (left-right (loser-right left)))
    (single-right key
                  priority
                  (single-left left-key
                               left-priority
                               left-left
                               left-split-key
                               left-right
                               key<?
                               prio<?)
                  split-key
                  right
                  key<?
                  prio<?)))

;;; Exported Type

(define-class <psq>
    (<standard-object>)
  ((key<?		#:reader psq-key<?)
   (priority<?		#:reader psq-priority<?)
   (tree		#:reader psq-tree)))

(define (%make-psq key<? priority<? tree)
  (make <psq>
    'key<? key<?
    'priority<? priority<?
    'tree tree))

(define (psq? obj)
  (is-a? obj <psq>))

(define (%update-psq psq new-tree)
  (%make-psq (psq-key<? psq)
             (psq-priority<? psq)
             new-tree))

;;; Exported Procedures

(define (make-psq key<? priority<?)
  (%make-psq key<? priority<? (make-void)))

(define (psq-empty? psq)
  (assert (psq? psq))
  (void? (psq-tree psq)))

(define (psq-ref psq key)
  (define cookie (cons #f #f))
  (assert (psq? psq))
  (let ((val (lookup (psq-tree psq) key cookie (psq-key<? psq))))
    (if (eq? val cookie)
        (pfds-assertion-violation 'psq-ref "not in tree" psq key)
        val)))

(define (psq-set psq key priority)
  (assert (psq? psq))
  (%update-psq psq
               (insert (psq-tree psq) key priority (psq-key<? psq) (psq-priority<? psq))))

(define (psq-update psq key f default)
  (assert (psq? psq))
  (%update-psq psq (update (psq-tree psq) key f default (psq-key<? psq) (psq-priority<? psq))))

(define (psq-delete psq key)
  (assert (psq? psq))
  (%update-psq psq (delete (psq-tree psq) key (psq-key<? psq) (psq-priority<? psq))))

(define (psq-contains? psq key)
  (define cookie (cons #f #f))
  (assert (psq? psq))
  (let ((val (lookup (psq-tree psq) key cookie (psq-key<? psq))))
    (not (eq? val cookie))))

(define (psq-min psq)
  (assert (psq? psq))
  (min (psq-tree psq)))

(define (psq-delete-min psq)
  (assert (and (psq? psq)
               (not (psq-empty? psq))))
  (%update-psq psq (delete-min (psq-tree psq) (psq-key<? psq) (psq-priority<? psq))))

(define (psq-pop psq)
  (assert (psq? psq))
  (let-values (((min rest) (pop (psq-tree psq) (psq-key<? psq) (psq-priority<? psq))))
    (values min (%update-psq psq rest))))

(define (psq-at-most psq max-priority)
  (assert (psq? psq))
  (let ((tree   (psq-tree psq))
        (key<?  (psq-key<? psq))
        (prio<? (psq-priority<? psq)))
    (at-most tree max-priority key<? prio<?)))

(define (psq-at-most-range psq max-priority min-key max-key)
  (assert (psq? psq))
  (let ((tree   (psq-tree psq))
        (key<?  (psq-key<? psq))
        (prio<? (psq-priority<? psq)))
    (at-most-range tree max-priority min-key max-key key<? prio<?)))

(define (psq-size psq)
  (assert (psq? psq))
  (let ((tree (psq-tree psq)))
    (if (winner? tree)
        (+ 1 (size (winner-loser-tree tree)))
        0)))


;;;; done

#| end of module |# )

;;; end of file
