;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: module dlists
;;;Date: Apr 30, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the module dlists: purely functional dlists.
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


;;;; commentary:
;;
;;Repeatedly appending  to a list is  a common, if inefficient  pattern in functional
;;programs. Usually the trick we use is to  build up the list in reverse, and then to
;;reverse it as the last action of a function.
;;
;;Dlists are  a representation of lists  as functions that provide  for constant time
;;append to either the front or end of a dlist that may be used instead.


;;;; documentation
;;
;; dlist : any ... -> dlist
;; returns a dlist containing all its arguments.
;;
;; dlist? : any -> boolean
;; returns #t if its argument is a dlist, #f otherwise.
;;
;; dlist-cons : any dlist -> dlist
;; returns a new dlist created by prepending the element to the head
;; of the dlist argument.
;;
;; dlist-snoc : dlist any -> dlist
;; returns a new dlist created by appending the element to the tail of
;; the dlist argument.
;;
;; dlist-append : dlist dlist -> dlist
;; returns a new dlist consisting of all the elements of the first
;; dlist, followed by all the items of the second dlist.
;;
;; dlist->list : dlist -> listof(any)
;; returns a list consisting of all the elements of the dlist.
;;
;; list->dlist : listof(any) -> dlist
;; returns a dlist consisting of all the elements of the list.


(declare (unit mmck.pfds.dlists)
	 (uses mmck.pfds.private.helpers)
	 (uses mmck.pfds.private.coops)
	 (emit-import-library mmck.pfds.dlists))

(module (mmck.pfds.dlists)
    (dlist
     dlist?
     dlist-cons
     dlist-snoc
     dlist-append
     dlist->list
     list->dlist)
  (import (scheme)
	  (mmck pfds private helpers)
	  (mmck pfds private coops))


;;;; implementation

(define-class <dlist>
    (<standard-object>)
  ((proc	#:reader undl)))

(define (dlist? obj)
  (is-a? obj <dlist>))

(define (dlist . args)
  (list->dlist args))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (singleton x)
  (list->dlist (list x)))

(define (dlist-append dl1 dl2)
  (make <dlist> 'proc (compose (undl dl1) (undl dl2))))

(define (dlist-cons element dlist)
  (dlist-append (singleton element) dlist))

(define (dlist-snoc dlist element)
  (dlist-append dlist (singleton element)))

(define (dlist->list dlist)
  ((undl dlist) '()))

(define (list->dlist list)
  (make <dlist> 'proc
   (lambda (rest)
     (append list rest))))


;;;; done

#| end of module |# )

;;; end of file
