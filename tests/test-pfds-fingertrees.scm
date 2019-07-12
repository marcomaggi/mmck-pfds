;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for fingertrees
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for fingertrees.
;;;
;;;Copyright (c) 2019 Marco Maggi <mrc.mgg@gmail.com>
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
;;;


;;;; units and module header

(require-library (mmck pfds)
		 (mmck checks))

(module (test-fingertrees)
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
	  (rename (mmck pfds)
		  (make-fingertree	%make-fingertree)
		  (list->fingertree	%list->fingertree))
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing fingertrees\n")


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

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))

(define-syntax test-not
  (syntax-rules ()
    ((_ ?body)
     (check ?body => #f))))

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

;; Right now, I am not testing  the monoidal parts of fingertrees, so we
;; use constructor that replaces these with arbitrary values
(define (make-fingertree)
  (%make-fingertree 0 (lambda (x y) x) (lambda (x) x)))

(define (list->fingertree l)
  (%list->fingertree l 0 (lambda (x y) x) (lambda (x) x)))

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))


(parameterise ((check-test-name	'core))

  (check
      (fingertree? (make-fingertree))
    => #t)

  (check
      (fingertree? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (fingertree->list (make-fingertree))
    => '())

;;; --------------------------------------------------------------------

  (test-predicate fingertree? (make-fingertree))
  (test-predicate fingertree-empty? (make-fingertree))

;;; --------------------------------------------------------------------
;;; construction

  (let ((l1 '(a b c d e f))
        (l2 '((#t . f) (#t . e) (#t . d) (#t . c) (#t . b) (#t . a)))
        (l3 '((#f . a) (#f . b) (#f . c) (#f . d) (#f . e) (#f . f)))
        (l4 '((#f . b) (#f . c) (#t . a) (#f . d) (#f . e) (#f . f)))
        (l5 '((#f . e) (#t . d) (#t . c) (#t . b) (#f . f) (#t . a)))
        (make (lambda (alist)
                (fold-left (lambda (tree pair)
                             (if (car pair)
                                 (fingertree-cons (cdr pair) tree)
			       (fingertree-snoc tree (cdr pair))))
		  (make-fingertree)
		  alist)))
        (empty (make-fingertree)))

    (test-eqv #f (fingertree-empty? (fingertree-cons #f empty)))
    (test-eqv #f (fingertree-empty? (fingertree-snoc empty #f)))
    (test-equal l1 (fingertree->list (make l2)))
    (test-equal l1 (fingertree->list (make l3)))
    (test-equal l1 (fingertree->list (make l4)))
    (test-equal l1 (fingertree->list (make l5))))

  #t)


(parameterise ((check-test-name	'removal))

  (let* ((l1 '(a b c d e f))
         (f1 (list->fingertree l1))
         (f2 (make-fingertree)))
    (test-exn pfds-fingertree-empty-condition (fingertree-uncons f2))
    (test-exn pfds-fingertree-empty-condition (fingertree-unsnoc f2))
    (let-values (((head tail) (fingertree-uncons f1)))
      (test-eqv (car l1) head)
      (test-equal (cdr l1) (fingertree->list tail)))
    (let*-values (((init last) (fingertree-unsnoc f1))
		  ((l*) (reverse l1))
		  ((l1-last) (car l*))
		  ((l1-init) (reverse (cdr l*))))
      (test-eqv l1-last last)
      (test-equal l1-init (fingertree->list init))))

  #t)


(parameterise ((check-test-name	'conversion))

  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
		 183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
		 156 218 204 186 155 117)))
    (test-equal '() (fingertree->list (list->fingertree '())))
    (test-equal l1 (fingertree->list (list->fingertree l1)))
    (test-equal l2 (fingertree->list (list->fingertree l2))))

  #t)


(parameterise ((check-test-name	'append))

  (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
		 183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
        (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
		 156 218 204 186 155 117))
        (append* (lambda (a b)
                   (fingertree->list
                    (fingertree-append
                     (list->fingertree a)
                     (list->fingertree b))))))
    (test-equal (append l1 '()) (append* l1 '()))
    (test-equal (append '() l1) (append* '() l1))
    (test-equal (append l1 l2) (append* l1 l2))
    (test-equal (append l1 l1) (append* l1 l1))
    (test-equal (append l1 l2) (append* l1 l2)))

  #t)


(parameterise ((check-test-name	'monoid))

  (let ((l1 '(31 238 100 129 6 169 239 150 96 141
		 207 208 190 45 56 183 199 254 78 210))
        (l2 '((31 238 100 129 6) (169 239 150) (96 141 207 208 190)
              ()  (45 56 183 199) (254 78 210)))
        (car/default (lambda (dflt) (lambda (x) (if (pair? x) (car x) dflt))))
        (list->sum-tree (lambda (l1) (%list->fingertree l1 0 + values))))
    (test-equal 254 (fingertree-measure (%list->fingertree l1 0 max values)))
    (test-equal 6 (fingertree-measure (%list->fingertree l1 1000 min values)))
    (test-equal l1 (fingertree-measure (%list->fingertree l2 '() append values)))
    (test-equal 595 (fingertree-measure
		     (%list->fingertree l2 0 + (car/default 0))))
    ;; sum of l1 is 4239
    (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 0))
							 (list->sum-tree l1))))
		     (fingertree->list (fingertree-append a b))))
    (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 1000))
							 (list->sum-tree l1))))
		     (fingertree->list (fingertree-append a b))))
    (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 2000))
							 (list->sum-tree l1))))
		     (fingertree->list (fingertree-append a b))))
    (test-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 5000))
							 (list->sum-tree l1))))
		     (fingertree->list (fingertree-append a b)))))

  #t)


(parameterise ((check-test-name	'folds))

  (let* ((l '(31 238 100 129 6 169 239 150 96 141
                 207 208 190 45 56 183 199 254 78 210))
         (lrev (reverse l))
         (total (apply + l))
         (ft (list->fingertree l)))
    ;; empty case
    (test-eqv #t (fingertree-fold (lambda _ #f) #t (make-fingertree)))
    (test-eqv #t (fingertree-fold-right (lambda _ #f) #t (make-fingertree)))
    ;; associative operations
    (test-eqv total (fingertree-fold + 0 ft))
    (test-eqv total (fingertree-fold-right + 0 ft))
    ;; non-associative operations
    (test-equal lrev (fingertree-fold cons '() ft))
    (test-equal l (fingertree-fold-right cons '() ft)))

  #t)


(parameterise ((check-test-name	'reversal))

  (define (list->last-tree l)
    (define *cookie* (cons 'no 'last))
    (define (pick x y)
      (if (eq? *cookie* y)
	  x
        y))
    (%list->fingertree l *cookie* pick values))

  (define (list->product-tree l)
    (%list->fingertree l 1 * values))

  (let ((rev (lambda (l)
               (fingertree->list
                (fingertree-reverse (list->fingertree l)))))
        (id (lambda (l)
              (fingertree->list
               (fingertree-reverse
                (fingertree-reverse (list->fingertree l))))))
        (l1 '(126 6 48 86 2 119 233 92 230 160))
        (l2 '(25 168 21 246 39 211 60 83 103 161
		 192 201 31 253 156 218 204 186 155 117)))
    ;; behaves the same as regular reverse on lists
    (test-eqv '() (rev '()))
    (test-equal '(1) (rev '(1)))
    (test-equal '(6 5 4 3 2 1) (rev '(1 2 3 4 5 6)))
    (test-equal (reverse l1) (rev l1))
    (test-equal (reverse l2) (rev l2))
    ;; double reversal is the the same list
    (test-equal l1 (id l1))
    (test-equal l2 (id l2))
    ;; a fingertree will have the same measure as its reverse if
    ;; the monoid is commutative
    (test-equal (fingertree-measure (list->product-tree l1))
		(fingertree-measure
		 (fingertree-reverse (list->product-tree l1))))
    ;; otherwise they are not necessarily the same
    ;; in this case, they are the same only if the first and last
    ;; elements are the same
    (test-not
     (equal? (fingertree-measure (list->last-tree l2))
	     (fingertree-measure (fingertree-reverse (list->product-tree l2))))))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
