;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: common helper functions
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines common helper functions.
;;;
;;;Copyright (c) 2019 Marco Maggi <mrc.mgg@gmail.com>
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


(declare (unit mmck.pfds.helpers)
	 (emit-import-library mmck.pfds.helpers))

(module (mmck pfds helpers)
    ((syntax: assert pfds-assertion-violation)
     (syntax: define*)
     (syntax: case-define*)
     fold-left
     fold-right
     make-pfds-assertion-violation
     pfds-assertion-violation
     pfds-assertion-violation?
     assert-argument-type
     assert-argument-list-type
     raise)
  (import (scheme)
	  (only (chicken module)
		reexport))
  (reexport (only (chicken base)
		  define-record-type
		  define-record-printer
		  define-constant
		  let-values
		  let*-values
		  receive
		  unless
		  when
		  delay-force
		  case-lambda
		  call/cc)
	    (only (chicken condition)
		  abort
		  condition
		  make-composite-condition
		  condition-case
		  condition-predicate)
	    (only (chicken format)
		  format))


;;;; exceptional-condition objects and related stuff

(define (raise obj)
  (abort obj))

;;; --------------------------------------------------------------------

(define (make-pfds-assertion-violation)
  (condition
    '(pfds-assertion-violation)))

(define (pfds-assertion-violation who message . irritants)
  (raise
   (make-composite-condition
    (condition `(exn location ,who message ,message arguments ,irritants))
    (make-pfds-assertion-violation))))

(define pfds-assertion-violation?
  (condition-predicate 'pfds-assertion-violation))

(define-syntax assert
  (syntax-rules ()
    ((_ ?expr)
     (unless ?expr
       (pfds-assertion-violation 'assert "failed assertion" (quote ?expr))))
    ))

;;; --------------------------------------------------------------------

(define (assert-argument-type who type.str type-pred arg arg.idx)
  (unless (type-pred arg)
    (pfds-assertion-violation who (string-append "expected argument "
						 (number->string arg.idx)
						 " of type \"" type.str "\"")
			      arg)))

(define (assert-argument-list-type who type.str type-pred arg* first-arg.idx)
  (fold-left (lambda (arg.idx arg)
	       (if (type-pred arg)
		   (+ 1 arg.idx)
		 (pfds-assertion-violation who (string-append "expected argument "
							      (number->string arg.idx)
							      " of type \"" type.str "\"")
					   arg)))
    first-arg.idx
    arg*))


;;;; misc

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))

(define (fold-right combine nil ell)
  (let loop ((combine	combine)
	     (nil	nil)
	     (ell	(reverse ell)))
    (if (pair? ell)
	(loop combine (combine (car ell) nil) (cdr ell))
      nil)))


;;;; syntaxes: define*, case-define*

(define-syntax %expand-define*/function
  (ir-macro-transformer
    (lambda (input-form inject compare)
      ;;We expect the following input form:
      ;;
      ;;  (%expand-define* (?who . ?formals) ?body0 ?body ...)
      ;;
      (let ((?who	(caadr input-form))
	    (?formals	(cdadr input-form))
	    (?body*	(cddr  input-form)))
	(let ((%__who__ (inject '__who__)))
	  `(define (,?who . ,?formals)
	     (let ((,%__who__ (quote ,?who)))
	       . ,?body*)))))))

(define-syntax %expand-define*/variable
  (ir-macro-transformer
    (lambda (input-form inject compare)
      ;;We expect the following input form:
      ;;
      ;;  (%expand-define* ?who ?expr)
      ;;
      (let ((?who	(cadr  input-form))
	    (?expr	(caddr input-form)))
	(let ((%__who__ (inject '__who__)))
	  `(define ,?who
	     (let ((,%__who__ (quote ,?who)))
	       ,?expr)))))))

(define-syntax define*
  (syntax-rules ()
    ((_ (?who . ?formals) ?body0 ?body ...)
     (%expand-define*/function (?who . ?formals) ?body0 ?body ...))
    ((_ ?who ?expr)
     (%expand-define*/variable ?who ?expr))
    ))

(define-syntax case-define*
  (syntax-rules ()
    ((_ ?who (?formals ?body0 ?body ...) ...)
     (define* ?who
       (case-lambda (?formals ?body0 ?body ...) ...)))
    ))


;;;; done

#| end of module |# )

;;; end of file
