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
;;;Copyright (c) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(declare (unit mmck.pfds.private.helpers)
	 (emit-import-library mmck.pfds.private.helpers))

(module (mmck pfds private helpers)
    ((syntax: assert pfds-assertion-violation)
     fold-left
     fold-right
     make-pfds-assertion-violation
     pfds-assertion-violation
     pfds-assertion-violation?
     raise)
  (import (scheme)
	  (only (chicken module)
		reexport))
  (reexport (only (chicken base)
		  define-record-type
		  let-values
		  let*-values
		  unless
		  when
		  delay-force
		  case-lambda)
	    (only (chicken condition)
		  abort
		  condition
		  make-composite-condition
		  condition-case
		  condition-predicate))


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


;;;; done

#| end of module |# )

;;; end of file
