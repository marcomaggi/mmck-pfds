;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for deques
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for deques.
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
;;;


;;;; units and module header

(require-library (mmck pfds)
		 (mmck checks))

(module (test-dlists)
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
	  (mmck pfds)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing dlists\n")


;;;; helpers

(define-syntax test-eqv
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body (=> eqv?) ?result))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ ?result ?body)
     (check ?body => ?result))))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))


(parameterise ((check-test-name	'core))

  (check
      (dlist? (dlist))
    => #t)

  (check
      (dlist? 123)
    => #f)

  (check
      (dlist? (dlist 1 2 3))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (dlist->list (dlist 1 2 3))
    => '(1 2 3))

  (check
      (dlist->list (dlist))
    => '())

  #t)


(parameterise ((check-test-name	'insertion))

  (check
      (dlist->list (dlist-cons 99 (dlist 1 2 3)))
    => '(99 1 2 3))

  (check
      (dlist->list (dlist-snoc (dlist 1 2 3) 99))
    => '(1 2 3 99))

  (check
      (dlist->list (dlist-cons 99 (dlist )))
    => '(99 ))

  (check
      (dlist->list (dlist-snoc (dlist ) 99))
    => '( 99))

  #t)


(parameterise ((check-test-name	'append))

  (check
      (dlist->list (dlist-append (dlist 1 2 3) (dlist 4 5 6)))
    => '(1 2 3 4 5 6))

  (check
      (dlist->list (dlist-append (dlist) (dlist 4 5 6)))
    => '(4 5 6))

  (check
      (dlist->list (dlist-append (dlist 1 2 3) (dlist)))
    => '(1 2 3))

  (check
      (dlist->list (dlist-append (dlist) (dlist)))
    => '())

  #t)


(parameterise ((check-test-name	'list))

  (check
      (dlist->list (list->dlist '(1 2 3)))
    => '(1 2 3))

  (check
      (dlist->list (list->dlist '()))
    => '())

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
