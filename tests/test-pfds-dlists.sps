;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: PFDS
;;;Contents: generic library tests
;;;Date: Tue Aug 13, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>
;;;
;;;Author: Ian Price <ianprice90@googlemail.com>
;;;
;;;This program is free software,  you can redistribute it and/or modify
;;;it under the terms of the new-style BSD license.
;;;
;;;You should  have received a copy  of the BSD license  along with this
;;;program.  If not, see <http://www.debian.org/misc/bsd.license>.
;;;


#!r6rs
(import (vicare)
  (pfds dlists)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing PFDS: dlists\n")


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
    ((_ ?predicate ?body)
     (check
	 (guard (E ((?predicate E)
		    #t)
		   (else #f))
	   ?body)
       => #t))))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ ?pred ?body)
     (check
	 (?pred ?body)
       => #t))))

;;; --------------------------------------------------------------------

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))


(parametrise ((check-test-name	'core))

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


(parametrise ((check-test-name	'insertion))

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


(parametrise ((check-test-name	'append))

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


(parametrise ((check-test-name	'list))

  (check
      (dlist->list (list->dlist '(1 2 3)))
    => '(1 2 3))

  (check
      (dlist->list (list->dlist '()))
    => '())

  #t)


;;;; done

(check-report)

;;; end of file
