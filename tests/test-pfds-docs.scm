;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: test program for examples in the documentation
;;;Date: May  2, 2019
;;;
;;;Abstract
;;;
;;;	This is a test program for the examples in the documentation.
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

(module (test-dlists)
    ()
  (import (scheme)
	  (only (chicken base)
		receive)
	  (chicken pretty-print)
	  (mmck pfds)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing documentation examples\n")


;;;; helpers

(define (char< ch1 ch2)
  (< (char->integer ch1)
     (char->integer ch2)))

(define (char= ch1 ch2)
  (= (char->integer ch1)
     (char->integer ch2)))

(define (string< str1 str2)
  (if (eq? str1 str2)
      #f
    (let ((len1 (string-length str1))
	  (len2 (string-length str2)))
      (if (< len1 len2)
	  (let next-char ((idx  0)
			  (len1 len1)
			  (str1 str1)
			  (str2 str2))
	    (or (= idx len1)
		(let ((ch1 (string-ref str1 idx))
		      (ch2 (string-ref str2 idx)))
		  (or (char< ch1 ch2)
		      (if (char= ch1 ch2)
			  (next-char (+ 1 idx) len1 str1 str2)
			#f)))))
	(let next-char ((idx  0)
			(len2 len2)
			(str1 str1)
			(str2 str2))
	  (if (= idx len2)
	      #f
	    (let ((ch1 (string-ref str1 idx))
		  (ch2 (string-ref str2 idx)))
	      (or (char< ch1 ch2)
		  (if (char= ch1 ch2)
		      (next-char (+ 1 idx) len2 str1 str2)
		    #f)))))))))


(parameterise ((check-test-name	'dlists))

  (pretty-print (dlist 1 2 3))

  (check
      (let* ((dell (dlist 5))
	     (dell (dlist-cons 4 dell))
	     (dell (dlist-snoc dell 6)))
	dell)
    (=> dlist=?) (dlist 4 5 6))

  (check
      (dlist-append (dlist 1 2)
		    (dlist 3 4)
		    (dlist 5 6)
		    (dlist 7 8))
    (=> dlist=?) (dlist 1 2 3 4 5 6 7 8))

  (check
      (dlist-append)
    (=> dlist=?) (dlist))

  (values))


(parameterise ((check-test-name	'queues))

  (pretty-print (make-queue))

  (check
      (let* ((Q (make-queue))
	     (Q (enqueue Q 1))
	     (Q (enqueue Q 2))
	     (Q (enqueue Q 3)))
	(receive (A Q)
	    (dequeue Q)
	  (receive (B Q)
	      (dequeue Q)
	    (receive (C Q)
		(dequeue Q)
	      (values A B C)))))
    => 1 2 3)

  (check
      (let* ((Q (make-queue))
	     (Q (enqueue Q 1))
	     (Q (enqueue Q 2))
	     (Q (enqueue Q 3)))
	(receive (A Q1)
	    (dequeue Q)
	  (receive (B Q1)
	      (dequeue Q)
	    (receive (C Q1)
		(dequeue Q)
	      (values A B C)))))
    => 1 1 1)

  (values))


(parameterise ((check-test-name	'deques))

  (pretty-print (make-deque))

  (check
      (let* ((Q (make-deque))
	     (Q (enqueue-front Q 1))
	     (Q (enqueue-front Q 2))
	     (Q (enqueue-front Q 3)))
	(receive (A Q)
	    (dequeue-front Q)
	  (receive (B Q)
	      (dequeue-front Q)
	    (receive (C Q)
		(dequeue-front Q)
	      (values A B C)))))
    => 3 2 1)

  (check
      (let* ((Q (make-deque))
	     (Q (enqueue-front Q 1))
	     (Q (enqueue-front Q 2))
	     (Q (enqueue-front Q 3)))
	(receive (A Q)
	    (dequeue-rear Q)
	  (receive (B Q)
	      (dequeue-rear Q)
	    (receive (C Q)
		(dequeue-rear Q)
	      (values A B C)))))
    => 1 2 3)

  (check
      (let* ((Q (make-deque))
	     (Q (enqueue-front Q 1))
	     (Q (enqueue-front Q 2))
	     (Q (enqueue-front Q 3)))
	(receive (A Q1)
	    (dequeue-front Q)
	  (receive (B Q1)
	      (dequeue-front Q)
	    (receive (C Q1)
		(dequeue-front Q)
	      (values A B C)))))
    => 3 3 3)

  (values))


(parameterise ((check-test-name	'psqs))

  (define key< string<)
  (define priority< <)

  (pretty-print (make-psq key< priority<))

  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 2))
	     (Q (psq-set Q "hello" 1))
	     (Q (psq-set Q "ciao"  1))
	     (Q (psq-set Q "ohayo" 2)))
	(receive (key1 Q1)
	    (psq-pop Q)
	  (receive (key2 Q2)
	      (psq-pop Q1)
	    (receive (key3 Q3)
		(psq-pop Q2)
	      (receive (key4 Q4)
		  (psq-pop Q3)
		(values key1 key2 key3 key4))))))
    => "ciao" "hello" "ohayo" "salut")

;;; --------------------------------------------------------------------

  ;;psq-ref
  ;;
  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 2))
	     (Q (psq-set Q "hello" 1))
	     (Q (psq-set Q "ciao"  4))
	     (Q (psq-set Q "ohayo" 3)))
	(values (psq-ref Q "salut")
		(psq-ref Q "hello")
		(psq-ref Q "ciao")
		(psq-ref Q "ohayo")))
    => 2 1 4 3)

  ;;psq-set
  ;;
  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 2))
	     (Q (psq-set Q "hello" 1))
	     (Q (psq-set Q "ciao"  4))
	     (Q (psq-set Q "ohayo" 3)))
	(values (psq-ref Q "salut")
		(psq-ref Q "hello")
		(psq-ref Q "ciao")
		(psq-ref Q "ohayo")))
    => 2 1 4 3)

  ;;psq-update, key already inserted
  ;;
  (check
      (let* ((Q  (make-psq key< priority<))
	     (Q  (psq-set Q "salut" 2))
	     (P1 (psq-ref Q "salut"))
	     (Q  (psq-update Q "salut"
			     (lambda (P1)
			       (+ 2 P1))
			     0))
	     (P2 (psq-ref Q "salut")))
	(values P1 P2))
    => 2 4)

  ;;psq-update, key missing
  ;;
  (check
      (let* ((Q  (make-psq key< priority<))
	     (Q  (psq-update Q "salut"
			     (lambda (P1)
			       (+ 1 P1))
			     9)))
	(psq-ref Q "salut"))
    => 10)

  ;;psq-min
  ;;
  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 2))
	     (Q (psq-set Q "hello" 1))
	     (Q (psq-set Q "ciao"  4))
	     (Q (psq-set Q "ohayo" 3)))
	(psq-min Q))
    => "hello")

  ;;psq-at-most
  ;;
  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 2))
	     (Q (psq-set Q "hello" 1))
	     (Q (psq-set Q "ciao"  4))
	     (Q (psq-set Q "ohayo" 3)))
	(psq-at-most Q 2))
    (=> equal?)
    '(("hello" . 1) ("salut" . 2)))

  ;;psq-at-most-range
  ;;
  (check
      (let* ((Q (make-psq key< priority<))
	     (Q (psq-set Q "salut" 1))
	     (Q (psq-set Q "hello" 2))
	     (Q (psq-set Q "ciao"  2))
	     (Q (psq-set Q "ohayo" 2)))
	(psq-at-most-range Q 2 "hello" "salut"))
    (=> equal?)
    '(("hello" . 2) ("ohayo" . 2) ("salut" . 1)))

  (values))


(parameterise ((check-test-name	'fingertrees))

  (pretty-print (make-fingertree 0 + (lambda (x) x)))


  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
