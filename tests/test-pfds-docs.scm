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
		receive)
	  (chicken pretty-print)
	  (mmck pfds)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing documentation examples\n")


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


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file
