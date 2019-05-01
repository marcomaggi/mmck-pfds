;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK PFDS
;;;Contents: main compilation unit
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This is the main compilation unit; it USES all the other compilation units.
;;;
;;;	This compilation  units defines the main  module: it imports all  the modules
;;;	exporting  public syntactic  bindings  and it  reexports  all such  syntactic
;;;	bindings.
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


;;;; units and module header

(declare (unit mmck.pfds)
	 (uses mmck.pfds.bbtrees)
	 (uses mmck.pfds.deques)
	 (uses mmck.pfds.dlists)
	 (uses mmck.pfds.fingertrees)
	 (uses mmck.pfds.heaps)
	 (uses mmck.pfds.psqs)
	 (uses mmck.pfds.queues)
	 (uses mmck.pfds.sequences)
	 (uses mmck.pfds.private.helpers)
	 (uses mmck.pfds.version)
	 (emit-import-library mmck.pfds))

(module (mmck.pfds)
    ()
  (import (only (chicken module) reexport))
  (reexport (only (mmck pfds private helpers)
		  make-pfds-assertion-violation
		  pfds-assertion-violation
		  pfds-assertion-violation?)
	    (mmck pfds bbtrees)
	    (mmck pfds deques)
	    (mmck pfds dlists)
	    (mmck pfds fingertrees)
	    (mmck pfds heaps)
	    (mmck pfds psqs)
	    (mmck pfds queues)
	    (mmck pfds sequences)
	    (mmck pfds version))
  #| end of module |# )

;;; end of file
