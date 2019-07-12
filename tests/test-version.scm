;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Pfds
;;;Contents: test program for version functions
;;;Date: Apr 29, 2019
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
;;;
;;;Copyright (c) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;Copyright (c) 2011 Ian Price <ianprice90@googlemail.com>
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

(require-library (mmck pfds))

(module (test-version)
    ()
  (import (scheme)
	  (mmck pfds)
	  (chicken pretty-print))


;;;; stuff

(pretty-print (list 'mmck-pfds-package-major-version	(mmck-pfds-package-major-version)))
(pretty-print (list 'mmck-pfds-package-minor-version	(mmck-pfds-package-minor-version)))
(pretty-print (list 'mmck-pfds-package-patch-level	(mmck-pfds-package-patch-level)))
(pretty-print (list 'mmck-pfds-package-prerelease-tag	(mmck-pfds-package-prerelease-tag)))
(pretty-print (list 'mmck-pfds-package-build-metadata	(mmck-pfds-package-build-metadata)))
(pretty-print (list 'mmck-pfds-package-version		(mmck-pfds-package-version)))
(pretty-print (list 'mmck-pfds-package-semantic-version	(mmck-pfds-package-semantic-version)))


;;;; done

#| end of module |# )

;;; end of file
