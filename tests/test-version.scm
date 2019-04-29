;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Template
;;;Contents: test program for version functions
;;;Date: Mar 29, 2019
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
;;;
;;;Copyright (C) 2019 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(require-library (mmck template))

(module (test-version)
    ()
  (import (scheme)
	  (mmck template)
	  (chicken pretty-print))


;;;; stuff

(pretty-print (list 'mmck-template-package-major-version	(mmck-template-package-major-version)))
(pretty-print (list 'mmck-template-package-minor-version	(mmck-template-package-minor-version)))
(pretty-print (list 'mmck-template-package-patch-level		(mmck-template-package-patch-level)))
(pretty-print (list 'mmck-template-package-prerelease-tag	(mmck-template-package-prerelease-tag)))
(pretty-print (list 'mmck-template-package-build-metadata	(mmck-template-package-build-metadata)))
(pretty-print (list 'mmck-template-package-version		(mmck-template-package-version)))
(pretty-print (list 'mmck-template-package-semantic-version	(mmck-template-package-semantic-version)))


;;;; done

#| end of module |# )

;;; end of file
