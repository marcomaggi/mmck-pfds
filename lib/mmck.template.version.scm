;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Template
;;;Contents: version functions
;;;Date: Fri Mar 29, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
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

(declare (unit mmck.template.version)
	 (emit-import-library mmck.template.version))

(module (mmck.template.version)
    (mmck-template-package-major-version
     mmck-template-package-minor-version
     mmck-template-package-patch-level
     mmck-template-package-prerelease-tag
     mmck-template-package-build-metadata
     mmck-template-package-version
     mmck-template-package-semantic-version)
  (import (scheme)
	  (prefix mmck.template.config config::))


;;;; version functions

(define (mmck-template-package-major-version)		config::MMUX_PKG_MAJOR_VERSION)
(define (mmck-template-package-minor-version)		config::MMUX_PKG_MINOR_VERSION)
(define (mmck-template-package-patch-level)		config::MMUX_PKG_PATCH_LEVEL)
(define (mmck-template-package-prerelease-tag)		config::MMUX_PKG_PRERELEASE_TAG)
(define (mmck-template-package-build-metadata)		config::MMUX_PKG_BUILD_METADATA)
(define (mmck-template-package-version)			config::MMUX_PKG_VERSION)
(define (mmck-template-package-semantic-version)	config::MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of module |# )

;;; end of file
