;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Template
;;;Contents: main compilation unit
;;;Date: Mar 29, 2019
;;;
;;;Abstract
;;;
;;;	This is the main compilation unit; it USES all the other compilation units.
;;;
;;;	This compilation  units defines the main  module: it imports all  the modules
;;;	exporting  public syntactic  bindings  and it  reexports  all such  syntactic
;;;	bindings.
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

(declare (unit mmck.template)
	 (uses mmck.template.alpha)
	 (uses mmck.template.version)
	 (emit-import-library mmck.template))

(module (mmck.template)
    ()
  (import (only (chicken module) reexport))
  (reexport (mmck.template.alpha))
  (reexport (mmck.template.version))
  #| end of module |# )

;;; end of file
