;;; latin-unity-latin9.el --- Define language environment

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 March 7
;; Last-modified: 2003 August 9

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Mule bogusly considers the various ISO-8859 extended character sets
;; as disjoint, when ISO 8859 itself clearly considers them to be subsets
;; of a larger character set.  This library provides functions which
;; determine the list of coding systems which can encode all of the
;; characters in the buffer.

;; Provides the latin-9 language environment.

;;; Code:

;;; Actual functionality has been moved to the latin-euro-standards package,
;;; to prevent a circular dependency for Mule-UCS. This is a compatibility
;;; stub. -- Aidan Kehoe, Mon Feb 7 20:18:03 CET 2005

(require 'latin-euro-standards)

;; Check that we're functionally equivalent to the old latin-unity-latin9.el

;; The character set should exist.
(assert (charsetp (find-charset 'latin-iso8859-16))
	"`latin-euro-standards' didn't provide a Latin 9 character set!")

;; As should the coding system.
(assert (coding-system-p (find-coding-system 'iso-8859-15))
	"`latin-euro-standards' didn't provide a Latin 9 coding system!")
	
;; And we should have a language environment. 
(assert 
 (assoc "Latin-9" language-info-alist)
 "`latin-euro-standards' didn't provide a Latin 9 language environment!")

(provide 'latin-unity-latin9)

;;; end of latin-unity-latin9.el
