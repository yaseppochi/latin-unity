;;; latin-unity-latin8.el --- Define language environment

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 October 24
;; Last-Modified: 2005 February 7

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

;; Provides the latin-8 language environment.

;;; Code:

;;; Actual functionality has been moved to the latin-euro-standards package,
;;; to prevent a circular dependency for Mule-UCS. This is a compatibility
;;; stub. -- Aidan Kehoe, Mon Feb 7 20:18:03 CET 2005

(require 'latin-euro-standards)

;; Check that we're functionally equivalent to the old latin-unity-latin8.el

;; The character set should exist.
(assert (charsetp (find-charset 'latin-iso8859-14)) t
	"`latin-euro-standards' didn't provide a Latin 8 character set!")

;; As should the coding system.
(assert (coding-system-p (find-coding-system 'iso-8859-14)) t
	"`latin-euro-standards' didn't provide a Latin 8 coding system!")
	
;; And we should have a language environment. 
(assert 
 (assoc "Latin-8" language-info-alist) t
 "`latin-euro-standards' didn't provide a Latin 8 language environment!")

(provide 'latin-unity-latin8)

;;; end of latin-unity-latin8.el
