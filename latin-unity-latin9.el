;;; latin-unity-latin9.el --- Define language environment -*- coding: iso-2022-7 -*-

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 March 7
;; Last-modified: 2002 March 7

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

;; The 'iso-8859-15 coding system must be defined before this file is loaded
;; in XEmacsen before 21.4.7 and 21.5.6.  No, it can't be required here, it's
;; already too late.

;;; Code:

(unless (assoc "Latin-9" language-info-alist)
  (defun setup-latin9-environment ()
    "Set up multilingual environment (MULE) for European Latin-9 users."
    (interactive)
    (set-language-environment "Latin-9"))

  (set-language-info-alist
   "Latin-9" '((charset ascii latin-iso8859-15)
	       (coding-system iso-8859-15)
	       (coding-priority iso-8859-15)
	       (input-method . "latin-9-prefix")
	       (sample-text
		. "Hello, Hej, Tere, Hei, Bonjour, Grüß Gott, Ciao, ¡Hola!, my ¤0.02")
	       (documentation . "\
This language environment is a generic one for Latin-9 (ISO-8859-15)
character set which supports the Euro and the following languages:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
We also have a German specific language environment \"German\"."))
   '("European")))

;; bind the EuroSign keysym
(define-key global-map [EuroSign] #'self-insert-command)
(put 'EuroSign 'ascii-character (make-char 'latin-iso8859-15 #x24))

(provide 'latin-unity-latin9)

;;; end of latin-unity-latin9.el
