;;; latin-unity-latin8.el --- Define language environment

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 October 24

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

;; define ISO-8859-14 for XEmacs 21.4 and earlier
;;;###autoload
(unless (find-charset 'latin-iso8859-14)
  ;; Create character set
  (make-charset
   'latin-iso8859-14 "ISO8859-14 (Latin 8)"
   ;; sheesh, what we do for backward compatibility
   ;; #### this test and the similar one below probably should be
   ;; reformulated to use condition-case
   (append (if (emacs-version>= 21 4)
	       '(short-name "Latin-8"
		 long-name "ISO8859-16 (Latin 8)")
	     nil)
	   '(registry "iso8859-14"
	     dimension 1
	     columns 1
	     chars 96
	     final ?_			; octet 05/15; cf ISO-IR 199
	     graphic 1
	     direction l2r)))
  ;; For syntax of Latin-8 characters.
  (require 'cl)
  (load "cl-macs" nil t)
  (loop for c from 32 to 127
    do (modify-syntax-entry (make-char 'latin-iso8859-14 c) "w"))
  (mapc (lambda (c)
	  (modify-syntax-entry (make-char 'latin-iso8859-14 c) "_"))
	'(#xA3 #xA7 #xA9 #xAD #xAE #xB6))
  )

;;;###autoload
(unless (find-coding-system 'iso-8859-14)
  ;; Create coding system
  (make-coding-system
   'iso-8859-14 'iso2022 "MIME ISO-8859-14"
   '(charset-g0 ascii
     charset-g1 latin-iso8859-14
     charset-g2 t			; grrr
     charset-g3 t			; grrr
     mnemonic "MIME/Ltn-8")))

;;;###autoload
(unless (assoc "Latin-8" language-info-alist)
  (defun setup-latin8-environment ()
    "Set up multilingual environment (MULE) for European Latin-8 users."
    (interactive)
    (set-language-environment "Latin-8"))

  ;; sheesh, what we do for backward compatibility
  (apply #'set-language-info-alist
	 (append `("Latin-8"
		   ((charset ascii latin-iso8859-14)
		    (coding-system iso-8859-14)
		    (coding-priority iso-8859-14)
		    (input-method . "latin-8-prefix")
		    (sample-text
		     .
		     ;; #### OK, who knows some Gaelic?
		     ,(format "\
Hey, if you know a Celtic language, send sample encoded text (eg, using
\"dot-above\" characters) to xemacs-beta@xemacs.org.  Please use a MIME
application/octet-stream attachment if possible."
			      ))
		    (documentation . "\
This is a generic language environment for Latin-8 (ISO-8859-14).  It
supports Celtic and most Western European languages.")))
		 (if (emacs-version>= 21 1 15)
		     '(("European"))
		   nil))))

(provide 'latin-unity-latin8)

;;; end of latin-unity-latin8.el
