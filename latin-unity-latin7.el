;;; latin-unity-latin7.el --- Define language environment -*- coding: iso-2022-7 -*-

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 March 7
;; Last-modified: 2002 March 23

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

;; Provides the latin-7 language environment.

;;; Code:

;; define ISO-8859-13 for XEmacs 21.4 and earlier
; don't ;;;###autoload
(unless (find-charset 'latin-iso8859-13)
  ;; Create character set
  (make-charset
   'latin-iso8859-13 "ISO8859-13 (Latin 7)"
   ;; sheesh, what we do for backward compatibility
   ;; #### this test and the similar one below probably should be
   ;; reformulated to use condition-case
   (append (if (emacs-version>= 21 4)
	       '(short-name "Latin-7"
		 long-name "ISO8859-13 (Latin 7)")
	     nil)
	   '(registry "iso8859-13"
	     dimension 1
	     columns 1
	     chars 96
	     final ?Y
	     graphic 1
	     direction l2r)))
  ;; For syntax of Latin-7 characters.
  (require 'cl)
  (load "cl-macs" nil t)		; howcum no #'provide?
  (loop for c from 64 to 127		; from 'À' to 'ÿ'
    do (modify-syntax-entry (make-char 'latin-iso8859-13 c) "w"))
  (mapc (lambda (c)
	  (modify-syntax-entry (make-char 'latin-iso8859-13 c) "w"))
	'(#xA8 #xAA #xAF #xB8 #xBA #xBF))
  
  (modify-syntax-entry (make-char 'latin-iso8859-13 32) "w") ; no-break space
  (modify-syntax-entry (make-char 'latin-iso8859-13 87) "_") ; multiply
  (modify-syntax-entry (make-char 'latin-iso8859-13 119) "_") ; divide
  (modify-syntax-entry (make-char 'latin-iso8859-13 127) ".") ; right squote
  )

(defvar iso8859/13-case-table
  (let ((table (copy-case-table (standard-case-table))))
    (mapc (lambda (pair)
	    (put-case-table-pair (make-char 'latin-iso8859-13 (car pair))
				 (make-char 'latin-iso8859-13 (cdr pair))
				 table))
	  '((#xA8 . #xB8) (#xAA . #xBA) (#xAF . #xBF)))
    (let ((i #xC0))
      (while (< i #xDF)
	(unless (= i #xD7)
	    (put-case-table-pair (make-char 'latin-iso8859-13 i)
				 (make-char 'latin-iso8859-13 (+ i #x20))
				 table))
	(setq i (1+ i))))	  
    table)
  "Case table for Latin 7, right half of ISO 8859/13.")

; don't ;;;###autoload
(unless (find-coding-system 'iso-8859-13)
  ;; Create coding system
  (make-coding-system
   'iso-8859-13 'iso2022 "MIME ISO-8859-13"
   '(charset-g0 ascii
     charset-g1 latin-iso8859-13
     charset-g2 t			; grrr
     charset-g3 t			; grrr
     mnemonic "MIME/Ltn-7")))

; don't ;;;###autoload
(unless (assoc "Latin-7" language-info-alist)
  (defun setup-latin7-environment ()
    "Set up multilingual environment (MULE) for Baltic Rim Latin-7 users."
    (interactive)
    (set-language-environment "Latin-7"))

  ;; sheesh, what we do for backward compatibility
  (apply #'set-language-info-alist
	 (append `("Latin-7"
		   ((charset ascii latin-iso8859-13)
		    (coding-system iso-8859-13)
		    (coding-priority iso-8859-13)
		    (input-method . "latin-7-prefix")
		    (sample-text
		     .
		     ,(format "\
Hello, Hej, Tere, Hei, Bonjour, Gr%c%c Gott, Ciao, %cHola!"
			      ;; SMALL U WITH UMLAUT
			      (make-char 'latin-iso8859-13 #x7C)
			      ;; GERMAN SHARP S
			      (make-char 'latin-iso8859-13 #x5F)
			      ;; INVERTED EXCLAMATION MARK
			      (make-char 'latin-iso8859-13 #x21)))
		    (documentation . "\
This is a generic language environment for Latin-7 (ISO-8859-13).  It
supports the Baltic Rim languages.")))
		 (if (emacs-version>= 21 1 15)
		     '(("Baltic Rim"))
		   nil))))

(provide 'latin-unity-latin7)

;;; end of latin-unity-latin7.el
