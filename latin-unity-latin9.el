;;; latin-unity-latin9.el --- Define language environment -*- coding: iso-2022-7 -*-

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

;; Provides the latin-9 language environment.

;;; Code:

;; define ISO-8859-15 for XEmacs 21.4 and earlier
;;;###autoload
(unless (find-charset 'latin-iso8859-15)
  ;; Create character set
  (make-charset
   'latin-iso8859-15 "ISO8859-15 (Latin 9)"
   '(short-name "Latin-9"
     long-name "ISO8859-15 (Latin 9)"
     registry "iso8859-15"
     dimension 1
     columns 1
     chars 96
     final ?b
     graphic 1
     direction l2r))
  ;; For syntax of Latin-9 characters.
  (require 'cl)
  (load "cl-macs" nil t)		; howcum no #'provide?
  (loop for c from 64 to 127		; from 'À' to 'ÿ'
    do (modify-syntax-entry (make-char 'latin-iso8859-15 c) "w"))
  (mapc (lambda (c)
	  (modify-syntax-entry (make-char 'latin-iso8859-15 c) "w"))
	'(#xA6 #xA8 #xB4 #xB8 #xBC #xBD #xBE))
  
  (modify-syntax-entry (make-char 'latin-iso8859-15 32) "w") ; no-break space
  (modify-syntax-entry (make-char 'latin-iso8859-15 87) "_") ; multiply
  (modify-syntax-entry (make-char 'latin-iso8859-15 119) "_") ; divide
  )

;;;###autoload
(unless (find-coding-system 'iso-8859-15)
  ;; Create coding system
  (make-coding-system
   'iso-8859-15 'iso2022 "MIME ISO-8859-15"
   '(charset-g0 ascii
     charset-g1 latin-iso8859-15
     charset-g2 t			; grrr
     charset-g3 t			; grrr
     mnemonic "MIME/Ltn-9")))

;;;###autoload
(unless (assoc "Latin-9" language-info-alist)
  (defun setup-latin9-environment ()
    "Set up multilingual environment (MULE) for European Latin-9 users."
    (interactive)
    (set-language-environment "Latin-9"))

  (set-language-info-alist
   "Latin-9"
   `((charset ascii latin-iso8859-15)
     (coding-system iso-8859-15)
     (coding-priority iso-8859-15)
     (input-method . "latin-9-prefix")
     (sample-text
      .
      ,(format
	"Hello, Hej, Tere, Hei, Bonjour, Gr%c%c Gott, Ciao, %cHola!, my %c0.02"
	(make-char 'latin-iso8859-15 #x7C)	; SMALL U WITH UMLAUT
	(make-char 'latin-iso8859-15 #x5F)	; GERMAN SHARP S
	(make-char 'latin-iso8859-15 #x21)	; INVERTED EXCLAMATION MARK
	(make-char 'latin-iso8859-15 #x24)	; EURO SIGN
	))
     (documentation . "\
This is a generic language environment for Latin-9 (ISO-8859-15).  It
supports the Euro and the following languages:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
We also have a German specific language environment \"German\"."))
   '("European")))

;; #### move these to a separate file for keysyms.

;;;###autoload
(unless (lookup-key global-map [EuroSign])
  (define-key global-map [EuroSign] #'self-insert-command))

;;;###autoload
(unless (get 'EuroSign 'ascii-character)
  (put 'EuroSign 'ascii-character (make-char 'latin-iso8859-15 #x24)))

(provide 'latin-unity-latin9)

;;; end of latin-unity-latin9.el
