;;; latin-unity-vars.el --- Common variables and objects of latin-unity -*- coding: iso-2022-7 -*-

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 January 26
;; Last-modified: 2002 January 26

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

;; Provides the 'iso-8859-15 coding system if not yet defined.

;;; Code:

(provide 'latin-unity-vars)

;;; User customization is in latin-unity.el

;; define ISO-8859-15 for XEmacs 21.4 and earlier
;(eval-when (compile load eval)
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
  (load-library "cl-macs")		; howcum no #'provide?
  (loop for c from 64 to 127		; from ',b@(B' to ',b(B'
    do (modify-syntax-entry (make-char 'latin-iso8859-15 c) "w"))
  (mapc (lambda (c)
	  (modify-syntax-entry (make-char 'latin-iso8859-15 c) "w"))
	'(#xA6 #xA8 #xB4 #xB8 #xBC #xBD #xBE))
  
  (modify-syntax-entry (make-char 'latin-iso8859-15 32) "w") ; no-break space
  (modify-syntax-entry (make-char 'latin-iso8859-15 87) "_") ; multiply
  (modify-syntax-entry (make-char 'latin-iso8859-15 119) "_") ; divide
  )

(unless (find-coding-system 'iso-8859-15)
  ;; Create coding system
  (make-coding-system
   'iso-8859-15 'iso2022 "MIME ISO-8859-15"
   '(charset-g0 ascii
     charset-g1 latin-iso8859-15
     charset-g2 t			; grrr
     charset-g3 t			; grrr
     mnemonic "MIME/Ltn-9")))
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
	      ;; I'd like to append ", my ,b$(B0.02" to the following string,
	      ;; but can't due to a bug in escape-quoted support
	      ;; NB: convert the Latin-1 to Latin-9 when possible
	      . "Hello, Hej, Tere, Hei, Bonjour, Gr,b|_(B Gott, Ciao, ,b!(BHola!, my ,b$(B0.02")
	     (documentation . "\
This language environment is a generic one for Latin-9 (ISO-8859-15)
character set which supports the Euro and the following languages:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
We also have a German specific language environment \"German\"."))
 '("European"))
;)

;; latin-unity-equivalence-table
;; could speed things up a tiny bit by splitting out the bit-vector, but
;; this is constant-time (a char-table ref plus an aref)
(defvar latin-unity-equivalences (make-char-table 'generic)
  "Char-table of Latin character equivalence vectors.

Each vector takes integral elements (or nil, meaning void).  The zero-th
element is interpreted as the bit vector representation of the set of
character sets that can represent the character.  A nil value will cause
an error if accessed, so probably zero should be used instead.  The next
(length latin-unity-character-sets) are the mapping of the char-table
index to code points in the other character sets.  The last is the
Unicode code point.

Note that because this is a char-table, many characters will refer to
the same vector.  Thus whenever updating a character's value, you must
use `copy-sequence', or there will be side-effects.

The table is actually loaded from latin-unity-tables.el.")


;;; Variables

(defvar latin-unity-debug nil
  "If non-nil, make file write operations as slow as molasses.
If there were bugs, this might help find them, but there aren't. ;^)")

(defvar latin-unity-help-buffer " *Coding system conflict*")

(defconst latin-unity-coding-systems
  `(iso-8859-1 iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-9
    ;; if these coding systems are defined, uncomment for latin-unity support
    ;iso-8859-10			; unsupported in Mule
    ;iso-8859-13			; unsupported in Mule
    ;iso-8859-14			; unsupported in Mule
    iso-8859-15)
  "List of coding systems \"unified\" by latin-unity.

Cf. `latin-unity-character-sets'.")

(defconst latin-unity-character-sets
   (append '(latin-iso8859-1 latin-iso8859-2 latin-iso8859-3
	     latin-iso8859-4 latin-iso8859-9)
	   (when (memq 'iso-8859-10 latin-unity-coding-systems)
	     '(latin-iso8859-10))
	   (when (memq 'iso-8859-13 latin-unity-coding-systems)
	     '(latin-iso8859-13))
	   (when (memq 'iso-8859-14 latin-unity-coding-systems)
	     '(latin-iso8859-14))
	   '(latin-iso8859-15)
	   ;; above are all GR sets, below are normally GL
	   '(ascii latin-jisx0201))
  "List of character sets \"unified\" by latin-unity.

\"Unified\" is a misnomer, since actually these character sets are all
subsets of a larger set.  Characters which are identified by these
library are actually the same characters according to ISO 8859.  The
exception is the Japanese JIS X 0201 left half (JIS Roman), which is
controversial.  It will by default be identified with ASCII, but also
may take values elsewhere according to user preference.  (Unimplemented.)

The ISO 8859 character sets are actually Latin-1 to Latin-9, the right
halves of the ISO 8859.

ASCII and Unicode are treated implicitly.  All of the listed character
sets are the GR of a coded character set that supports ASCII, except
for JIS Roman.  Whether JIS Roman is considered to be identical to
ASCII, or a slight revision, depends on user preference.  Unicode is a
\"universal\" character set which is always a \"safe\" encoding for
streams that receive buffer contents.")

(defvar latin-unity-ascii-and-jis-roman "\000-\177"
  "skip-chars set defining ASCII characters also in JIS Roman.

#### Defaults to treating JIS Roman as identical to ASCII, not consistent
with the equivalence table.")

(defconst latin-unity-cset-codesys-alist
  (append '((latin-iso8859-1  . iso-8859-1)
	    (latin-iso8859-2  . iso-8859-2)
	    (latin-iso8859-3  . iso-8859-3)
	    (latin-iso8859-4  . iso-8859-4)
	    (latin-iso8859-9  . iso-8859-9))
	  (when (memq 'iso-8859-10 latin-unity-coding-systems)
	    '((latin-iso8859-10 . iso-8859-10)))
	  (when (memq 'iso-8859-10 latin-unity-coding-systems)
	    '((latin-iso8859-13 . iso-8859-13)))
	  (when (memq 'iso-8859-10 latin-unity-coding-systems)
	    '((latin-iso8859-14 . iso-8859-14)))
	  '((latin-iso8859-15 . iso-8859-15)
	    ;; the following mappings are bogus, the RightThang not clear
	    (ascii            . t)		; any will do
	    (latin-jisx0201   . iso2022)))	; need further information
  "Map Latin charsets to corresponding coding systems or classes.")

;; bit vectors for checking the feasible character sets

(defconst latin-unity-all-flags
  (lognot (lsh (lognot 0) (length latin-unity-character-sets)))
  "Bit vector representing the set of all Latin character sets.")

;; put the character set indicies and flag bits in reasonable places
(let ((index 1) (bit 1))
  (if (> (length latin-unity-character-sets) 25)
      (error "representation too small to support so many charsets!"))
  (mapcar (lambda (cs)
	    (put cs 'latin-unity-flag-bit bit)
	    (put cs 'latin-unity-index index)
	    (setq bit (lsh bit 1)
		  index (1+ index)))
	  latin-unity-character-sets))

;;; end of latin-unity-vars.el
