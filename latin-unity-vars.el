;;; latin-unity-vars.el --- Common variables and objects of latin-unity

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 January 26
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

;;; Code:

(provide 'latin-unity-vars)

;; If you consider any of these bloat, you can comment them out, but
;; YOU MUST REGENERATE latin-unity-tables.el.  The table and the code
;; depend on arbitrarily chosen indexes for the charsets.

;; Latin-7 charset, ISO 8859/13 coding system, Latin-7 environment (Baltic Rim)
;; Few people will need this, but it avoids screwups.
(require 'latin-unity-latin7)

;; Latin-8 charset, ISO 8859/14 coding system, Latin-8 environment (Celtic)
;; Few people will need this, but it avoids screwups.
(require 'latin-unity-latin8)

;; Latin-9 charset, ISO 8859/15 coding system, Latin-9 environment (Euro)
(require 'latin-unity-latin9)

;; Latin-10 charset, ISO 8859/16 coding system, Latin-10 environment (Romanian)
;; Few people will need this, but it avoids screwups.
(require 'latin-unity-latin10)

;;; User customization is in latin-unity.el

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

(defcustom latin-unity-debug nil
  "If non-nil, make file write operations as slow as molasses.
If there were bugs, this might help find them, but there aren't. ;^)"
  :type 'boolean
  :group 'latin-unity)

(defvar latin-unity-help-buffer " *Coding system conflict*")

(defconst latin-unity-coding-systems
  (let (lucs)
    (mapc (lambda (x)
	    (when (find-coding-system x)
	      (setq lucs (cons x lucs))))
	  '(iso-8859-1 iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-9
	    iso-8859-10 iso-8859-13 iso-8859-14 iso-8859-15 iso-8859-16))
    (nreverse lucs))
  "List of coding systems \"unified\" by latin-unity.

Cf. `latin-unity-character-sets'.")

(defconst latin-unity-character-sets
  (let (lucs)
    (mapc (lambda (x)
	    (when (find-charset x)
	      (setq lucs (cons x lucs))))
	  '(latin-iso8859-1 latin-iso8859-2 latin-iso8859-3 latin-iso8859-4
	    latin-iso8859-9 latin-iso8859-10 latin-iso8859-13 latin-iso8859-14
	    latin-iso8859-15 latin-iso8859-16
	    ;; above are all GR sets, below are normally GL
	    ascii latin-jisx0201))
    (nreverse lucs))
  "List of character sets \"unified\" by latin-unity.

\"Unified\" is a misnomer, since actually these character sets are all
subsets of a larger set.  Characters which are identified by these
library are actually the same characters according to ISO 8859.  The
exception is the Japanese JIS X 0201 left half (JIS Roman), which is
controversial.  It will by default be identified with ASCII, but also
may take values elsewhere according to user preference.  (Unimplemented.)

The ISO 8859 character sets are actually Latin-1 to Latin-10, the right
halves of the ISO 8859 Latin character sets.

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
  (let (lucs)
    (mapc (lambda (x)
	    (when (find-coding-system (cdr x))
	      (setq lucs (cons x lucs))))
	  '((latin-iso8859-1  . iso-8859-1)
	    (latin-iso8859-2  . iso-8859-2)
	    (latin-iso8859-3  . iso-8859-3)
	    (latin-iso8859-4  . iso-8859-4)
	    (latin-iso8859-9  . iso-8859-9)
	    (latin-iso8859-10 . iso-8859-10)
	    (latin-iso8859-13 . iso-8859-13)
	    (latin-iso8859-14 . iso-8859-14)
	    (latin-iso8859-15 . iso-8859-15)
	    (latin-iso8859-16 . iso-8859-16)
	    ;; the following mappings are bogus, the RightThang not clear
	    (ascii            . iso-8859-1)	; any will do
	    (latin-jisx0201   . jisx0201)))	; doesn't currently exist
    (nreverse lucs))
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

;;; end of latin-unity-vars.
