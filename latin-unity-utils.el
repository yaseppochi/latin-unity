;;; latin-unity-utils.el --- Utility functions for preparing latin-unity data

;; Copyright (C) 2002 Free Software Foundation, Inc.

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
;; of a larger character set.

;; This library provides functions which for creating databases of
;; equivalence classes of characters.

;; It is NOT REQUIRED for the use of latin-unity.el; only for creating
;; the data it uses (provided in latin-unity-tables.el).

;; This is a developer-only library; _do not_ autoload anything in it.

;;; Code:

(provide 'latin-unity-utils)
(provide 'latin-unity-tables)	; Not a lie.

;;; Requires
(require 'cl)
(load "cl-macs" nil t)			; howcum no #'provide?
;; Get the charsets, among other things
(require 'latin-unity-vars)

(if (fboundp 'character-to-unicode)
    ;; #### untested
    (fset 'char-to-ucs 'character-to-unicode)
  ;; the following libraries are from Mule-UCS.
  ;; this dependency can be eliminated by providing char-to-ucs.
  (require 'mule-ucs-unicode "unicode")
  (require 'un-define))

;; table of character set support for each Unicode code point
;; Table from http://www.cs.tut.fi/~jkorpela/iso8859/charsupp.htm8
;(defconst latin-unity-character-sets-vector
;  (let ((vec (make-vector (1+ #x20AC) 0)))
;    (flet ((loopset (start end value)
;	     (loop for i from start to end do
;	       (aset vec i value)))
;	   (mapsetbit (cs char-list)
;	     (let ((bit (get cs 'latin-unity-flag-bit)))
;	       (mapcar (lambda (i)
;			 (aset vec i (logior bit (aref vec i))))
;		       char-list)))
;	   (mapclearbit (cs char-list)
;	     (let ((bit (get cs 'latin-unity-flag-bit)))
;	       (if (null bit) (error (format "no flag bit for %s" cs)))
;	       (mapcar (lambda (i)
;			 (aset vec i (logand (lognot bit) (aref vec i))))
;		       char-list))))
;      (loopset #x0 #x7F
;	       (logior (get 'ascii 'latin-unity-flag-bit)
;		       (get 'latin-jisx0201 'latin-unity-flag-bit)))
;      ;; C1 is handled in the definition of the char-table.
;      (loopset #xA0 #xFF
;	       (logior (get 'latin-iso8859-1 'latin-unity-flag-bit)
;		       (get 'latin-iso8859-9 'latin-unity-flag-bit)
;		       (if (memq 'iso-8859-14 latin-unity-coding-systems)
;			   (get 'latin-iso8859-14 'latin-unity-flag-bit)
;			 0)
;		       (get 'latin-iso8859-15 'latin-unity-flag-bit)))
;      (mapsetbit 'latin-iso8859-2
;	'(#xA0 #xA4 #xA7 #xA8 #xAD #xB0 #xB4 #xB8 #xC1 #xC2 #xC4
;	  #xC7 #xC9 #xCB #xCD #xCE #xD3 #xD4 #xD6 #xD7 #xDA
;	  #xDC #xDD #xDF #xE1 #xE2 #xE4 #xE7 #xE9 #xEB #xED
;	  #xF3 #xF4 #xF6 #xF7 #xFA #xFC #xFD
;	  #x0102 #x0103 #x0104 #x0105 #x0106
;	  #x0107 #x010C #x010D #x010E #x010F
;	  #x0110 #x0111 #x0118 #x0119 #x011A
;	  #x011B #x0139 #x013A #x013D #x013E
;	  #x0141 #x0142 #x0143 #x0144 #x0147
;	  #x0148 #x0150 #x0151 #x0154 #x0155
;	  #x0158 #x0159 #x015A #x015B #x015E
;	  #x015F #x0160 #x0161 #x0162 #x0163
;	  #x0164 #x0165 #x016E #x016F #x0170
;	  #x0171 #x0179 #x017A #x017B #x017C
;	  #x017D #x017E #x02C7 #x02D8 #x02D9
;	  #x02DB #x02DD))
;      (mapsetbit 'latin-iso8859-3
;	'(#xA0 #xA3 #xA4 #xA7 #xA8 #xAD #xB0 #xB2 #xB3 #xB4 #xB5
;	  #xB7 #xB8 #xBD #xC0 #xC1 #xC2 #xC4 #xC7 #xC8 #xC9
;	  #xCA #xCB #xCC #xCD #xCE #xCF #xD1 #xD2 #xD3 #xD4
;	  #xD6 #xD7
;	  #xD9 #xDA #xDB #xDC #xDF #xE0 #xE1 #xE2 #xE4 #xE7
;	  #xE8 #xE9 #xEA #xEB #xEC #xED #xEE #xEF #xF1 #xF2
;	  #xF3 #xF4 #xF6 #xF7 #xF9 #xFA #xFB #xFC
;	  #x0108 #x0109 #x010A #x010B #x011C
;	  #x011D #x011E #x011F #x0120 #x0121
;	  #x0124 #x0125 #x0126 #x0127 #x0130
;	  #x0131 #x0134 #x0135 #x015C #x015D
;	  #x015E #x015F #x016C #x016D #x017B
;	  #x017C #x02D8 #x02D9))
;      (mapsetbit 'latin-iso8859-4
;	'(#xA0 #xA4 #xA7 #xA8 #xAD #xAF #xB0 #xB4 #xB8 #xC1 #xC2
;	  #xC3 #xC4 #xC5 #xC6 #xC9 #xCB #xCD #xCE #xD4 #xD5
;	  #xD6 #xD7 #xD8 #xDA #xDB #xDC #xDF #xE1 #xE2 #xE3
;	  #xE4 #xE5 #xE6 #xE9 #xEB #xED #xEE #xF4 #xF5 #xF6
;	  #xF7 #xF8 #xFA #xFB #xFC
;	  #x0100 #x0101 #x0104 #x0105 #x010C
;	  #x010D #x0110 #x0111 #x0112 #x0113
;	  #x0116 #x0117 #x0118 #x0119 #x0122
;	  #x0123 #x0128 #x0129 #x012A #x012B
;	  #x012E #x012F #x0136 #x0137 #x0138
;	  #x013B #x013C #x0145 #x0146 #x014A
;	  #x014B #x014C #x014D #x0156 #x0157
;	  #x0160 #x0161 #x0166 #x0167 #x0168
;	  #x0169 #x016A #x016B #x0172 #x0173
;	  #x017D #x017E #x02C7 #x02D9 #x02DB))
;      (mapclearbit 'latin-iso8859-9
;	'(#xD0 #xDD #xDE #xF0 #xFD #xFE))
;      (mapsetbit 'latin-iso8859-9
;	'(#x011E #x011F #x0130 #x0131 #x015E
;	  #x015F))
;      ;; Unsupported in current versions of Mule
;      (when (memq 'iso-8859-10 latin-unity-coding-systems)
;	(mapsetbit 'latin-iso8859-10
;	  '(#xA0 #xA7 #xAD #xB0 #xB4 #xB7 #xC1 #xC2 #xC3 #xC4 #xC5
;	    #xC6 #xCB #xCD #xCE #xCF #xD0 #xD3 #xD4 #xD5 #xD6
;	    #xD8 #xDA #xDB #xDC #xDD #xDE #xDF #xE1 #xE2 #xE3
;	    #xE4 #xE5 #xE6 #xE9 #xEB #xED #xEE #xEF #xF0 #xF3
;	    #xF4 #xF5 #xF6 #xF8 #xFA #xFB #xFC #xFD #xFE 
;	    #x0100 #x0101 #x0104 #x0105 #x010C
;	    #x010D #x0110 #x0112 #x0113 #x0116
;	    #x0117 #x0118 #x0119 #x0122 #x0123
;	    #x0128 #x0129 #x012A #x012B #x012E
;	    #x012F #x0136 #x0137 #x0138 #x013B
;	    #x013C #x0145 #x0146 #x014A #x014B
;	    #x014C #x014D #x0160 #x0161 #x0166
;	    #x0167 #x0168 #x0169 #x016A #x016B
;	    #x0172 #x0173 #x017D #x017E #x2015)))
;      (when (memq 'iso-8859-13 latin-unity-coding-systems)
;	(mapsetbit 'latin-iso8859-13
;	  '(#xA0 #xA2 #xA3 #xA4 #xA6 #xA7 #xA9 #xAB #xAC #xAD #xAE
;	    #xB0 #xB1 #xB2 #xB3 #xB5 #xB6 #xB7 #xB9 #xBB #xBC
;	    #xBD #xBE #xC4 #xC5 #xC6 #xC9 #xD3 #xD5 #xD6 #xD7
;	    #xD8 #xDC #xDF #xE4 #xE5 #xE6 #xE9 #xF3 #xF5 #xF6
;	    #xF8 #xFC
;	    #x0100 #x0101 #x0104 #x0105 #x0106
;	    #x0107 #x010C #x010D #x0112 #x0113
;	    #x0116 #x0117 #x0118 #x0119 #x0122
;	    #x0123 #x012A #x012B #x012E #x012F
;	    #x0136 #x0137 #x013B #x013C #x0141
;	    #x0142 #x0143 #x0144 #x0145 #x0146
;	    #x014C #x014D #x0156 #x0157 #x015A
;	    #x015B #x0160 #x0161 #x016A #x016B
;	    #x0172 #x0173 #x017D #x017E #x2019
;	    #x201C #x201D #x201E)))
;	(when (memq 'iso-8859-14 latin-unity-coding-systems)
;	  (mapclearbit 'latin-iso8859-14
;	    '(#xA1 #xA2 #xA4 #xA5 #xA6 #xA8 #xAA #xAB #xAC #xAF
;	      #xB0 #xB1 #xB2 #xB3 #xB4 #xB5 #xB7 #xB8 #xB9 #xBA
;	      #xBB #xBC #xBD #xBE #xBF #xD0 #xD7 #xDE #xF0 #xF7
;	      #xFE))
;	  (mapsetbit 'latin-iso8859-14
;	    '(#x010A #x010B #x0120 #x0121 #x0174
;	      #x0175 #x0176 #x0177 #x0178 #x1E02
;	      #x1E03 #x1E0A #x1A0B #x1E1E #x1E1F
;	      #x1E40 #x1E41 #x1E56 #x1E57 #x1E60
;	      #x1E61 #x1E6A #x1E6B #x1E80 #x1E81
;	      #x1E82 #x1E83 #x1E84 #x1E85 #x1EF2
;	      #x1EF3)))
;	(mapclearbit 'latin-iso8859-15
;	  '(#xA4 #xA6 #xA8 #xB4 #xB8 #xBC #xBD #xBE))
;	(mapsetbit 'latin-iso8859-15
;	  '(#x0152 #x0153 #x0160 #x0161 #x0178
;	    #x017C #x017D #x20AC)))
;    vec))

;; Populate the equivalence table
(let* ((u+index (1+ (length latin-unity-character-sets))) ; alias
       (zero (make-vector (1+ u+index) nil))              ; useful constant
       ;; temporary holding tank for equivs: list of Mule characters
       ;; equivalent to the Unicode code point
       (unitable (make-vector (1+ #x20AC) nil)))

  ;; 
  ;; ASCII is spatial, Mule treats C0, SPC, and DEL as ASCII, but
  ;; (= (charset-property 'ascii 'chars) 94) :-(
  (loop for i from #x00 to #x7F do
    (let* ((ch (make-char 'ascii i))	; multibyte dirty
	   (ucs (char-to-ucs ch)))
      (if ucs
	  (aset unitable ucs (cons ch (aref unitable ucs)))
	;; Unfortunately it seems that Mule-UCS doesn't know Latin-9....
	;; It also is smart enough to know that there are holes in Latin-3.
	(message "Mule-UCS doesn't know about %s" (split-char ch)))))

  ;; Other character sets
  ;; Control-1 is spatial, but handled below
  ;; NB: JIS Roman defaults to differing from ASCII
  (mapc (lambda (cs)
	  (let (lo hi)
	    ;; cond because Morioka added a lot of extra sizes
	    ;; not relevant to our Latin character sets
	    (message "Processing charset %s ..." cs)
	    (cond ((= (charset-property cs 'chars) 94)
		   (setq lo #x21 hi #x7E))
		  ((= (charset-property cs 'chars) 96)
		   (setq lo #x20 hi #x7F))
		  (t (message "Odd size character set (%s)!" cs)
		     (setq lo #x20 hi #x7F)))
	    (loop for i from lo to hi do
	      (let* ((ch (make-char cs i)) ; multibyte dirty
		     (ucs (char-to-ucs ch)))
		(if ucs
		    (aset unitable ucs (cons ch (aref unitable ucs)))
		  (message "Mule-UCS doesn't know about %s"
			   (split-char ch)))))))
	(set-difference (copy-sequence latin-unity-character-sets)
			'(ascii latin-iso8859-13 latin-iso8859-14
			  latin-iso8859-15 latin-iso8859-16)))

  ;; Latin-7, -8, -9, and -10 are spatial, Mule-UCS doesn't handle them
  ;; correctly (maybe because they're not built in?)
  ;; Latin-7 (ISO 8859/13)
  (when (find-coding-system 'iso-8859-13)
    (message "Processing charset %s ..." 'iso-8859-13)
    (mapc (lambda (pair)
	    (let ((ucs (cdr pair))
		  (ch (make-char 'latin-iso8859-13 (car pair))))
	      (aset unitable ucs (cons ch (aref unitable ucs)))))
	  '((#xA0 . #x00A0) (#xA1 . #x201D) (#xA2 . #x00A2) (#xA3 . #x00A3)
	    (#xA4 . #x00A4) (#xA5 . #x201E) (#xA6 . #x00A6) (#xA7 . #x00A7)
	    (#xA8 . #x00D8) (#xA9 . #x00A9) (#xAA . #x0156) (#xAB . #x00AB)
	    (#xAC . #x00AC) (#xAD . #x00AD) (#xAE . #x00AE) (#xAF . #x00C6)
	    (#xB0 . #x00B0) (#xB1 . #x00B1) (#xB2 . #x00B2) (#xB3 . #x00B3)
	    (#xB4 . #x201C) (#xB5 . #x00B5) (#xB6 . #x00B6) (#xB7 . #x00B7)
	    (#xB8 . #x00F8) (#xB9 . #x00B9) (#xBA . #x0157) (#xBB . #x00BB)
	    (#xBC . #x00BC) (#xBD . #x00BD) (#xBE . #x00BE) (#xBF . #x00E6)
	    (#xC0 . #x0104) (#xC1 . #x012E) (#xC2 . #x0100) (#xC3 . #x0106)
	    (#xC4 . #x00C4) (#xC5 . #x00C5) (#xC6 . #x0118) (#xC7 . #x0112)
	    (#xC8 . #x010C) (#xC9 . #x00C9) (#xCA . #x0179) (#xCB . #x0116)
	    (#xCC . #x0122) (#xCD . #x0136) (#xCE . #x012A) (#xCF . #x013B)
	    (#xD0 . #x0160) (#xD1 . #x0143) (#xD2 . #x0145) (#xD3 . #x00D3)
	    (#xD4 . #x014C) (#xD5 . #x00D5) (#xD6 . #x00D6) (#xD7 . #x00D7)
	    (#xD8 . #x0172) (#xD9 . #x0141) (#xDA . #x015A) (#xDB . #x016A)
	    (#xDC . #x00DC) (#xDD . #x017B) (#xDE . #x017D) (#xDF . #x00DF)
	    (#xE0 . #x0105) (#xE1 . #x012F) (#xE2 . #x0101) (#xE3 . #x0107)
	    (#xE4 . #x00E4) (#xE5 . #x00E5) (#xE6 . #x0119) (#xE7 . #x0113)
	    (#xE8 . #x010D) (#xE9 . #x00E9) (#xEA . #x017A) (#xEB . #x0117)
	    (#xEC . #x0123) (#xED . #x0137) (#xEE . #x012B) (#xEF . #x013C)
	    (#xF0 . #x0161) (#xF1 . #x0144) (#xF2 . #x0146) (#xF3 . #x00F3)
	    (#xF4 . #x014D) (#xF5 . #x00F5) (#xF6 . #x00F6) (#xF7 . #x00F7)
	    (#xF8 . #x0173) (#xF9 . #x0142) (#xFA . #x015B) (#xFB . #x016B)
	    (#xFC . #x00FC) (#xFD . #x017C) (#xFE . #x017E) (#xFF . #x2019))))
  (when (find-coding-system 'iso-8859-14)
    (message "Processing charset %s ..." 'iso-8859-14)
    (mapc (lambda (pair)
	    (let ((ucs (cdr pair))
		  (ch (make-char 'latin-iso8859-14 (car pair))))
	      (aset unitable ucs (cons ch (aref unitable ucs)))))
	  '((#xA0 . #x00A0) (#xA1 . #x1E02) (#xA2 . #x1E03) (#xA3 . #x00A3)
	    (#xA4 . #x010A) (#xA5 . #x010B) (#xA6 . #x1E0A) (#xA7 . #x00A7)
	    (#xA8 . #x1E80) (#xA9 . #x00A9) (#xAA . #x1E82) (#xAB . #x1E0B)
	    (#xAC . #x1EF2) (#xAD . #x00AD) (#xAE . #x00AE) (#xAF . #x0178)
	    (#xB0 . #x1E1E) (#xB1 . #x1E1F) (#xB2 . #x0120) (#xB3 . #x0121)
	    (#xB4 . #x1E40) (#xB5 . #x1E41) (#xB6 . #x00B6) (#xB7 . #x1E56)
	    (#xB8 . #x1E81) (#xB9 . #x1E57) (#xBA . #x1E83) (#xBB . #x1E60)
	    (#xBC . #x1EF3) (#xBD . #x1E84) (#xBE . #x1E85) (#xBF . #x1E61)
	    (#xC0 . #x00C0) (#xC1 . #x00C1) (#xC2 . #x00C2) (#xC3 . #x00C3)
	    (#xC4 . #x00C4) (#xC5 . #x00C5) (#xC6 . #x00C6) (#xC7 . #x00C7)
	    (#xC8 . #x00C8) (#xC9 . #x00C9) (#xCA . #x00CA) (#xCB . #x00CB)
	    (#xCC . #x00CC) (#xCD . #x00CD) (#xCE . #x00CE) (#xCF . #x00CF)
	    (#xD0 . #x0174) (#xD1 . #x00D1) (#xD2 . #x00D2) (#xD3 . #x00D3)
	    (#xD4 . #x00D4) (#xD5 . #x00D5) (#xD6 . #x00D6) (#xD7 . #x1E6A)
	    (#xD8 . #x00D8) (#xD9 . #x00D9) (#xDA . #x00DA) (#xDB . #x00DB)
	    (#xDC . #x00DC) (#xDD . #x00DD) (#xDE . #x0176) (#xDF . #x00DF)
	    (#xE0 . #x00E0) (#xE1 . #x00E1) (#xE2 . #x00E2) (#xE3 . #x00E3)
	    (#xE4 . #x00E4) (#xE5 . #x00E5) (#xE6 . #x00E6) (#xE7 . #x00E7)
	    (#xE8 . #x00E8) (#xE9 . #x00E9) (#xEA . #x00EA) (#xEB . #x00EB)
	    (#xEC . #x00EC) (#xED . #x00ED) (#xEE . #x00EE) (#xEF . #x00EF)
	    (#xF0 . #x0175) (#xF1 . #x00F1) (#xF2 . #x00F2) (#xF3 . #x00F3)
	    (#xF4 . #x00F4) (#xF5 . #x00F5) (#xF6 . #x00F6) (#xF7 . #x1E6B)
	    (#xF8 . #x00F8) (#xF9 . #x00F9) (#xFA . #x00FA) (#xFB . #x00FB)
	    (#xFC . #x00FC) (#xFD . #x00FD) (#xFE . #x0177) (#xFF . #x00FF))))
  (when (find-coding-system 'iso-8859-15)
    (message "Processing charset %s ..." 'iso-8859-15)
    (loop for i from #x20 to #x7F do
      (let* ((ch (make-char 'latin-iso8859-15 i)) ; multibyte dirty
	     (ucs (+ i #x80)))
	(aset unitable ucs (cons ch (aref unitable ucs)))))
    (mapc (lambda (ucs)
	    (let ((ch (make-char 'latin-iso8859-15 ucs)))
	      (aset unitable ucs (delq ch (aref unitable ucs)))))
	  '(#xA4 #xA6 #xA8 #xB4 #xB8 #xBC #xBD #xBE))
    (mapc (lambda (pair)
	    (let ((ucs (car pair))
		  (ch (make-char 'latin-iso8859-15 (cdr pair))))
	      (aset unitable ucs (cons ch (aref unitable ucs)))))
	  '((#x0152 . #xBC) (#x0153 . #xBD) (#x0160 . #xA6) (#x0161 . #xA8)
	    (#x0178 . #xBE) (#x017D . #xB4) (#x017E . #xB8) (#x20AC . #xA4))))
  (when (find-coding-system 'iso-8859-16)
    (mapc (lambda (pair)
	    (let ((ucs (cdr pair))
		  (ch (make-char 'latin-iso8859-16 (car pair))))
	      (aset unitable ucs (cons ch (aref unitable ucs)))))
	  '((#xA0 . #x00A0) (#xA1 . #x0104) (#xA2 . #x0105) (#xA3 . #x0141)
	    (#xA4 . #x20AC) (#xA5 . #x201E) (#xA6 . #x0160) (#xA7 . #x00A7)
	    (#xA8 . #x0161) (#xA9 . #x00A9) (#xAA . #x0218) (#xAB . #x00AB)
	    (#xAC . #x0179) (#xAD . #x00AD) (#xAE . #x017A) (#xAF . #x017B)
	    (#xB0 . #x00B0) (#xB1 . #x00B1) (#xB2 . #x010C) (#xB3 . #x0142)
	    (#xB4 . #x017D) (#xB5 . #x201D) (#xB6 . #x00B6) (#xB7 . #x00B7)
	    (#xB8 . #x017E) (#xB9 . #x010D) (#xBA . #x0219) (#xBB . #x00BB)
	    (#xBC . #x0152) (#xBD . #x0153) (#xBE . #x0178) (#xBF . #x017C)
	    (#xC0 . #x00C0) (#xC1 . #x00C1) (#xC2 . #x00C2) (#xC3 . #x0102)
	    (#xC4 . #x00C4) (#xC5 . #x0106) (#xC6 . #x00C6) (#xC7 . #x00C7)
	    (#xC8 . #x00C8) (#xC9 . #x00C9) (#xCA . #x00CA) (#xCB . #x00CB)
	    (#xCC . #x00CC) (#xCD . #x00CD) (#xCE . #x00CE) (#xCF . #x00CF)
	    (#xD0 . #x0110) (#xD1 . #x0143) (#xD2 . #x00D2) (#xD3 . #x00D3)
	    (#xD4 . #x00D4) (#xD5 . #x0150) (#xD6 . #x00D6) (#xD7 . #x015A)
	    (#xD8 . #x0170) (#xD9 . #x00D9) (#xDA . #x00DA) (#xDB . #x00DB)
	    (#xDC . #x00DC) (#xDD . #x0118) (#xDE . #x021A) (#xDF . #x00DF)
	    (#xE0 . #x00E0) (#xE1 . #x00E1) (#xE2 . #x00E2) (#xE3 . #x0103)
	    (#xE4 . #x00E4) (#xE5 . #x0107) (#xE6 . #x00E6) (#xE7 . #x00E7)
	    (#xE8 . #x00E8) (#xE9 . #x00E9) (#xEA . #x00EA) (#xEB . #x00EB)
	    (#xEC . #x00EC) (#xED . #x00ED) (#xEE . #x00EE) (#xEF . #x00EF)
	    (#xF0 . #x0111) (#xF1 . #x0144) (#xF2 . #x00F2) (#xF3 . #x00F3)
	    (#xF4 . #x00F4) (#xF5 . #x0151) (#xF6 . #x00F6) (#xF7 . #x015B)
	    (#xF8 . #x0171) (#xF9 . #x00F9) (#xFA . #x00FA) (#xFB . #x00FB)
	    (#xFC . #x00FC) (#xFD . #x0119) (#xFE . #x021B) (#xFF . #x00FF))))

  ;; Fill in the equivalences

  ;; Default the whole equivalences table
  (aset zero 0 0)
  (put-char-table t zero latin-unity-equivalences)

  ;; Control 1 code points are spatial
  ;; Warning on these is beyond the scope of this library.
  (put-char-table 'control-1
		  (vector latin-unity-all-flags
			  nil nil nil nil nil nil nil nil nil)
		  latin-unity-equivalences)

  ;; Now map over the unitable to the equivalences char-table
  (mapc (lambda (equivs)
	  (when equivs			; null for all non-Latin characters
	    (dolist (ch1 equivs)
	      (let ((vec (copy-sequence
			  (get-char-table ch1 latin-unity-equivalences)))
		    (ucs (char-to-ucs ch1)))
		(when ucs (aset vec u+index ucs))
		(dolist (ch2 equivs)
		  (let* ((cset (char-charset ch2))
			 (bit (get cset 'latin-unity-flag-bit))
			 (index (get cset 'latin-unity-index)))
		    (aset vec 0 (logior bit (aref vec 0)))
		    (aset vec index ch2)))
		(put-char-table ch1 vec latin-unity-equivalences)))))
	unitable))

(defun latin-unity-dump-tables ()
  "Create a Lisp library to initialize the equivalences char-table."

  (interactive)

  ;; set up buffer
  (set-buffer (get-buffer-create "latin-unity-tables.el"))
  (erase-buffer)

  ;; insert preface
  (let ((nilvec (make-vector (+ (length latin-unity-character-sets) 2) nil))
	(creation-date-string (format-time-string "%Y %B %d")))
    (insert ";;; latin-unity-tables.el --- initialize latin-unity-equivalences"
	    "\n;; Do not edit --- automatically generated."
	    "\n;; Created: " creation-date-string
	    "\n(provide 'latin-unity-tables)"
	    "\n(defconst latin-unity-equivalences"
	    "\n  (let ((table (make-char-table 'generic)))"
	    "\n    ;; default all non-Latin charsets"
    (format "\n    (put-char-table t %s table)"
	    (progn (aset nilvec 0 0) nilvec))
            "\n    ;; Control 1 code points are spatial"
	    "\n    ;; Warning on these is beyond this library's scope."
    (format "\n    (put-char-table 'control-1 %s table)"
	    (progn (aset nilvec 0 latin-unity-all-flags) nilvec)))

    ;; insert table insertions
    ;; alternate mmc: (format "(apply #'make-char '%s)" (split-char ch))
    (flet ((mmc (ch)
	     (let ((x (split-char ch)))
	       (concat (format "(make-char '%s %d" (first x) (second x))
		       (if (third x) (format " %d)" (third x)) ")")))))
      (map-char-table
       (lambda (key val)
	 (when (characterp key)
	   (insert (format "\n    (put-char-table %s (vector %s) table)"
			   (mmc key)
			   (mapconcat
			    (lambda (elt)
			      (cond ((characterp elt) (mmc elt))
				    ((null elt) "nil")
				    ;; be careful to emit read syntax here!
				    ((integerp elt) (format "#x%X" elt))
				    (t (format "%s" elt))))
			    val
			    " ")))))
       latin-unity-equivalences))

    ;; insert trailing matter
    (insert "\n    table)"
	    "\n  \"Map a (Latin) Mule character to the set of"
	    " character sets containing it."
	    "\nCreated: " creation-date-string "\")"
	    "\n(put 'latin-unity-equivalences 'creation-date-string \""
	    creation-date-string "\")"
	    "\n;;; end of latin-unity-tables.el"
	    "\n"))

  ;; write the file
  (write-file "latin-unity-tables.el" t)
  (message "Wrote %s." "latin-unity-tables.el"))

;;; end of latin-unity-utils.el
