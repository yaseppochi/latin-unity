;;; latin-unity.el --- Identify equivalent characters in the ISO Latin sets

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 January 17
;; Last-modified: 2002 January 21

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

(provide 'latin-unity)


;;; Requires
;; Do NOT require latin-unity-utils; that requires Mule-UCS.

(require 'latin-unity-vars)
(require 'latin-unity-tables)


;;; User customization

(defgroup latin-unity nil
  "Handle equivalent ISO-8859 characters properly (identify them) on output."
  :group 'mule)

(defcustom latin-unity-approved-ucs-list '(utf-8 iso-2022-7 ctext)
  "List of coding systems considered to be universal.

Order matters; coding systems earlier in the list will be preferred when
recommending a coding system.
"
  :type '(list symbol)
  :group 'latin-unity)

;; #### Coding systems which are not Latin and not in
;; `latin-unity-approved-ucs-list' are handled by short circuiting checks
;; of coding system against the next two variables.  A preferable approach
;; is to define an alist of coding systems to corresponding sets of "safe"
;; character sets, then checking that `(charsets-in-region begin end)' is
;; contained in the appropriate set.  If you want this _now_ do it yourself
;; and send a patch to <stephen@xemacs.org> ;-).

(defcustom latin-unity-ignored-coding-system-list nil
  "List of coding systems such that the buffer is not checked for Latin unity.

Usually this means that `buffer-file-coding-system' is a member of this list.
#### not clear this API is right."
  :type '(list symbol)
  :group 'latin-unity)

(defcustom latin-unity-approved-coding-system-list nil
  "List of coding systems forcing a save of the buffer even if Latin unity
is not satisfied.
#### not clear this API is right."
  :type '(list symbol)
  :group 'latin-unity)


;;; Implementation

;; Accessors for character and charset equivalences

(defsubst latin-unity-feasible-charsets (character)
  "Return the set (bit-vector) of charsets that can represent CHARACTER.
Accessor for `latin-unity-equivalences'."
  (aref (get-char-table character latin-unity-equivalences) 0))

(defsubst latin-unity-equivalent-character (character charset)
  "Return the code point representing CHARACTER in CHARSET.
Accessor for `latin-unity-equivalences'."
  (aref (get-char-table character latin-unity-equivalences)
	(get charset 'latin-unity-index)))

;;;###autoload
(defun latin-unity-buffer-feasible-representations ()
  "Apply latin-unity-region-feasible-representations to the current buffer."
  (interactive)
  (latin-unity-region-feasible-representations (point-min)
					       (point-max)
					       (current-buffer)))

;; latin-unity-region-feasible-representations
;;
;; The basic algorithm is to map over the region, compute the set of
;; charsets that can represent each character (the "feasible charset"),
;; and take the intersection of those sets.
;;
;; The current implementation takes advantage of the fact that ASCII
;; characters are common and cannot change asciisets.  Then using
;; skip-chars-forward makes motion over ASCII subregions very fast.
;;
;; This same strategy could be applied generally by precomputing classes
;; of characters equivalent according to their effect on latinsets, and
;; adding a whole class to the skip-chars-forward string once a member is
;; found.
;;
;; Probably efficiency is a function of the number of characters matched,
;; or maybe the length of the match string?  With "skip-category-forward"
;; over a precomputed category table it should be really fast.  In practice
;; for Latin character sets there are only 29 classes.

;;;###autoload
(defun latin-unity-region-feasible-representations (begin end &optional buf)
  "Return character sets that can represent the text from BEGIN to END in BUF.

BUF defaults to the current buffer.  Called interactively, will be
applied to the region.  Function assumes BEGIN <= END.

The return value is a cons.  The car is the list of character sets
that can individually represent all of the non-ASCII portion of the
buffer, and the cdr is the list of character sets that can
individually represent all of the ASCII portion."

  (interactive "r")
  ;; #### should collect "found" sets too, for listing in help buffer
  ;; this probably requires a second pass....
  (let* ((asciisets (logior (get 'ascii 'latin-unity-flag-bit)
			    (get 'latin-jisx0201 'latin-unity-flag-bit)))
	 (latinsets (logand (lognot asciisets) latin-unity-all-flags)))
    (save-excursion
      (set-buffer (or buf (current-buffer)))
      (goto-char begin)
      ;; The characters skipped here can't change asciisets
      (skip-chars-forward latin-unity-ascii-and-jis-roman)
      (while (< (point) end)
	(let* ((ch (char-after))
	       (cs (car (split-char ch))))
	  (cond ((or (eq cs 'latin-jisx0201)
		     (eq cs 'ascii))
		 (setq asciisets
		       (logand asciisets (latin-unity-feasible-charsets ch)
			       )))
		(t
		 (setq latinsets
		       (logand latinsets (latin-unity-feasible-charsets ch)
			       )))))
	(forward-char)
	;; The characters skipped here can't change asciisets
	(skip-chars-forward latin-unity-ascii-and-jis-roman)))
    (cons latinsets asciisets)))

(defvar latin-unity-iso-8859-1-aliases '(iso-8859-1)
  "List of coding systems to be treated as aliases of ISO 8859/1.")

;; I see nothing useful to be done with APPEND.
;; FILENAME, VISIT, or LOCKNAME could be used to default the coding system,
;; but this would conflict with the semantics of `write-region'.

;;;###autoload
(defun latin-unity-sanity-check (start end filename append visit lockname)
  "Check if `buffer-file-coding-system' can represent the region START to END.

FILENAME, APPEND, VISIT, and LOCKNAME are ignored.

Return nil if buffer-file-coding-system is not (ISO-2022-compatible) Latin.
If buffer-file-coding-system is safe for the charsets actually present in
the buffer, return it.  Otherwise, ask the user to choose a coding system,
and return that.

This function does _not_ do the safe thing when buffer-file-coding-system is
nil (aka no-conversion).  It considers that \"non-Latin\", and passes it on
to the Mule detection mechanism.

This function is intended for use as a `write-region-pre-hook'."

  ;; #### This function is too complicated.  Break it up.

  (let ((codesys buffer-file-coding-system))
    (cond ((null codesys) nil)
	  ((memq codesys latin-unity-ignored-coding-system-list) nil)
	  ((or (and (eq (coding-system-type codesys) 'iso2022)
		    (coding-system-property codesys 'charset-g1))
	       (memq codesys latin-unity-iso-8859-1-aliases))
	   (let* ((csets (latin-unity-region-feasible-representations start
								      end))
		  (asets (cdr csets))
		  (lsets (car csets))
		  (bfcsgr (or (car (rassq codesys
					  latin-unity-cset-codesys-alist))
			      (coding-system-property codesys
						      'charset-g1)))
		  recommended target-cs)
	     (when latin-unity-debug 
	       (cond ((null csets) (error "no feasible reps vectors?!?"))
		     ((null asets) (error "no ascii reps vector?!?"))
		     ((null lsets) (error "no latin reps vector?!?"))
		     ((null (get 'ascii 'latin-unity-flag-bit))
		      (error "no flag bit for ascii?!?"))
		     ((null (get bfcsgr 'latin-unity-flag-bit))
		      (error (format "no flag bit for %s?" bfcsgr))))
	       (message "%s" csets)
	       (sit-for 1))
	     (if (and (/= (logand (get 'ascii 'latin-unity-flag-bit) asets) 0)
		      (/= (logand (get bfcsgr 'latin-unity-flag-bit) lsets) 0))
		 codesys
	       (pop-to-buffer (get-buffer-create latin-unity-help-buffer) t)
	       ;; RFE: We'd like this to list the infeasible character sets.
	       ;; RFE: It also would be nice if the offending characters were
	       ;; marked in the buffer being checked.
	       (erase-buffer)
	       (insert (format "\
The default coding system for this buffer (%s) cannot
appropriately encode some of the characters which are present in the buffer.
Please pick a coding system.  The following are recommended because they can
encode any character in the buffer:

   "
			       codesys))
	       (mapc
		(lambda (cs)
		  (if (/= (logand (get cs 'latin-unity-flag-bit) lsets) 0)
		      (let ((sys (cdr (assq cs
					    latin-unity-cset-codesys-alist))))
			(unless recommended
			  (setq target-cs cs recommended sys))
			(insert (format " %s" sys)))))
		latin-unity-character-sets)
	       ;; universal coding systems
	       (mapc (lambda (cs)
		       (when (find-coding-system cs)
			 (unless recommended (setq recommended cs))
			 (insert (format " %s" cs))))
		     latin-unity-approved-ucs-list)
	       (insert "

Note that if you select a coding system that can not encode some characters
in your buffer, those characters will be changed to an arbitrary replacement
character, by default `~', on output.

More information on coding systems:

utf-8, iso-2022-7, and ctext support all characters safely.  iso-2022-7 and
ctext are ISO 2022 conforming coding systems for 7-bit and 8-bit environments
respectively.  Be careful, there is a lot of software that does not understand
them.  utf-8 (Unicode) may also be unsupported in some environments, but they
are becoming fewer all the time.  utf-8 is recommended if usable.

In Mule, most iso-* coding systems are capable of encoding all characters.
However, characters outside of the normal range for the coding system require
use of ISO 2022 extension techniques and is likely to be unsupported by other
software, including software that supports iso-2022-7 or ctext.

For a list of coding systems, abort this operations and invoke
`list-coding-systems.")

	       (let ((val (read-coding-system (format "Coding system [%s]: "
						      recommended)
					      recommended)))
		 (delete-window)
		 ;; compute return
		 (cond
		  ;; pre-approved coding systems
		  ((or (memq val latin-unity-approved-ucs-list)
		       (memq val latin-unity-approved-coding-system-list))
		   val)
		  ;; ISO 2022 (including ISO 8859) compatible systems
		  ;; maybe we should check for G2 and G3 sets
		  ((and (eq (coding-system-type val) 'iso2022)
			(setq target-cs
			      (or (coding-system-property val 'charset-g1)
				  target-cs))
			(if (latin-unity-remap-region start end target-cs val)
			    val
			  (error
			   (format (concat "Couldn't remap characters to"
					   " charset %s for coding system %s"
					   target-cs val))))))
		  ;; other coding systems -- eg Windows 125x, KOI8?
		  ;; unimplemented
		  (t nil))))))
	  (t nil))))

(defun latin-unity-remap-region (begin end character-set
				 ;; #### maybe this should be a keyword arg?
				 &optional coding-system)
  "Remap characters between BEGIN and END to equivalents in CHARACTER-SET.
Optional argument CODING-SYSTEM may be a coding system name (a symbol) or
nil.

Characters with no equivalent are left as-is.

Return CODING-SYSTEM if CODING-SYSTEM can encode all characters in the
region, t if CODING-SYSTEM is nil and the coding system with G0 = 'ascii
and G1 = CHARACTER-SET can encode all characters, and otherwise nil.  Note
that a non-null return does _not_ mean it is safe to write the file, only
the specified region.  (This behavior is useful for multipart MIME encoding
and the like.)

Interactively BEGIN and END are set to the current region and the function
prompts for CHARACTER-SET.  CODING-SYSTEM is always set to nil, as it has
no useful function interactively.

Note:  by default this function is quite fascist about universal coding
systems.  It only admits utf-8, iso-2022-7, and ctext.  Customize
`latin-unity-approved-ucs-list' to change this."

  (interactive "rSCharacter set: ")

  (if (not (charsetp (find-charset character-set)))
      ;; #### Should be more user-friendly here
      (error (format "%s is not the name of a character set." character-set)))

  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((repch (latin-unity-equivalent-character (char-after)
						       character-set)))
	  (if (not repch)
	      (forward-char 1)
	    (insert repch)
	    (delete-char 1))))))

  (cond ((memq coding-system latin-unity-approved-ucs-list) coding-system)
	((null (delq character-set
		     (delq 'ascii (charsets-in-region begin end))))
	 (or coding-system t))
	(t nil)))

;;;###autoload  
(defun latin-unity-test ()
  "Test the latin-unity package.

At present it just makes a multilingual buffer.  To test, setq
buffer-file-coding-system to some value, make the buffer dirty (eg
with RET BackSpace), and save."

  (interactive)
  (switch-to-buffer (get-buffer-create "latin-unity-test"))
  (erase-buffer)
  (insert "From here ...\n")
  (insert "Latin-1: f")
  (insert (make-char 'latin-iso8859-1 #xFC)) ; u umlaut, also in Latin-2
  (insert "r\n\nLatin-2: Nik")		; my apologies if I misremembered
  (insert (make-char 'latin-iso8859-2 57)) ; s caron, not in Latin-1
  (insert ?i)
  (insert (make-char 'latin-iso8859-2 102)) ; c acute, not in Latin-1
  (insert "\n... to here is representable in Latin-2 but not Latin-1.\n")
  (insert (make-char 'latin-iso8859-1 255))
  (insert "\nFrom top to here is not representable in Latin-[12].\n")

  (insert "
By deleting various portions of the buffer and saving, or by setq'ing
buffer-file-coding-system and saving, you can see how the thing works.
After compiling and loading the file, do (by hand)

(add-hook 'write-region-pre-hook 'latin-unity-sanity-check)

To see a little more of what it's doing (you need to read the code to
interpret, though) and to get some sledgehammer error checks

(setq latin-unity-debug t)

To disable the hook, do

(remove-hook 'write-region-pre-hook 'latin-unity-sanity-check)
"))

;;; end of latin-unity.el
