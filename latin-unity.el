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

;; Provides the 'iso-8859-15 coding system if yet undefined.
;; #### Get the final byte for 'iso-8859-15 and do it too.

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
#### not clear this API is right, see comment in `latin-unity.el'."
  :type '(list symbol)
  :group 'latin-unity)

(defcustom latin-unity-approved-coding-system-list nil
  "List of coding systems forcing a save of the buffer even if Latin unity
is not satisfied.
#### not clear this API is right, see comment in `latin-unity.el'."
  :type '(list symbol)
  :group 'latin-unity)

(defcustom latin-unity-iso-8859-1-aliases '(iso-8859-1)
  "List of coding systems to be treated as aliases of ISO 8859/1."
  :type '(list symbol)
  :group 'latin-unity)


;;; User interface

;; Install/uninstall

;;;###autoload
(defun latin-unity-install ()
  "Set up hooks and initialize variables for latin-unity.

This function is idempotent.  It will reinitialize any hooks or variables
that are not in initial state."

  (interactive)

  (add-hook 'write-region-pre-hook 'latin-unity-sanity-check))

;;;###autoload
(defun latin-unity-uninstall ()
  "Clean up hooks and void variables used by latin-unity."

  (interactive)

  (remove-hook 'write-region-pre-hook 'latin-unity-sanity-check))


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
(defun latin-unity-buffer-representations-feasible ()
  "Apply latin-unity-region-representations-feasible to the current buffer."
  (interactive)
  (latin-unity-region-representations-feasible (point-min)
					       (point-max)
					       (current-buffer)))

;; latin-unity-region-representations-feasible
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
(defun latin-unity-region-representations-feasible (begin end &optional buf)
  "Return character sets that can represent the text from BEGIN to END in BUF.

BUF defaults to the current buffer.  Called interactively, will be
applied to the region.  Function assumes BEGIN <= END.

The return value is a cons.  The car is the list of character sets
that can individually represent all of the non-ASCII portion of the
buffer, and the cdr is the list of character sets that can
individually represent all of the ASCII portion."

  (interactive "r")
  (let* ((asciisets (logior (get 'ascii 'latin-unity-flag-bit)
			    (get 'latin-jisx0201 'latin-unity-flag-bit)))
	 (latinsets (logand (lognot asciisets) latin-unity-all-flags)))
    (save-excursion
      (set-buffer (or buf (current-buffer)))
      (goto-char begin)
      ;; The characters skipped here can't change asciisets.
      ;; Note that to generalize this we would need to have a notion of
      ;; classes of characters which do not change the representability.
      ;; One thing we can do is to add the character itself.
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


;; #### possibly it would be faster to do this in the previous function
;; however, this is not obvious because this function is quite fast (the
;; region mapping is all in C), and therefore we can short-circuit the
;; slow Lisp function above
(defun latin-unity-region-representations-present (begin end &optional buffer)
  "Return a cons of two bit vectors giving character sets in region.

The car indicates which Latin characters sets were found, the cdr the ASCII
character sets.  BUFFER defaults to the current buffer."

  (let ((lsets 0)
	(asets 0))
    (mapc (lambda (cs)
	    (cond ((memq cs '(ascii latin-jisx0201))
		   (setq asets (logior (get cs 'latin-unity-flag-bit) asets)))
		  ((get cs 'latin-unity-bit-flag)
		   (setq lsets (logior (get cs 'latin-unity-flag-bit) lsets)))))
	  (charsets-in-region begin end buffer))
    (cons lsets asets)))


;; #### I see nothing useful to be done with APPEND.
;; FILENAME, VISIT, or LOCKNAME could be used to default the coding system,
;; but this would conflict with the semantics of `write-region'.
;; #### The efficiency of this function can clearly be improved.

;;;###autoload
(defun latin-unity-sanity-check (begin end filename append visit lockname
				 &optional coding-system)
  "Check if CODING-SYSTEM can represent all characters between BEGIN and END.

For compatibility with old broken versions of `write-region', CODING-SYSTEM
defaults to `buffer-file-coding-system'.  FILENAME, APPEND, VISIT, and
LOCKNAME are ignored.

Return nil if buffer-file-coding-system is not (ISO-2022-compatible) Latin.
If buffer-file-coding-system is safe for the charsets actually present in
the buffer, return it.  Otherwise, ask the user to choose a coding system,
and return that.

This function does _not_ do the safe thing when buffer-file-coding-system is
nil (aka no-conversion).  It considers that \"non-Latin\", and passes it on
to the Mule detection mechanism.

This function is intended for use as a `write-region-pre-hook'.  It does
nothing except return CODING-SYSTEM if `write-region' handlers are inhibited."

  (let ((codesys (or coding-system buffer-file-coding-system)))
    (cond
     ;; don't do anything if we're in a `write-region' handler
     ((eq inhibit-file-name-operation 'write-region) codesys)
     ((null codesys) nil)
     ((memq codesys latin-unity-ignored-coding-system-list) nil)
     ((or (and (eq (coding-system-type codesys) 'iso2022)
	       (coding-system-property codesys 'charset-g1))
	  (memq codesys latin-unity-iso-8859-1-aliases))
      ;; c[al]?sets == compatible character sets
      ;; p[al]?sets == present in buffer character sets
      ;; a == ascii, l == latin
      (let* ((csets (latin-unity-region-representations-feasible begin end))
	     (casets (cdr csets))
	     (clsets (car csets))
	     ;; we also need to check for what's in the buffer
	     ;; #### it will save a lot of time in typical case if we
	     ;; do this check first and return immediately if feasible
	     (psets (latin-unity-region-representations-present begin end))
	     (pasets (cdr psets))
	     (plsets (car psets))
	     (bfcsgr (or (car (rassq codesys latin-unity-cset-codesys-alist))
			 (coding-system-property codesys 'charset-g1)))
	     recommended target-cs)
	(when latin-unity-debug 
	  (cond ((null csets) (error "no feasible reps vectors?!?"))
		((null casets) (error "no ascii reps vector?!?"))
		((null clsets) (error "no latin reps vector?!?"))
		((null psets) (error "no reps present vectors?!?"))
		((null pasets) (error "no ascii reps present vector?!?"))
		((null plsets) (error "no latin reps present vector?!?"))
		((null (get 'ascii 'latin-unity-flag-bit))
		 (error "no flag bit for ascii?!?"))
		((null (get bfcsgr 'latin-unity-flag-bit))
		 (error (format "no flag bit for %s?" bfcsgr))))
	  (message "%s" csets)
	  (sit-for 1))
	;; we represent everything in the buffer without remapping
	(if (and (= (logxor (get 'ascii 'latin-unity-flag-bit) pasets) 0)
		 (= (logxor (get bfcsgr 'latin-unity-flag-bit) plsets) 0))
	    codesys
	  ;; #### break out this help code into a separate function.
	  ;; don't forget to leave the computation of the recommend cs!
	  ;; #### this let is bletch, figure out how to handle the help
	  ;; buffer elegantly
	  (let ((obuffer (current-buffer)))
	    (pop-to-buffer (get-buffer-create latin-unity-help-buffer) t)
	    ;; #### RFE: It also would be nice if the offending characters were
	    ;; marked in the buffer being checked.
	    (erase-buffer)
	    (insert (format "\
This buffer's default coding system (%s)
cannot appropriately encode some of the characters present in the buffer."
			    codesys))
	    (when latin-unity-debug
	      (insert "  Character sets found are:\n\n   ")
	      (mapc (lambda (cs) (insert (format " %s" cs)))
		    ;; #### Blarg, we've already done this
		    (charsets-in-region begin end obuffer)))
	    (insert "

Please pick a coding system.  The following are recommended because they can
encode any character in the buffer:

   ")
	    (mapc
	     (lambda (cs)
	       (if (/= (logand (get cs 'latin-unity-flag-bit) clsets) 0)
		   (let ((sys (cdr (assq cs latin-unity-cset-codesys-alist))))
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

Page down for more information on coding systems:

utf-8, iso-2022-7, and ctext support all characters safely.  iso-2022-7 and
ctext are ISO 2022 conforming coding systems for 7-bit and 8-bit environments
respectively.  Be careful, there is a lot of software that does not understand
them.  utf-8 (Unicode) may also be unsupported in some environments, but they
are becoming fewer all the time.  utf-8 is recommended if usable.

In Mule, most iso-* coding systems are capable of encoding all characters.
However, characters outside of the normal range for the coding system require
use of ISO 2022 extension techniques and is likely to be unsupported by other
software, including software that supports iso-2022-7 or ctext.

For a list of coding systems, abort now and invoke `list-coding-systems'.")
	    (goto-char (point-min))

	    (let ((val (read-coding-system (format "Coding system [%s]: "
						   recommended)
					   recommended)))
	      (delete-window)
	      (set-buffer obuffer)
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
		     (if (latin-unity-remap-region begin end target-cs val)
			 val
		       (error
			(format (concat "Couldn't remap characters to"
					" charset %s for coding system %s"
					target-cs val))))))
	       ;; other coding systems -- eg Windows 125x, KOI8?
	       ;; #### unimplemented
	       (t nil)))))))
      (t nil))))


;;;###autoload
(defun latin-unity-recode-region (begin end wrong-cs right-cs)
  "Recode characters between BEGIN and END from WRONG-CS to RIGHT-CS.

When called interactively, BEGIN and END are set to the beginning and
end, respectively, of the active region, and XEmacs prompts for WRONG-CS
and RIGHT-CS.

WRONG-CS and RIGHT-CS are character sets.  Characters retain the same code
point but the character set is changed.  Only characters from WRONG-CS are
changed to RIGHT-CS.  The identity of the character may change.  Note that
this could be dangerous, if characters whose identities you do not want
changed are included in the region.  This function cannot guess which
characters you want changed, and which should be left alone.

Another way to accomplish this, but using coding systems rather than character
sets to specify the desired recoding, is `latin-unity-recode-coding-region'.
That function may be faster but is somewhat more dangerous, because it may
recode more than one character set.

To change from one Mule representation to another without changing identity
of any characters, use `latin-unity-remap-region'."

  ;; #### Implement constraint and completion here
  (interactive "*r\nSCurrent character set: \nSDesired character set: ")

  (save-excursion
    (goto-char begin)
    (while (< (point) end)
      (let ((split (split-char (char-after))))
	(if (eq (car split) wrong-cs)
	    ;; this order preserves marker and extent endpoints
	    (progn
	      (insert (apply #'make-char (cons right-cs (cdr split))))
	      (delete-char))
	  (forward-char))))))


;;;###autoload
(defun latin-unity-recode-coding-region (begin end wrong-cs right-cs)
  "Recode text between BEGIN and END from WRONG-CS to RIGHT-CS.

When called interactively, BEGIN and END are set to the beginning and
end, respectively, of the active region, and XEmacs prompts for WRONG-CS
and RIGHT-CS.

WRONG-CS and RIGHT-CS are coding systems.  Characters retain the same code
point but the character set is changed.  The identity of characters may change.
This is an inherently dangerous function; multilingual text may be recoded in
unexpected ways.  #### It's also dangerous because the coding systems are not
sanity-checked in the current implementation.

Another, safer, way to accomplish this, using character sets rather than coding
systems to specify the desired recoding, is to use `latin-unity-recode-region.

To change from one Mule representation to another without changing identity
of any characters, use `latin-unity-remap-region'."

  (interactive "*r\nzCurrent coding system: \nzDesired coding system: ")
  (encode-coding-region begin end wrong-cs)
  (decode-coding-region begin end right-cs))


;;;###autoload
(defun latin-unity-remap-region (begin end character-set
				 ;; #### maybe this should be a keyword arg?
				 &optional coding-system)
  "Remap characters between BEGIN and END to equivalents in CHARACTER-SET.
Optional argument CODING-SYSTEM may be a coding system name (a symbol) or
nil.  Characters with no equivalent are left as-is.

When called interactively, BEGIN and END are set to the beginning and
end, respectively, of the active region, and XEmacs prompts for
CHARACTER-SET.

Return CODING-SYSTEM if CODING-SYSTEM can encode all characters in the
region, t if CODING-SYSTEM is nil and the coding system with G0 = 'ascii
and G1 = CHARACTER-SET can encode all characters, and otherwise nil.  Note
that a non-null return does _not_ mean it is safe to write the file, only
the specified region.  (This behavior is useful for multipart MIME encoding
and the like.)

Interactively BEGIN and END are set to the current region and the function
prompts for CHARACTER-SET.  There is no way to specify CODING-SYSTEM, as it
has no useful function interactively.

Note:  by default this function is quite fascist about universal coding
systems.  It only admits utf-8, iso-2022-7, and ctext.  Customize
`latin-unity-approved-ucs-list' to change this.

This function remaps characters that are artificially distinguished by Mule
internal code.  It may change the code point as well as the character set.
To recode characters that were decoded in the wrong coding system, use
`latin-unity-recode-region'."

  (interactive "*r\nSCharacter set: ")

  (if (not (charsetp (find-charset character-set)))
      ;; #### Should be more user-friendly here
      (error (format "%s is not the name of a character set." character-set)))

  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (not (eobp))
	;; #### RFE: optimize using skip-chars-forward
	(let* ((ch (char-after))
	       (repch (latin-unity-equivalent-character ch character-set)))
	  (if (or (not repch)
		  (= repch ch))
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
  (switch-to-buffer (get-buffer-create "latin-unity test"))
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

M-: (latin-unity-install) RET.

To see a trace of what it's doing (you need to read the code to interpret),
to get more information about character sets in the region, and to enable
some sledgehammer error checks

M-: (setq latin-unity-debug t) RET

To disable the hook, do

M-: (latin-unity-uninstall) RET.

Note:  the *install functions are interactive, I wrote them as above so you
can C-x C-e them in this buffer.
"))

;;; end of latin-unity.el
