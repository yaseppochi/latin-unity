;;; latin-unity.el --- Identify equivalent characters in the ISO Latin sets

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 January 17
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

;; Provides the 'iso-8859-15 coding system if yet undefined.
;; #### Get the final byte for 'iso-8859-16 and do it too.

;;; Code:

(provide 'latin-unity)


;;; Requires

(require 'latin-unity-vars)
(require 'latin-unity-latin9)		; define iso-8859-15
;; #### uncomment when we have ISO 8859/16
;(require 'latin-unity-latin10)		; define iso-8859-16
(if (or (fboundp 'character-to-unicode)	; XEmacs  post-21.5.5
	(fboundp 'char-to-ucs))		; Mule-UCS already loaded
    (require 'latin-unity-tables "latin-unity-utils")
  (require 'latin-unity-tables))	; doesn't require Unicode support


;;; User customization

(defgroup latin-unity nil
  "Handle equivalent ISO-8859 characters properly (identify them) on output."
  :group 'mule)

;; #### We demand a coding system widget!
;; #### The :set functions should do sanity and cross checks.
(defcustom latin-unity-preapproved-coding-system-list
  '(buffer-default preferred)
  "*List of coding systems used without querying the user if feasible.

The feasible first coding system in this list is used.  The special values
'preferred and 'buffer-default may be present:

  buffer-default  Use the coding system used by `write-region', if feasible.
  preferred       Use the coding system specified by `prefer-coding-system'
                  if feasible.

Note that if your preferred coding system is a universal coding system, and
@samp{preferred} is a member of this list, @pkgname{} will blithely convert
all your files to that coding system.  This is considered a feature, but it
may surprise most users.  Users who don't like this behavior should put
@samp{preferred} in @samp{latin-unity-preferred-coding-system-list}.

\"Feasible\" means that all characters in the buffer can be represented by
the coding system.  Coding systems in `latin-unity-ucs-list' are always
considered feasible.  Other feasible coding systems are computed by
`latin-unity-representations-feasible-region'.

Note that the first universal coding system in this list shadows all other
coding systems."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-preferred-coding-system-list
  '(iso-8859-1 iso-8859-15 iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-9)
  "*List of coding systems suggested the user if feasible.

If none of the coding systems in `latin-unity-preferred-coding-system-list'
are feasible, this list will be recommended to the user, followed by the
`latin-unity-ucs-list' (so those coding systems should not be in this list).
The first coding system in this list is default.

The special values 'preferred and 'buffer-default may be present:
  buffer-default  Use the coding system used by `write-region', if feasible.
  preferred       Use the coding system specified by `prefer-coding-system'
                  if feasible.

\"Feasible\" means that all characters in the buffer can be represented by
the coding system.  Coding systems in `latin-unity-ucs-list' are always
considered feasible.  Other feasible coding systems are computed by
`latin-unity-representations-feasible-region'."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-ucs-list '(utf-8 iso-2022-7 ctext escape-quoted)
  "*List of coding systems considered to be universal.

A universal coding system can represent all characters by definition.

Order matters; coding systems earlier in the list will be preferred when
recommending a coding system.  These coding systems will not be used without
querying the user, and follow the `latin-unity-preferred-coding-system-list'
in the list of suggested coding systems.

If none of the preferred coding systems are feasible, the first in this list
will be the default.

Note: if `escape-quoted' is not a member of this list, you will be unable to
autosave files or byte-compile Mule Lisp files."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-charset-alias-alist
  '((latin-1 . latin-iso8859-1)
    (latin-2 . latin-iso8859-2)
    (latin-3 . latin-iso8859-3)
    (latin-4 . latin-iso8859-4)
    (latin-5 . latin-iso8859-9)
    (latin-9 . latin-iso8859-15)
    (latin-10 . latin-iso8859-16))
  "*Alist mapping aliases to Mule charset names (symbols)."
  :type '(repeat (cons symbol symbol))
  :group 'latin-unity)

(defcustom latin-unity-coding-system-alias-alist nil
  "*Alist mapping aliases to Mule coding system names (symbols)."
  :type '(repeat (cons symbol symbol))
  :group 'latin-unity)

;; Needed because 'iso-8859-1 is type 'no-conversion, NOT type 'iso2022
(defcustom latin-unity-iso-8859-1-aliases '(iso-8859-1)
  "List of coding systems to be treated as aliases of ISO 8859/1.

This is not a user variable; to customize input of coding systems or charsets,
`latin-unity-coding-system-alias-alist' or `latin-unity-charset-alias-alist'."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-coding-system-list-buffer
  " *latin-unity coding system preferences*"
  "Name of buffer used to display codings systems by priority."
  :type 'string
  :group 'latin-unity)

(defun latin-unity-list-coding-systems (display-excluded)
  "Display the coding systems listed by priority and group.

With prefix argument, also display otherwise excluded coding systems.

See also `latin-unity-preapproved-coding-systems',
`latin-unity-preferred-coding-systems', and `latin-unity-ucs-list'."

  (interactive "_P")

  (save-excursion
    (pop-to-buffer (get-buffer-create
		    latin-unity-coding-system-list-buffer))
    (erase-buffer)
    (let ((start (point)))

      (insert "Pre-approved coding systems:\n ")
      (mapc (lambda (codesys) (insert (format " %s" codesys)))
	    latin-unity-preapproved-coding-system-list)
      (fill-region start (point))

      (insert "\nSuggested coding systems:\n ")
      (setq start (point))
      (mapc (lambda (codesys) (insert (format " %s" codesys)))
	    latin-unity-preferred-coding-system-list)
      (fill-region start (point))

      (insert "\Universal coding systems:\n ")
      (setq start (point))
      (mapc (lambda (codesys) (insert (format " %s" codesys)))
	    latin-unity-ucs-list)
      (fill-region start (point))

      (when display-excluded
	;; Should arrange to only display included ones!
	(insert "\nAll coding systems:\n ")
	(setq start (point))
	(mapc (lambda (codesys) (insert (format " %s" codesys)))
	      (coding-system-list))
	(fill-region start (point))))))

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

;; Buffer coding system feasibility

;;;###autoload
(defun latin-unity-representations-feasible-buffer ()
  "Apply latin-unity-representations-feasible-region to the current buffer."
  (interactive)
  (latin-unity-representations-feasible-region (point-min)
					       (point-max)
					       (current-buffer)))

;; latin-unity-representations-feasible-region
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
(defun latin-unity-representations-feasible-region (begin end &optional buf)
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
      (save-restriction
	(widen)
	(let ((begin (or begin (point-min)))
	      (end (or end (and (null begin) (point-max)))))
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
	    (skip-chars-forward latin-unity-ascii-and-jis-roman)))))
    (cons latinsets asciisets)))


;; #### possibly it would be faster to do this in the previous function
;; charsets-in-region is in Lisp and quite slow.  :-(
(defun latin-unity-representations-present-region (begin end &optional buffer)
  "Return a cons of two bit vectors giving character sets in region.

The car indicates which Latin characters sets were found, the cdr the ASCII
character sets.  BUFFER defaults to the current buffer."

  (let ((lsets 0)
	(asets 0)
	(skipchars ""))
    (save-excursion
      (set-buffer (or buffer (current-buffer)))
      (save-restriction
	;; #### not quite right, should test
	(narrow-to-region (or begin (point-min))
			  (or end (and (null begin) (point-max))))
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((ch (char-after))
		 (cs (car (split-char ch))))
	    (cond
	     ((eq cs 'ascii)
	      (setq skipchars (concat "\000-\177" skipchars))
	      (setq asets (logior (get cs 'latin-unity-flag-bit 0) asets)))
	     ((eq cs 'latin-jisx0201)
	      ;; #### get this someday
	      ;;(setq skipchars (concat skipchars latin-unity-latin-jisx0201))
	      (setq skipchars (concat skipchars (list ch)))
	      (setq asets (logior (get cs 'latin-unity-flag-bit 0) asets)))
	     (t
	      ;; #### actually we can do the whole charset here
	      ;; precompute and set a property on the cs symbol
	      (setq skipchars (concat skipchars (list ch)))
	      (setq lsets (logior (get cs 'latin-unity-flag-bit 0) lsets)))))
	  ;; The characters skipped here can't change asciisets
	  (skip-chars-forward skipchars))))
    (cons lsets asets)))

;; #### I see nothing useful to be done with APPEND.
;; FILENAME, VISIT, or LOCKNAME could be used to default the coding system,
;; but this would conflict with the semantics of `write-region'.
;; #### The efficiency of this function can clearly be improved.
;; #### Maybe this function should have a no-ask for the sake of testing?

(defcustom latin-unity-like-to-live-dangerously nil
  "Suppress warnings about failure to remap buffer."
  :type 'boolean
  :group 'latin-unity)

;;;###autoload
(defun latin-unity-sanity-check (begin end filename append visit lockname
				 &optional coding-system)
  "Check if CODING-SYSTEM can represent all characters between BEGIN and END.

If not, attempt to remap Latin characters to a single Latin-N set.

For compatibility with old broken versions of `write-region', CODING-SYSTEM
defaults to `buffer-file-coding-system'.  FILENAME, APPEND, VISIT, and
LOCKNAME are ignored.

Return nil if buffer-file-coding-system is not (ISO-2022-compatible) Latin.
If buffer-file-coding-system is safe for the charsets actually present in
the buffer, return it.  Otherwise, ask the user to choose a coding system,
and return that.

This function does _not_ do the safe thing when `buffer-file-coding-system'
is nil (= no-conversion).  It considers that \"non-Latin\", and passes it on
to the Mule detection mechanism.  This could result in corruption.  So avoid
setting `buffer-file-coding-system' to nil or 'no-conversion or 'binary.

This function is intended for use as a `write-region-pre-hook'.  It does
nothing except return nil if `write-region' handlers are inhibited."

  ;; don't do anything if we're in a `write-region' handler
  (if (eq inhibit-file-name-operation 'write-region)
      ;; is this the right return value?
      nil
    (let ((buffer-default
	   ;; theoretically we could look at other write-region-prehooks,
	   ;; but they might write the buffer and we lose bad
	   (or coding-system
	       buffer-file-coding-system
	       (find-file-coding-system-for-write-from-filename filename)))
	  (preferred (coding-category-system (car (coding-priority-list))))
	  ;; check what representations are feasible
	  ;; csets == compatible character sets as (latin . ascii)
	  (csets (latin-unity-representations-feasible-region begin end))
	  ;; as an optimization we also check for what's in the buffer
	  ;; psets == present in buffer character sets as (latin . ascii)
	  (psets (latin-unity-representations-present-region begin end)))
      (when latin-unity-debug
	;; cheezy debug code
	(cond ((null csets) (error "no feasible reps vectors?!?"))
	      ((null (cdr csets)) (error "no ascii reps vector?!?"))
	      ((null (car csets)) (error "no latin reps vector?!?"))
	      ((null psets) (error "no reps present vectors?!?"))
	      ((null (cdr psets)) (error "no ascii reps present vector?!?"))
	      ((null (car psets)) (error "no latin reps present vector?!?"))
	      ((null (get 'ascii 'latin-unity-flag-bit))
	       (error "no flag bit for ascii?!?")))
	(message "%s %s" csets psets)
	(sit-for 1))

      (cond
       ;; try the preapproved systems
       ((catch 'done
	  (let ((systems latin-unity-preapproved-coding-system-list)
		(sys (car latin-unity-preapproved-coding-system-list)))
	    ;; while always returns nil
	    (while systems
	      ;; #### to get rid of this we probably need to preprocess
	      ;; latin-unity-preapproved-coding-system-list
	      (setq sys (cond ((and (eq sys 'buffer-default) buffer-default))
			      ((and (eq sys 'preferred) preferred))
			      (t sys)))
	      (when (latin-unity-maybe-remap begin end sys csets psets t)
		(throw 'done sys))
	      (setq systems (cdr systems))
	      (setq sys (car systems))))))

       ;; ask the user about the preferred systems
       ;; #### RFE: It also would be nice if the offending characters
       ;; were marked in the buffer being checked.
       (t (let* ((recommended
		  (latin-unity-recommend-representation begin end csets))
		 (codesys (car recommended))
		 ;(charset (cdr recommended)) ; unused?
		 )
	    (when latin-unity-debug (message "%s" recommended))
	    ;; compute return
	    (cond

	     ;; universal coding systems
	     ;; #### we might want to unify here if the codesys is ISO 2022
	     ;; but we don't have enough information to decide
	     ((memq codesys latin-unity-ucs-list) codesys)

	     ;; ISO 2022 (including ISO 8859) compatible systems
	     ;; #### maybe we should check for G2 and G3 sets
	     ;; note the special case is necessary, as 'iso-8859-1 is NOT
	     ;; type 'iso2022, it's type 'no-conversion
	     ((or (memq codesys latin-unity-iso-8859-1-aliases)
		  (eq (coding-system-type codesys) 'iso2022))
	      ;; #### make sure maybe-remap always returns a coding system
	      ;; #### I thought about like-to-live-dangerously here,
	      ;; but first make sure make sure maybe-remap returns nil
	      (when (latin-unity-maybe-remap begin end codesys csets psets nil)
		codesys))

	     ;; other coding systems -- eg Windows 125x, KOI8?
	     ;; #### unimplemented

	     ;; no luck, pass the buck back to `write-region'
	     ;; #### we really shouldn't do this, defeats the purpose
	     (t (unless latin-unity-like-to-live-dangerously
		  (warn (concat "Passing to default coding system,"
				" data corruption likely"))
		  (ding)
		  nil))
	     )))
       ))))


;; #### maybe this is what we want to test?  add a no-ask flag.
(defun latin-unity-recommend-representation (begin end feasible
					     &optional buffer)
  "Recommend a representation for BEGIN to END from FEASIBLE in BUFFER.

Returns a cons of a coding system (which can represent all characters in
BUFFER) and a charset (to which all non-ASCII characters in BUFFER can be
remapped.  (The former will be nil only if `latin-unity-ucs-list' is nil.)

FEASIBLE is a bitvector representing the feasible character sets.
BUFFER defaults to the current buffer."

  ;; interactive not useful because of representation of FEASIBLE
  (unless buffer (setq buffer (current-buffer)))

        ;; #### this code is repeated too often
  (let ((buffer-default
	 ;; theoretically we could look at other write-region-prehooks,
	 ;; but they might write the buffer and we lose bad
	 (or
	  ; coding-system ; I think this is null anyway
	  buffer-file-coding-system
	  ;; #### this is wrong for auto-saves at least
	  ; (find-file-coding-system-for-write-from-filename
	  ;   (buffer-file-name))
	  ))
	(preferred (coding-category-system (car (coding-priority-list))))
	recommended)
    (save-excursion
      (pop-to-buffer (get-buffer-create latin-unity-help-buffer) t)
      (erase-buffer)
      (insert (format "\
Choose a coding system to save buffer %s.
All preapproved coding systems (%s)
fail to appropriately encode some of the characters present in the buffer."
		      (buffer-name buffer)
		      latin-unity-preapproved-coding-system-list))
      ;; #### we could get this from PRESENT and avoid the auto-save silliness
      (when latin-unity-debug
	(insert "  Character sets found are:\n\n   ")
	(mapc (lambda (cs) (insert (format " %s" cs)))
	      (save-excursion
		(set-buffer buffer)
		(save-restriction
		  (widen)
		  (let ((begin (or begin (point-min)))
			(end (or end (point-max))))
		    ;; #### this function is slow!
		    (charsets-in-region begin end))))))
      (insert "

Please pick a coding system.  The following are recommended because they can
encode any character in the buffer:

   ")
      (mapc (lambda (cs)
	      (when latin-unity-debug (message "%s" cs))
	      (let ((sys (cdr (assq cs latin-unity-cset-codesys-alist))))
		(when (and (memq sys
				 (mapcar
				  (lambda (x)
				    (cond ((and (eq x 'preferred) preferred))
					  ((and (eq x 'buffer-default)
						buffer-default))
					  (t x)))
				  latin-unity-preferred-coding-system-list))
			   (find-coding-system sys)
			   (/= (logand (get cs 'latin-unity-flag-bit)
				       (car feasible))
			       0))
		  (unless recommended (setq recommended (cons sys cs)))
		  (insert (format " %s" sys)))))
	    latin-unity-character-sets)
      ;; universal coding systems
      (mapc (lambda (sys)
	      (when (find-coding-system sys)
		(unless recommended (setq recommended (cons sys nil)))
		(insert (format " %s" sys))))
	    latin-unity-ucs-list)
      (insert "

Note that if you select a coding system that can not encode some characters
in your buffer, those characters will be changed to an arbitrary replacement
character, by default `~', on output.

Page down for more information on coding systems:

utf-8, iso-2022-7, and ctext support all characters safely.  iso-2022-7 and
ctext are ISO 2022 conforming coding systems for 7-bit and 8-bit environments
respectively.  Be careful, there is a lot of software that does not understand
them.  utf-8 (Unicode) may also be unsupported in some environments, but they
are becoming fewer all the time.  utf-8 is recommended if usable (except for
some users of Asian ideographs who need to mix languages).

In Mule, most iso-* coding systems are capable of encoding all characters.
However, characters outside of the normal range for the coding system require
use of ISO 2022 extension techniques and is likely to be unsupported by other
software, including software that supports iso-2022-7 or ctext.

For a list of coding systems, quit and invoke `list-coding-systems'.")
      (goto-char (point-min))
      ;; `read-coding-system' never returns a non-symbol
      (let ((val (read-coding-system (format "Coding system [%s]: "
					     (car recommended))
				     (car recommended))))
	(delete-window)
	(if (eq val (car recommended))
	    recommended
	  (cons val
		;; #### this code is repeated too often
		(or (car (rassq val latin-unity-cset-codesys-alist))
		    (and val
			 (eq (coding-system-type val) 'iso2022)
			 (coding-system-property val 'charset-g1)))))))))

;; this could be a flet in latin-unity-sanity-check
;; -- no, this is what we want to test?
;; #### this function's interface needs to change, s/codesys/charset/
;; #### did you update all calls?
;; #### did you update all docs?
(defun latin-unity-maybe-remap (begin end codesys feasible
				&optional present no-error)
  "Try to remap from BEGIN to END to CODESYS.  Return nil on failure.

Return CODESYS on success.  CODESYS is a coding system or nil.
FEASIBLE is a cons of bitvectors indicating the set of character sets which
can represent all non-ASCII characters and ASCII characters, respectively,
in the current buffer.
PRESENT is a cons of bitvectors indicating the set of non-ASCII and ASCII
character sets, respectively, present in the current buffer.

Pass NO-ERROR to `latin-unity-remap-region'."

  ;; may God bless and keep the Mule ... far away from us!
  (when (memq codesys latin-unity-iso-8859-1-aliases)
    (setq codesys 'iso-8859-1))

  (when latin-unity-debug
    (message (format "%s" (list codesys feasible present))))

  (let ((gr (or (car (rassq codesys latin-unity-cset-codesys-alist))
		(and codesys
		     (eq (coding-system-type codesys) 'iso2022)
		     (coding-system-property codesys 'charset-g1)))))
    (when latin-unity-debug (message (format "%s" (list codesys gr))))
    (cond
     ((null codesys) nil)
     ((memq codesys latin-unity-ucs-list)
      codesys)
     ;; this is just an optimization, as the next arm should catch it
     ;; note we can assume ASCII here, as if GL is JIS X 0201 Roman,
     ;; GR will be JIS X 0201 Katakana
     ((and (/= (cdr present) 0)
	   (/= (car present) 0)
	   (= (get 'ascii 'latin-unity-flag-bit) (cdr present))
	   (= (get gr 'latin-unity-flag-bit 0) (car present)))
      codesys)
     ;; we represent everything in the buffer with remapping
     ((and (/= (logand (get 'ascii 'latin-unity-flag-bit) (cdr feasible)) 0)
	   (/= (logand (get gr 'latin-unity-flag-bit 0) (car feasible)) 0))
      (when latin-unity-debug (message "trying remap"))
      (latin-unity-remap-region begin end gr codesys no-error))
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

  (interactive
   (let ((begin (region-beginning))
	 (end (region-end)))
     (list begin end
	   ;; #### Abstract this to handle both charset and coding system
	   (let ((cs (intern (completing-read "Current character set: "
					      obarray #'find-charset))))
	     (while (not (find-charset cs))
	       (setq cs (latin-unity-guess-charset cs))
	       (cond ((not (find-charset cs))
		      (setq cs (intern (completing-read
					"Oops.  Current character set: "
					obarray #'find-charset))))
		     ((y-or-n-p (format "Guessing %s " cs)) cs)
		     (t (setq cs nil))))
	     cs)
	   (let ((cs (intern (completing-read "Desired character set: "
					      obarray #'find-charset))))
	     (while (not (find-charset cs))
	       (setq cs (latin-unity-guess-charset cs))
	       (cond ((not (find-charset cs))
		      (setq cs (intern (completing-read
					"Oops.  Desired character set: "
					obarray #'find-charset))))
		     ((y-or-n-p (format "Guessing %s " cs)) cs)
		     (t (setq cs nil))))
	     cs))))

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

  (interactive
   (let ((begin (region-beginning))
	 (end (region-end)))
     (list begin end
	   ;; #### Abstract this to handle both charset and coding system
	   (let ((cs (intern (completing-read "Current coding system: "
					      obarray #'find-coding-system))))
	     (while (not (find-coding-system cs))
	       (setq cs (latin-unity-guess-coding-system cs))
	       (cond ((not (find-coding-system cs))
		      (setq cs (intern (completing-read
					"Oops.  Current coding system: "
					obarray #'find-coding-system))))
		     ((y-or-n-p (format "Guessing %s " cs)) cs)
		     (t (setq cs nil))))
	     cs)
	   (let ((cs (intern (completing-read "Desired coding system: "
					      obarray #'find-coding-system))))
	     (while (not (find-coding-system cs))
	       (setq cs (latin-unity-guess-coding-system cs))
	       (cond ((not (find-coding-system cs))
		      (setq cs (intern
				(completing-read
				 "Oops.  Desired coding system: "
				 obarray #'find-coding-system))))
		     ((y-or-n-p (format "Guessing %s " cs)) cs)
		     (t (setq cs nil))))
	     cs))))

  (encode-coding-region begin end wrong-cs)
  (decode-coding-region begin end right-cs))


;;;###autoload
(defun latin-unity-remap-region (begin end character-set
				 &optional coding-system no-error)
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
`latin-unity-ucs-list' to change this.

This function remaps characters that are artificially distinguished by Mule
internal code.  It may change the code point as well as the character set.
To recode characters that were decoded in the wrong coding system, use
`latin-unity-recode-region'."

  (interactive
   (let ((begin (region-beginning))
	 (end (region-end)))
     (list begin end
	   ;; #### Abstract this to handle both charset and coding system
	   (let ((cs (intern (completing-read "Character set: "
					      obarray #'find-charset))))
	     (while (not (find-charset cs))
	       (setq cs (latin-unity-guess-charset cs))
	       (cond ((not (find-charset cs))
		      (setq cs (intern
				(completing-read "Oops.  Character set: "
						 obarray #'find-charset))))
		     ((y-or-n-p (format "Guessing %s " cs)) cs)
		     (t (setq cs nil))))
	     cs))))

  (save-excursion
    (save-restriction
      ;; #### we're not even gonna try if we're in an auto-save
      (when begin
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
	      (delete-char 1))))

	(let ((remaining (delq character-set
			       (delq 'ascii
				     ;; #### this function is slow!
				     (charsets-in-region begin end)))))
	  (when (or remaining latin-unity-debug)
	    (message (format "Could not remap characters from %s to %s"
			     remaining character-set)))
	  (cond ((memq coding-system latin-unity-ucs-list) coding-system)
		((null remaining)
		 (or coding-system
		     (cdr (assq coding-system latin-unity-cset-codesys-alist))
		     ;; #### Is this the right thing to do here?
		     t))
		(t (unless no-error (error 'args-out-of-range
					   "Remap failed; can't save!")))))
	))))

(defun latin-unity-guess-charset (candidate)
  "Guess a charset based on the symbol CANDIDATE.

CANDIDATE itself is not tried as the value.

Uses the natural mapping in `latin-unity-cset-codesys-alist', and the values
in `latin-unity-charset-alias-alist'."
  (let ((charset
	 (cond ((not (symbolp candidate))
		(error 'wrong-type-argument "Not a symbol: " candidate))
	       ((find-coding-system candidate)
		(car (rassq candidate latin-unity-cset-codesys-alist)))
	       (t (cdr (assq  candidate latin-unity-charset-alias-alist))))))
    (when (find-charset charset)
      charset)))

(defun latin-unity-guess-coding-system (candidate)
  "Guess a coding system based on the symbol CANDIDATE.

CANDIDATE itself is not tried as the value.

Uses the natural mapping in `latin-unity-cset-codesys-alist', and the values
in `latin-unity-coding-system-alias-alist'."

  (let ((coding-system
	 (cond ((not (symbolp candidate))
		(error 'wrong-type-argument "Not a symbol: " candidate))
	       ((find-charset candidate)
		(car (assq candidate latin-unity-cset-codesys-alist)))
	       (t (cdr (assq candidate
			     latin-unity-coding-system-alias-alist))))))
    (when (find-coding-system coding-system)
      coding-system)))


;; tests
(defun latin-unity-test ()
  "Test the latin-unity package.  Requires mule-ucs, but easy to generalize.

You need to run `latin-unity-install' first."

  (interactive)

  ;; save variables we intend to trash
  (put 'latin-unity-test 'ucs-list latin-unity-ucs-list)
  (put 'latin-unity-test 'preapproved
       latin-unity-preapproved-coding-system-list)
  (put 'latin-unity-test 'preferred
       latin-unity-preferred-coding-system-list)
  (put 'latin-unity-test 'default buffer-file-coding-system)

  (pop-to-buffer "*latin-unity test*")
  (erase-buffer)

  ;; #### need to check error conditions and stuff too
  (mapc (lambda (test)
	  (let ((coding-system (car test))
		(string (cdr test)))
	    (setq buffer-file-coding-system coding-system)
	    (goto-char (point-max))
	    (let ((a (point)))
	      (insert string)
	      (let ((b (point))
		    (coding-system-for-read coding-system))
		(insert "\n")
		(write-region a b "/tmp/test-latin-unity")
		(goto-char (+ (point)
			      (second (insert-file-contents
				       "/tmp/test-latin-unity"))))
		(if (string= (buffer-substring a b)
			     (buffer-substring (1+ b) (point)))
		    (insert "\nPassed.\n")
		  (insert "\nFailed.\n"))))))
	(list
	 ;; Erwan David's example
	 (cons 'iso-8859-15
	       (concat "test accentu"
		       (list (make-char 'latin-iso8859-1 #x69))
		       ", avec "
		       (list (make-char 'latin-iso8859-15 #x24))
		       "uro."))
	 ))

  ;; restore variables we trashed
  (setq latin-unity-ucs-list (get 'latin-unity-test 'ucs-list))
  (setq latin-unity-preapproved-coding-system-list
	(get 'latin-unity-test 'preapproved))
  (setq latin-unity-preferred-coding-system-list
	(get 'latin-unity-test 'preferred))
  (setq buffer-file-coding-system (get 'latin-unity-test 'default))
  )


;;;###autoload  
(defun latin-unity-example ()
  "An example of the latin-unity package.

At present it just makes a multilingual buffer.  To test, setq
buffer-file-coding-system to some value, make the buffer dirty (eg
with RET BackSpace), and save."

  (interactive)
  (switch-to-buffer (get-buffer-create "latin-unity example"))
  (erase-buffer)
  (insert "From here ...\n")
  (insert "Latin-1: f")
  (insert (make-char 'latin-iso8859-1 #xFC)) ; u umlaut, also in Latin-2
  (insert "r\n\nLatin-2: Nik")		; my apologies if I misremembered
  (insert (make-char 'latin-iso8859-2 57)) ; s caron, not in Latin-1
  (insert ?i)
  (insert (make-char 'latin-iso8859-2 102)) ; c acute, not in Latin-1
  (insert "\n... to here is representable in Latin-2 but not Latin-1.\n")
  (insert (make-char 'latin-iso8859-1 255)) ; y daieresis, not in Latin-2
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

Note:  the *install functions are interactive---you can execute with M-x.
I wrote them as above so you can C-x C-e them in this buffer.
"))

;;; end of latin-unity.el
