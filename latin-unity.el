;;; latin-unity.el --- Identify equivalent characters in the ISO Latin sets

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 January 17
;; Last-modified: 2003 August 9

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

;; Provides the 'iso-8859-13 and 'iso-8859-15 coding systems if undefined.

;;; Code:

(provide 'latin-unity)


;;; Requires

(require 'latin-unity-vars)
(require 'latin-unity-latin7)		; define iso-8859-13
;; uncomment to add support for ISO 8859/14
;(require 'latin-unity-latin8)		; define iso-8859-14
(require 'latin-unity-latin9)		; define iso-8859-15
;; uncomment to add support for ISO 8859/16
;(require 'latin-unity-latin10)		; define iso-8859-16
(if (or (fboundp 'character-to-unicode)	; XEmacs  post-21.5.5
	(fboundp 'char-to-ucs))		; Mule-UCS already loaded
    (require 'latin-unity-tables "latin-unity-utils")
  (require 'latin-unity-tables))	; doesn't require Unicode support


;;; User customization

(defgroup latin-unity nil
  "Handle equivalent ISO-8859 characters properly (identify them) on output."
  :group 'mule)

(defcustom latin-unity-preapproved-coding-system-list
  '(buffer-default preferred)
  "*List of coding systems used without querying the user if feasible.

The first feasible coding system in this list is used.  The special values
'preferred and 'buffer-default may be present:

  buffer-default  Use the coding system used by `write-region', if feasible.
  preferred       Use the coding system specified by `prefer-coding-system'
                  if feasible.

\"Feasible\" means that all characters in the buffer can be represented by
the coding system.  Coding systems in `latin-unity-ucs-list' are always
considered feasible.  Other feasible coding systems are computed by
`latin-unity-representations-feasible-region'.

Most users will want at least one ISO 8859 coding system in this list, as
otherwise pure ASCII files will not be preapproved.  (This is a bug, due
to the limitation of applicability of this package to Latin and universal.
The condition that an ISO 8859 coding system be included will be satisfied
implicitly by 'buffer-default or 'preferred for most users, but it can be
annoying for users of ISO 2022 or EUC coding systems.)

Note that the first universal coding system in this list shadows all other
coding systems.  In particular, if your preferred coding system is a universal
coding system, and 'preferred is a member of this list, latin-unity will
blithely convert all your files to that coding system.  This is considered a
feature, but it may surprise most users.  Users who don't like this behavior
should move 'preferred to `latin-unity-preferred-coding-system-list'."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-preferred-coding-system-list
  '(iso-8859-1 iso-8859-15 iso-8859-2 iso-8859-3 iso-8859-4 iso-8859-9)
  "*List of coding systems suggested the user if feasible.

If no coding system in `latin-unity-preapproved-coding-system-list' is
feasible, this list will be recommended to the user, followed by the
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

Order matters; coding systems earlier in the list will be preferred
when recommending a coding system.  These coding systems will not be
used without querying the user (unless they are also present in
`latin-unity-preapproved-coding-system-list'), and follow the
`latin-unity-preferred-coding-system-list' in the list of suggested
coding systems.

If none of the preferred coding systems are feasible, the first in
this list will be the default.

Notes on certain coding systems:  If `escape-quoted' is not a member of
this list, you will be unable to autosave files or byte-compile Mule
Lisp files.

latin-unity does not try to be \"smart\" about general ISO 2022 coding
systems, such as ISO-2022-JP.  (They are not recognized as equivalent
to `iso-2022-7'.)  If your preferred coding system is one of these,
consider adding it to `latin-unity-ucs-list'.  However, this will
typically have the side effect that (eg) ISO 8859/1 files will be
saved in 7-bit form with ISO 2022 escape sequences."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-charset-alias-alist
  '((latin-1 . latin-iso8859-1)
    (latin-2 . latin-iso8859-2)
    (latin-3 . latin-iso8859-3)
    (latin-4 . latin-iso8859-4)
    (latin-5 . latin-iso8859-9)
    (latin-7 . latin-iso8859-13)
    (latin-9 . latin-iso8859-15)
    (latin-10 . latin-iso8859-16))
  "*Alist mapping aliases (symbols) to Mule charset names (symbols).

Both aliases and names are symbols.
Aliases of unsupported charsets will be treated as if the charset name had
been entered directly (normally an error will be signaled)."
  :type '(repeat (cons symbol symbol))
  :group 'latin-unity)

(defcustom latin-unity-coding-system-alias-alist nil
  "*Alist mapping aliases to Mule coding system names.

Both aliases and names are symbols.
Aliases of unsupported coding systems will be treated as if the coding system
name had been entered directly (normally an error will be signaled)."
  :type '(repeat (cons symbol symbol))
  :group 'latin-unity)

;; Needed because 'iso-8859-1 is type 'no-conversion, NOT type 'iso2022
(defcustom latin-unity-iso-8859-1-aliases '(iso-8859-1)
  "List of coding systems to be treated as aliases of ISO 8859/1.

Not a user variable.  Customize input of coding systems or charsets via
`latin-unity-coding-system-alias-alist' or `latin-unity-charset-alias-alist'."
  :type '(repeat symbol)
  :group 'latin-unity)

(defcustom latin-unity-coding-system-list-buffer
  " *latin-unity coding system preferences*"
  "Name of buffer used to display codings systems by priority."
  :type 'string
  :group 'latin-unity)

(defcustom latin-unity-hack-cookies-enabled-p t
  "If non-nil, `latin-unity-sanity-check' validates coding cookies."
  :type 'boolean
  :group 'latin-unity)

(defcustom latin-unity-like-to-live-dangerously nil
  "Convert failure to remap buffer from error to warning."
  :type 'boolean
  :group 'latin-unity)

(defcustom latin-unity-may-set-coding-flag 'ask
  "When may latin-unity reset `buffer-file-coding-system'?

    nil           never change it, return silently
    t             always change it, return silently
    regrets-only  never change it, warn that coding system is inappropriate
    warn          always change it, warn user of change
    ask           ask user's permission."
  :type '(choice
	  (const nil  :doc "never change, return silently")
	  (const t    :doc "always change, return silently")
	  (const regrets-only
		 :doc "never change, warn that coding system is inappropriate")
	  (const warn :doc "always change, warn user of change")
	  (const ask  :doc "ask user's permission"))
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

;; Internal variables

(defvar latin-unity-coding-cookies-found 0
  "Internal variable.")

;; Utilities

;; Mule is _so_ losing.  Coding system objects should generally be hidden
;; from lookup functions, etc.
(defsubst latin-unity-massage-name (x coding-system)
  "Return the real name of X, a symbol.

X may be 'buffer-default, 'preferred, a coding system object, or a symbol
naming a coding system.  CODING-SYSTEM determines the interpretation of
'buffer-default, and `coding-priority-list' that of  'preferred."
  (coding-system-name
   (cond ((eq x 'buffer-default) coding-system)
	 ((eq x 'preferred)
	  (coding-system-name ; #### nil arg -> binary, is this OK?
	   (coding-category-system (car (coding-priority-list)))))
	 (t x))))

;; Think about using l-u-massage-name.
;; Maybe (if (coding-system-alias-p cs) (coding-system-aliasee cs) cs)?
;; But watch out, check what happens if an eol variant is derived from an
;; alias.  Also, note that `define-coding-system-alias' is relatively recent.
;; Most "aliases" (including all the ones I know for 'iso-8859-1 :-( ) are
;; made with `copy-coding-system', not d-c-s-a.
(defsubst latin-unity-base-name (cs)
  "Return the base name of the coding system object or symbol CS.

The base name is a symbol naming the similar coding system with no EOL
convention."
  (coding-system-name (coding-system-base (find-coding-system cs))))

(defun latin-unity-buffer-charsets-string (buffer &optional begin end)
  "Insert a string listing the charsets found in BUFFER in the current buffer.

Returns the list of charsets.

By default the entire buffer is considered (and narrowing is ignored).
Optional arguments BEGIN and END may be provided to restrict the region
considered.  The returned string is prefixed with a single space.

This is a debugging function; don't depend on its behavior."
  (mapc (lambda (cs) (insert (format " %s" cs)))
	(save-excursion
	  (set-buffer buffer)
	  (save-restriction
	    (widen)
	    (let ((begin (or begin (point-min)))
		  (end (or end (point-max))))
	      ;; this function is slow!
	      (charsets-in-region begin end))))))

(defsubst latin-unity-charset-feasible-system (charset bvector buffer-default)
  "Return a feasible coding-system based on CHARSET and BVECTOR, or nil.

BVECTOR is a bit vector encoding feasible Latin charsets (ie, not ASCII).
If CHARSET is feasible, look it up in `latin-unity-cset-codesys-alist',
otherwise return nil."
  (when latin-unity-debug (message "%s" charset))
  (let ((sys (cdr (assq charset latin-unity-cset-codesys-alist))))
    (and (memq sys (mapcar (lambda (x)
			     (latin-unity-massage-name x buffer-default))
			   latin-unity-preferred-coding-system-list))
	 ;; User may have specified systems unavailable in this XEmacs
	 (find-coding-system sys)
	 (/= 0 (logand (get charset 'latin-unity-flag-bit) bvector))
	 sys)))

(defsubst latin-unity-coding-system-latin-charset (coding-system)
  "Return the Latin charset used by CODING-SYSTEM, or nil, if none."
  (or (car (rassq coding-system latin-unity-cset-codesys-alist))
      (and coding-system
	   (eq (coding-system-type coding-system) 'iso2022)
	   (coding-system-property coding-system 'charset-g1))))


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
	;; Should arrange to only display excluded ones!
	(insert "\nAll coding systems:\n ")
	(setq start (point))
	(mapc (lambda (codesys) (insert (format " %s" codesys)))
	      (coding-system-list))
	(fill-region start (point))))))

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
;; Easy optimizations would be to (1) append observed characters to the
;; characters-to-skip string, and (2) to fail immediately on detection of
;; a non-Latin character.  (Avoid kludgy implementations of (2) which don't
;; admit generalization as we add more character sets to unify.)
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
	;; autosave may pass us nil arguments.  Force both to be nil, or
	;; both to be integer-or-marker-p.
	(let ((begin (or begin (and (null end) (point-min))))
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
;; charsets-in-region unusable; it is in Lisp and quite slow.  :-(
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
	;; autosave may pass us nil arguments.  Force both to be nil, or
	;; both to be integer-or-marker-p.
	;; #### implementation differs from l-u-r-f-r
	(narrow-to-region (or begin (and (null end) (point-min)))
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

(defun latin-unity-maybe-set-coding-system (coding-system current)
  "Set the `buffer-file-coding-system' to CODING-SYSTEM if not same as CURRENT.
Exact behavior depends on `latin-unity-may-set-coding-flag'.
Return value is not currently useful."
  ;; we'd like the `message's below to be `warn', but `warn' is too obtrusive
  ;(message "new %s; current %s" coding-system current)
  (message "new %s; current %s" (latin-unity-base-name coding-system)
	                        (latin-unity-base-name current))
  (case latin-unity-may-set-coding-flag
    ((nil))
    ((t)
     (set-buffer-file-coding-system coding-system))
    ((regrets-only)
     (message "Specified coding system used to save, but default not changed."))
    ((warn)
     (set-buffer-file-coding-system coding-system)
     (message "Specified coding system used to save, and default changed."))
    ((ask)
     (cond ((eq (latin-unity-base-name coding-system)
		(latin-unity-base-name current))
	    (message (concat "Specified coding system used."
			     "  Default has same base and was not changed.")))
	   ((y-or-n-p (format "Change default coding system to %s? "
			      coding-system))
	    (set-buffer-file-coding-system coding-system)
	    (message "Specified coding system used, and default changed."))
	   (t (message
	       "Specified coding system used, but default not changed."))))
    (otherwise
     (message (format "Unknown value for latin-unity-may-set-coding-flag: %s."
		      latin-unity-may-set-coding-flag)))))

;; #### I see nothing useful to be done with APPEND.
;; FILENAME, VISIT, or LOCKNAME could be used to default the coding system,
;; but this would conflict with the semantics of `write-region'.
;; #### The efficiency of this function can clearly be improved.
;; #### This function should be set up to call the check functions in a
;; condition-case, and call out to error handlers.  Then tests could be
;; written more easily.

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
and return that.  If the user is asked to choose, possibly set the
`buffer-file-coding-system' depending on `latin-unity-may-set-coding-flag'.

This function does _not_ do the safe thing when `buffer-file-coding-system'
is nil (= no-conversion).  It considers that \"non-Latin\", and passes it on
to the Mule detection mechanism.  This could result in corruption.  So avoid
setting `buffer-file-coding-system' to nil or 'no-conversion or 'binary.

This function is intended for use as a `write-region-pre-hook'.  It does
nothing except return nil if `write-region' handlers are inhibited."

  ;; don't do anything if we're in a `write-region' handler
  ;; #### is nil the right return value if we are?
  (if (eq inhibit-file-name-operation 'write-region)
      nil
    (prog1
    (let ((buffer-default
	   ;; theoretically we could look at other write-region-prehooks,
	   ;; but they might write the buffer and we lose bad
	   (coding-system-name	; #### nil arg -> binary, is this OK?
	    (or coding-system
		buffer-file-coding-system
		(find-file-coding-system-for-write-from-filename filename))))
	  ;; check what representations are feasible
	  ;; csets == compatible character sets as (latin . ascii)
	  (csets (latin-unity-representations-feasible-region begin end))
	  ;; as an optimization we also check for what's in the buffer
	  ;; psets == present in buffer character sets as (latin . ascii)
	  (psets (latin-unity-representations-present-region begin end)))

	(when latin-unity-debug (message "%s %s" csets psets) (sit-for 1))

	(cond
	 ;; try the preapproved systems
	 ((catch 'done
	    (let ((systems latin-unity-preapproved-coding-system-list))
	      ;; while always returns nil
	      (while systems
		(let ((sys (latin-unity-massage-name (car systems)
						     buffer-default)))
		  (when latin-unity-debug (message "sys is %s" sys))
		  (when (latin-unity-maybe-remap begin end sys
						 csets psets t)
		    (when latin-unity-debug (message "throwing %s" sys))
		    (throw 'done sys))
		  (setq systems (cdr systems)))))))
	 ;; ask the user about the preferred systems
	 ;; #### RFE: It also would be nice if the offending characters
	 ;; were marked in the buffer being checked.  Evidently GNU Emacs
	 ;; 20.x could do this.
	 (t (let* ((recommended
		    (latin-unity-recommend-representation begin end csets
							  buffer-default))
		   (codesys (car recommended))
		   ;(charset (cdr recommended)) ; unused?
		   )
	      (when latin-unity-debug (message "%s" recommended))
	      ;; compute return
	      (cond

	       ;; universal coding systems
	       ;; #### we might want to unify here if the codesys is ISO 2022
	       ;; but we don't have enough information to decide
	       ((memq (latin-unity-base-name codesys) latin-unity-ucs-list)
		(unless (eq (latin-unity-base-name codesys)
			    (latin-unity-base-name buffer-default))
		  (latin-unity-maybe-set-coding-system codesys buffer-default))
		codesys)

	       ;; ISO 2022 (including ISO 8859) compatible systems
	       ;; #### maybe we should check for G2 and G3 sets
	       ;; note the special case is necessary, as 'iso-8859-1 is NOT
	       ;; type 'iso2022, it's type 'no-conversion
	       ((or (memq (latin-unity-base-name codesys)
			  latin-unity-iso-8859-1-aliases)
		    (eq (coding-system-type codesys) 'iso2022))
		;; #### make sure maybe-remap always returns a coding system
		;; #### I thought about like-to-live-dangerously here,
		;; but first make sure make sure maybe-remap returns nil
		(setq codesys
		      (latin-unity-massage-name codesys buffer-default))
		(when (and (latin-unity-maybe-remap begin end codesys
						    csets psets nil)
			   (not (eq (latin-unity-base-name codesys)
				    (latin-unity-base-name buffer-default))))
		  (latin-unity-maybe-set-coding-system codesys buffer-default)
		  codesys))

	       ;; other coding systems -- eg Windows 125x, KOI8?
	       ;; #### unimplemented

	       ;; no luck, pass the buck back to `write-region'
	       (latin-unity-like-to-live-dangerously
		(warn (concat "Passing to default coding system,"
			      " data corruption likely"))
		(ding)
		nil)
	       (t (error
		   'coding-system-error
		   "couldn't find a coding system to encode all characters"))
	       )))
	 ))
    (when latin-unity-hack-cookies-enabled-p
      (setq latin-unity-coding-cookies-found 0)
      (latin-unity-hack-cookies-prop-line)
      (latin-unity-hack-cookies-last-page)))))


;; #### maybe this is what we want to test?  add a no-ask flag.
(defun latin-unity-recommend-representation (begin end feasible buffer-default
					     &optional buffer)
  "Recommend a representation for BEGIN to END from FEASIBLE in BUFFER.

Returns a cons of a coding system (which can represent all characters in
BUFFER) and a charset (to which all non-ASCII characters in BUFFER can be
remapped.  (The former will be nil only if `latin-unity-ucs-list' is nil.)

FEASIBLE is a bitvector representing the feasible character sets.
BUFFER defaults to the current buffer."

  ;; interactive not useful because of representation of FEASIBLE
  (unless buffer (setq buffer (current-buffer)))

  (let (recommended)
    (save-excursion
      (pop-to-buffer (get-buffer-create latin-unity-help-buffer) t)
      (erase-buffer)
      (insert (format "\
Choose a coding system to save buffer %s.
All preapproved coding systems %s
fail to appropriately encode some of the characters present in the buffer."
		      (buffer-name buffer)
		      (mapcar (lambda (x)
				(if (memq x '(preferred buffer-default))
				    (format "%s==%s" x
					    (latin-unity-massage-name
					     x buffer-default))
				  x))
			      latin-unity-preapproved-coding-system-list)))
      ;; #### break this out into a separate function for testing
      (when latin-unity-debug
	(insert "  Character sets in the buffer are:\n\n   ")
	(latin-unity-buffer-charsets-string buffer))
      (insert "

Please pick a coding system.  The following are recommended because they can
encode any character in the buffer:

   ")
      (mapc (lambda (cs)
	      (let ((sys (latin-unity-charset-feasible-system cs
							      (car feasible)
							      buffer-default)))
		(when sys
		  (unless recommended (setq recommended (cons sys cs)))
		  (insert (format " %s" sys)))))
	    latin-unity-character-sets)
      ;; universal coding systems
      (mapc (lambda (sys)
	      ;; User may have specified systems unavailable in this XEmacs
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

For a list of coding systems, quit this command and invoke
`list-coding-systems'.")
      (goto-char (point-min))
      ;; `read-coding-system' never returns a non-symbol
      (let ((val (read-coding-system (format "Coding system [%s]: "
					     (car recommended))
				     (car recommended))))
	(delete-window)
	(if (eq val (car recommended))
	    recommended
	  (cons val (latin-unity-coding-system-latin-charset val)))))))

;; this could be a flet in latin-unity-sanity-check
;; -- no, this is what we want to regression test?
;; #### this function's interface needs to change, s/codesys/charset/
;; #### did you update all calls?
;; #### did you update all docs?
(defun latin-unity-maybe-remap (begin end codesys feasible
				&optional present no-error)
  "Try to remap from BEGIN to END to CODESYS.  Return nil on failure.

Return CODESYS on success.  CODESYS is a real coding system or nil.
FEASIBLE is a cons of bitvectors indicating the set of character sets which
can represent all non-ASCII characters and ASCII characters, respectively,
in the current buffer.
PRESENT is a cons of bitvectors indicating the set of non-ASCII and ASCII
character sets, respectively, present in the current buffer.

Pass NO-ERROR to `latin-unity-remap-region'."

  ;; may God bless and keep the Mule ... far away from us!
  ;; #### We can canonicalize here with impunity.  Transformations of the
  ;; characters in the buffer will not change the representation of newline.
  ;; It's the return value to -sanity-check that possibly needs EOL.
  (when codesys (setq codesys (latin-unity-base-name codesys)))
  (when (memq codesys latin-unity-iso-8859-1-aliases)
    (setq codesys 'iso-8859-1))

  (when latin-unity-debug
    (message (format "%s" (list codesys feasible present))))

  (let ((gr (latin-unity-coding-system-latin-charset codesys)))
    (when latin-unity-debug (message (format "%s" (list codesys gr))))
    (cond
     ((null codesys) nil)
     ;; #### this should be replaced by (latin-unity-ucs-p cs), etc!!
     ((memq codesys latin-unity-ucs-list) codesys)
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
	   (latin-unity-read-coding-system-or-charset
	    'charset
	    "Current character set: ")
	   (latin-unity-read-coding-system-or-charset
	    'charset
	    "Desired character set: "))))

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
	   (latin-unity-read-coding-system-or-charset
	    'coding-system
	    "Current coding system: ")
	   (latin-unity-read-coding-system-or-charset
	    'coding-system
	    "Desired coding system: "))))

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
	   (latin-unity-read-coding-system-or-charset 'charset
						      "Character set: "))))

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


(defun latin-unity-read-coding-system-or-charset (target-type &optional prompt)
  "Handle user input of coding system or charset names with guessing.

Returns a coding-system name or charset name according to TARGET-TYPE.
Prompt with optional PROMPT, which defaults to \"Enter TARGET-TYPE: \".

Uses `latin-unity-guess-coding-system' to \"guess\" an appropriate coding
system from a charset name and vice versa (via `latin-unity-guess-charset').
These functions also consult alias lists."

  (unless (memq target-type '(coding-system charset))
    (error 'args-out-of-range "wanted 'coding-system or 'charset"
	   target-type))

  (let ((prompt (if (stringp prompt)
		    prompt
		  (format "Enter %s name: " target-type))))
    (flet ((typecheck (x)
	     (funcall (intern (format "find-%s" target-type)) x))
	   (guess (x)
	     (funcall (intern (format "latin-unity-guess-%s" target-type)) x)))
      (let ((obj (intern (completing-read prompt obarray #'typecheck))))
	(while (not (typecheck obj))
	  (setq obj (guess obj))
	  (cond ((not (typecheck obj))
		 (setq obj (intern (completing-read (concat "Oops!  " prompt)
						    obarray #'typecheck))))
		((y-or-n-p (format "Guessing %s. OK? " obj)) obj)
		(t (setq obj t))))
	obj))))


(defun latin-unity-guess-charset (candidate)
  "Guess a charset based on the symbol CANDIDATE.

CANDIDATE itself is not tried as the value.

Uses the natural mapping in `latin-unity-cset-codesys-alist', and the values
in `latin-unity-charset-alias-alist'."
  (let* ((indirect (cdr (assq candidate
			      latin-unity-coding-system-alias-alist)))
	 (charset
	  (cond ((not (symbolp candidate))
		 (error 'wrong-type-argument "Not a symbol" candidate))
		;; #### Use latin-unity-coding-system-latin-charset here?
		((find-coding-system candidate)
		 (car (rassq candidate latin-unity-cset-codesys-alist)))
		((find-coding-system indirect)
		 (car (rassq indirect latin-unity-cset-codesys-alist)))
		(t (cdr (assq  candidate latin-unity-charset-alias-alist))))))
    (when (find-charset charset)
      charset)))

(defun latin-unity-guess-coding-system (candidate)
  "Guess a coding system based on the symbol CANDIDATE.

CANDIDATE itself is not tried as the value.

Uses the natural mapping in `latin-unity-cset-codesys-alist', and the values
in `latin-unity-coding-system-alias-alist'.

Returns a symbol naming a coding system, or t to mean \"not a coding system\".
\(Horrible, but Mule interprets nil as a spelling of 'binary.)"

  (let* ((indirect (cdr (assq candidate latin-unity-charset-alias-alist)))
	 (coding-system
	  (cond ((not (symbolp candidate))
		 (error 'wrong-type-argument "Not a symbol" candidate))
		((find-charset candidate)
		 (cdr (assq candidate latin-unity-cset-codesys-alist)))
		((find-charset indirect)
		 (cdr (assq indirect latin-unity-cset-codesys-alist)))
		((cdr (assq candidate latin-unity-coding-system-alias-alist)))
		(t t))))
    (when (find-coding-system coding-system)
      coding-system)))

;; The logic for the latin-unity-hack-cookies-* searches is of course
;; ripp'd untimely from the hack-local-variables-* stuff in files.el.

;; #### probably this function should be hacked like the prop-line version
;; #### possibly this function should error on syntax errors (this doesn't
;;      prevent loading the file, but does prevent saving it)
(defun latin-unity-hack-cookies-last-page (&optional force)
  "Find a coding cookie in the local variables block on the last page.
Warn that XEmacs doesn't support coding cookies there.
If found and it differs from `buffer-file-coding-system', ask the user if
if the coding cookie should be changed.  If optional argument FORCE is
non-nil, fix the cookie without prompt.
#### Probably there should be an argument for the coding system to set."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (search-backward "" (max (- (point-max) 3000) (point-min)) 'move)
      (when (let ((case-fold-search t))
	      (and (search-forward "\\<Local Variables:" nil t)
		   (if (not (search-forward "\\<Local Variables:" nil t))
		       t
		     (warn "Two local variables sections found, ignoring.")
		     nil)))
	(let ((continue t)
	      problems prefix prefixlen suffix)
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  ;; Whitespace immediately preceding "local variables:" _is_ part
	  ;; of prefix; whitespace immediately following "local variables:"
	  ;; _is not_ part of suffix.  This means that you can have more
	  ;; indentation than "local variables:" has, but not less, while
          ;; you can pad the suffix with whitespace for nice alignment.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix (buffer-substring (point)
					     (progn (end-of-line) (point)))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))
	  (if prefix (setq prefixlen (length prefix)
			   prefix (regexp-quote prefix)))
	  (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display
		(re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (setq problems
			(list "Local variables entry is missing the prefix"))))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (let (var val head tail)
	      ;; #### need to use lisp-mode syntax table here;
	      ;; 2d arg of char-syntax
	      ;; #### need to hack eol here
	      (if (or (eql (char-after) ?:)
		      (not (eql (char-syntax (char-after)) ?_)))
		  (setq problems (cons "no local variable found" problems))
		(setq var (read (current-buffer)))
		;; magic cookies suck AND SWALLOW!
		;; #### I'll be damned if I'll worry about colon-terminated
		;; variable names here -- what if there are MULTIPLE COLONS?
		(let ((name (symbol-name var)))
		  (setq var (if (eql ?: (aref name (1- (length name))))
				(forward-char -1) ; back up over colon
				(substring name 0 -1)
			      name)))
		(skip-chars-forward " \t")
		(if (equal (char-after) ?:)
		    (forward-char 1)
		  (setq problems (cons "Missing colon in local variables entry"
				       problems))))
	      ;; end: probably doesn't have a value
	      (if (string-equal "end" var)
		  (setq continue nil)
		;; read the variable value.
		(skip-chars-forward " \t")
		;; check for effective end-of-line
		(if (or (eolp)
			(looking-at "\\s<\\|\\s1\\s2\\|\\s5\\s6"))
		    (setq problems (cons "no value found for local variable"
					 problems))
		  (setq head (point))
		  ;; #### this can error on a syntax error (eg "( . nil)")
		  (setq val (read (current-buffer)))
		  (setq tail (point))
		  (skip-chars-forward " \t")
		  (unless (if suffix (looking-at suffix) (eolp))
		    (setq problems
			  (cons "Local variables entry has incorrect suffix"
				problems))))
		(cond
		 (problems (while problems
			     (warn (car problems))
			     (setq problems (cdr problems)))
			   (setq continue nil))
		 ((string-match "coding" var)
		  (warn "Coding cookie in local variables unsupported")
		  (latin-unity-hack-coding-cookie val head tail force))))
	      )))))))

(defun latin-unity-hack-cookies-prop-line (&optional force)
  "Find a coding cookie in the first (non-shebang) line of the file.
If found and it differs from `buffer-file-coding-system', ask the user if
if the coding cookie should be changed.  If optional argument FORCE is
non-nil, fix the cookie without prompt.
#### Probably there should be an argument for the coding system to set."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r")	; does exec(2) gobble leading space?
      (let ((end (save-excursion
		   ;; If the file begins with "#!"
		   ;; (un*x exec interpreter magic), look
		   ;; for mode frobs in the first two
		   ;; lines.  You cannot necessarily
		   ;; put them in the first line of
		   ;; such a file without screwing up
		   ;; the interpreter invocation.
		   (end-of-line (if (looking-at "^#!") 2 1))
		   (point))))
	;; Parse the -*- line into the `result' alist.
	(let* ((stx (and (search-forward "-*-" end t) (point)))
	       ;; if there are more than two "-*-", use the first two
	       (etx (and stx (search-forward "-*-" end t) (- (point) 3))))
	  ;; insist on correct format and silently ignore otherwise
	  (cond
	   ((null stx) nil)
	   ((null etx) (warn "unterminated prop line, ignoring"))
	   (t (goto-char stx)
	      (while (re-search-forward "coding:[ \t]*" etx t)
		(goto-char (match-end 0))
		(let* ((head (point))
		       (val (read (current-buffer)))
		       (tail (point)))
		  (latin-unity-hack-coding-cookie val head tail force))))))))))

(defun latin-unity-hack-coding-cookie (value begin end &optional force)
  "Fixup a coding cookie.
If VALUE differs from `buffer-file-coding-system', ask the user if the
coding cookie found between BEGIN and END should be changed.  If optional
argument FORCE is non-nil, fix the cookie without prompt.
#### Probably there should be an argument for the coding system to set."
  (setq latin-unity-coding-cookies-found
	(1+ latin-unity-coding-cookies-found))
  (when (> latin-unity-coding-cookies-found 1)
    (warn "%d coding cookies found; you should have only one."
	  latin-unity-coding-cookies-found))
  (let ((bfcs (latin-unity-base-name buffer-file-coding-system)))
    (unless (eq value bfcs)
      (when (or force
		(y-or-n-p
		 (format "Incorrect coding cookie %S found.  Replace with %S? "
			 value bfcs)))
	(save-excursion
	  (goto-char begin)
	  (delete-region begin end)
	  (insert bfcs))))))

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
  (insert (make-char 'latin-iso8859-1 #xFC)) ; u diaeresis, also in Latin-2
  (insert "r\n\nLatin-2: Nik")		; my apologies if I misremembered
  (insert (make-char 'latin-iso8859-2 #xB9)) ; s caron, not in Latin-1
  (insert ?i)
  (insert (make-char 'latin-iso8859-2 #xE6)) ; c acute, not in Latin-1
  (insert "\n... to here is representable in Latin-2 but not Latin-1.\n")
  (insert (make-char 'latin-iso8859-1 #xFF)) ; y daieresis, not in Latin-2
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
