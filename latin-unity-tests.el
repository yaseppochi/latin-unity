;;; latin-tests-unity.el ---  Test the latin-unity package

;; Copyright (C) 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, charsets
;; Created: 2002 October 20
;; Last-modified: 2002 October 20

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
;; of a larger character set.  The latin-unity package provides functions
;; which determine the list of coding systems which can encode all of the
;; characters in the buffer.  This library tests the functionality.

;;  Requires mule-ucs, but easy to generalize.

;; 

;;; Code:

(defconst latin-unity-was-active
  (memq 'latin-unity-sanity-check write-region-pre-hook))

(unless latin-unity-was-active (latin-unity-install))

;; save variables we intend to trash
(put 'latin-unity-test 'ucs-list latin-unity-ucs-list)
(put 'latin-unity-test 'preapproved
     latin-unity-preapproved-coding-system-list)
(put 'latin-unity-test 'preferred
     latin-unity-preferred-coding-system-list)
(put 'latin-unity-test 'default buffer-file-coding-system)

(unwind-protect
    (progn
      (with-temp-buffer
	(setq latin-unity-preapproved-coding-system-list '(buffer-default))
	;; #### need to check error conditions and stuff too
	;; Successful remapping
	(mapc (lambda (test)
		;; The way we should do the successful tests is to have
		;; two coding systems, the buffer's current one, and the
		;; target.  We set/bind preapproved-coding-system-list to
		;; the target.
		;; better yet, target should be the preapproved list
		(let ((current (car test))
		      (target (cadr test))
		      (string (caddr test)))
		  (setq buffer-file-coding-system current)
		  (setq latin-unity-preapproved-coding-system-list
			(list target))
		  (goto-char (point-max))
		  (let ((a (point)))
		    (insert string)
		    (let ((b (point))
			  (coding-system-for-read target))
		      (insert "\n")
		      (write-region a b "/tmp/test-latin-unity")
		      (goto-char (+ (point)
				    (second (insert-file-contents
					     "/tmp/test-latin-unity"))))
		      (eval
		       `(Assert (string= ,(buffer-substring a b)
					 ,(buffer-substring (1+ b)
							    (point)))))))))
	      (list
	       ;; Erwan David's example
	       (list 'iso-8859-1 'iso-8859-15
		     (format "test accentu%c, avec %curo."
			     ;; LATIN SMALL LETTER E WITH ACUTE 
			     (make-char 'latin-iso8859-1 #x69)
			     ;; EURO SIGN
			     (make-char 'latin-iso8859-15 #x24)))
	       ;; We had problems with plain Latin-1 :-(
	       (list 'iso-8859-1 'iso-8859-1
		     (format "Ville Skytt%c  <ville.skytta@xemacs.org>"
			     ;; LATIN SMALL LETTER A WITH DIAERESIS
			     (make-char 'latin-iso8859-1 #x64)))
	       (list 'iso-8859-1 'iso-8859-2
		     (format "f%cr Hrvoje Nik%ci%c"
			     ;; LATIN SMALL LETTER U WITH DIAERESIS
			     (make-char 'latin-iso8859-1 #x7C)
			     ;; LATIN SMALL LETTER S WITH CARON
			     (make-char 'latin-iso8859-2 57)
			     ;; LATIN SMALL LETTER C WITH ACUTE
			     (make-char 'latin-iso8859-2 102)))
	       (list 'iso-8859-1 'utf-8
		     (format "f%cr Hrvoje, %cclept Nik%ci%c"
			     (make-char 'latin-iso8859-1 #xFC)
			     (make-char 'latin-iso8859-1 255)
			     (make-char 'latin-iso8859-2 57)
			     ;; LATIN SMALL LETTER Y WITH DIAERESIS
			     (make-char 'latin-iso8859-2 102)))
	       )
	     ))

      ;; do interactive tests
      (when (interactive-p)
	(message "No interactive tests yet."))

      )

  ;; unwind forms

  ;; restore variables we trashed
  (setq latin-unity-ucs-list (get 'latin-unity-test 'ucs-list))
  (setq latin-unity-preapproved-coding-system-list
	(get 'latin-unity-test 'preapproved))
  (setq latin-unity-preferred-coding-system-list
	(get 'latin-unity-test 'preferred))
  (setq buffer-file-coding-system (get 'latin-unity-test 'default))

  ;; conditionally uninstall
  (unless latin-unity-was-active (latin-unity-uninstall)))

;;; end of latin-unity-tests.el
