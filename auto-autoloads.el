;;; DO NOT MODIFY THIS FILE
(if (featurep 'latin-unity-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "latin-unity/_pkg.el")

(package-provide 'latin-unity :version 0.9 :type 'regular)

;;;***

;;;### (autoloads (latin-unity-test latin-unity-sanity-check latin-unity-region-feasible-representations latin-unity-buffer-feasible-representations) "latin-unity" "latin-unity/latin-unity.el")

(autoload 'latin-unity-buffer-feasible-representations "latin-unity" "\
Apply latin-unity-region-feasible-representations to the current buffer." t nil)

(autoload 'latin-unity-region-feasible-representations "latin-unity" "\
Return character sets that can represent the text from BEGIN to END in BUF.

BUF defaults to the current buffer.  Called interactively, will be
applied to the region.  Function assumes BEGIN <= END.

The return value is a cons.  The car is the list of character sets
that can individually represent all of the non-ASCII portion of the
buffer, and the cdr is the list of character sets that can
individually represent all of the ASCII portion." t nil)

(autoload 'latin-unity-sanity-check "latin-unity" "\
Check if `buffer-file-coding-system' can represent the region START to END.

FILENAME, APPEND, VISIT, and LOCKNAME are ignored.

Return nil if buffer-file-coding-system is not (ISO-2022-compatible) Latin.
If buffer-file-coding-system is safe for the charsets actually present in
the buffer, return it.  Otherwise, ask the user to choose a coding system,
and return that.

This function does _not_ do the safe thing when buffer-file-coding-system is
nil (aka no-conversion).  It considers that \"non-Latin\", and passes it on
to the Mule detection mechanism.

This function is intended for use as a `write-region-pre-hook'." nil nil)

(autoload 'latin-unity-test "latin-unity" "\
Test the latin-unity package.

At present it just makes a multilingual buffer.  To test, setq
buffer-file-coding-system to some value, make the buffer dirty (eg
with RET BackSpace), and save." t nil)

;;;***

(provide 'latin-unity-autoloads)
