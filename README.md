# latin-unity

Last-modified: 2002 October 22

Converted-to-Markdown: 2021 August 12

Mule bogusly considers the various ISO-8859 extended character sets as
disjoint, when ISO 8859 itself clearly considers them to be subsets of
a larger character set.  For example, all of the Latin character sets
include NO-BREAK SPACE at code point 32 (ie, 0xA0 in an 8-bit code),
but the Latin-1 and Latin-2 NO-BREAK SPACE characters are considered
to be different by Mule, an obvious absurdity.

This package provides functions which determine the list of coding
systems which can encode all of the characters in the buffer, and
translate to a common coding system if possible.


## Basic usage:

To set up the package, simply put

(latin-unity-install)

in your init file.


## Availability:

anonymous CVS:
Get the __latin-unity__ module and build as usual.

WWW:
``ftp://ftp.xemacs.org/pub/xemacs/packages/latin-unity-VERSION-pkg.tar.gz``


## Features:

- If a buffer contains only ASCII and ISO-8859 Latin characters, the
  buffer can be "unified", that is treated so that all characters
  are translated to one charset that includes them all.  If the
  current buffer coding system is not sufficient, the package will
  suggest alternatives.  It prefers ISO-8859 encodings, but also
  suggests UTF-8 (if available; 21.4+ feature, currently requires
  Mule-UCS for 21.4 versions), ISO 2022 7-bit, or X Compound Text
  if no ISO 8859 coding system is comprehensive enough.

  It allows the user to use other coding systems, and the list of
  suggested coding systems is Customizable.

  __latin-unity__ will automatically adjust `buffer-file-coding-system` to
  the user's choice, on a Customizable basis.

  Optionally checks `-*- coding: codesys -*-` cookies for consistency.
  This only works for Emacs coding cookies; doesn't handle encoding
  attributes in XML declarations or HTML META tags yet.

  This probably also is useful out of the box if the buffer contains
  non-Latin characters in addition to a mixture of Latin characters.
  For example, it would reduce a buffer originally encoded in
  ISO-2022-JP (including Latin-1 characters) to ISO 8859/1 if all
  the Japanese were deleted.  (untested)

- ISO 8859/13 for XEmacs 21.4 and 21.1 (both untested).
  To get 'iso-8859-13 preferred to 'iso-8859-1 in autodetection, use
  (set-coding-category-system 'iso-8-1 'iso-8859-13).  (untested)
  Alternatively set language environment to Latin-7.

  If all you want is ISO 8859/13 support, you can `(require
  'latin-unity-latin7)' and `(require 'latin-latin7-input)', and not
  do `(latin-unity-install)'.

- ISO 8859/14 for XEmacs 21.4 and 21.1 (both untested).
  To get 'iso-8859-14 preferred to 'iso-8859-1 in autodetection, use
  (set-coding-category-system 'iso-8-1 'iso-8859-14).  (untested)
  Alternatively set language environment to Latin-8.

  If all you want is ISO 8859/14 support, you can `(require
  'latin-unity-latin8)', and not do `(latin-unity-install)'.
  Latin-8 does not yet have an input method.

- ISO 8859/15 for XEmacs 21.4 (moderately tested) and 21.1 (lightly
  tested), including binding the EuroSign keysym to ISO 8859/15 0xA4
  (as well as the other "new" keysyms needed for ISO 8859/15).  To
  get 'iso-8859-15 preferred to 'iso-8859-1 in autodetection, use
  (set-coding-category-system 'iso-8-1 'iso-8859-15).  (untested)
  Alternatively set language environment to Latin-9.

  If all you want is ISO 8859/15 support, you can `(require
  'latin-unity-latin9)' and `(require 'latin-euro-input)', and not
  do `(latin-unity-install)'.

- ISO 8859/16 for XEmacs 21.4 and 21.1 (both untested).
  To get 'iso-8859-16 preferred to 'iso-8859-1 in autodetection, use
  (set-coding-category-system 'iso-8-1 'iso-8859-16).  (untested)
  Alternatively set language environment to Latin-10.

  If all you want is ISO 8859/16 support, you can `(require
  'latin-unity-latin10)', and not do `(latin-unity-install)'.
  Latin-10 does not yet have an input method.

- Hooks into `write-region' to prevent (or at least drastically
  reduce the probability of) introduction of ISO 2022 escape
  sequences for "foreign" character sets.  This hook is not set by
  default in this package yet; try M-x latin-unity-example RET for a
  short introduction and some useful C-x C-e'able exprs.

  This may permit us to turn off support for those sequences
  entirely in our ISO 8859 coding-systems.

- Interactive functions to _remap_ a region between character sets
  (preserving character identity) and _recode_ a region (preserving
  the code point).  The former is probably not useful if the
  automatic function is working at all, but provided for
  completeness.  The latter is useful if Mule mistakenly reads an
  ISO 8859/2 file as ISO 8859/1; you can change it without rereading
  the file.  Since it's region-oriented, you can also deal with cut
  and paste from dumb applications that export everything as ISO 8859/1.

- A nearly comprehensive Texinfo manual contains a discussion of
  why these things happen, why they can't be 100% avoided in an 8-bit
  world, and some defensive measures users can take, as well as the
  usual install, configure, and operating instructions.

- __latin-unity__ itself depends only on mule-base in operation.  Table
  generation depends on Unicode support such as Mule-UCS or XEmacs
  >= 21.5.6, and the package build currently requires Mule-UCS.  The
  input method depends on LEIM and fsf-compat.

### Current misfeatures:

- Needs to be refactored so that automatic tests of functionality
  currently buried in interactive functions can be written.  See
  comment on `latin-unity-sanity-check'.

- Doesn't automatically save pure ASCII files in ASCII superset
  encodings like iso-2022-jp.  Workaround:  put an ISO 8859 coding
  system in `latin-unity-preapproved-coding-system-list'.

- Need `(require 'latin-euro-input)' to get Quail support.

- Possible performance hit on large (> 20kB) buffers with many
  (>20%) non-ASCII characters.  Partially optimized, but see near
  `latin-unity-region-representations-feasible-region' in
  __latin-unity.el__ for possible further optimizations.

- Package depends on Mule-UCS, LEIM (Quail), and fsf-compat.

- This README is too long.

- Must load __latin-unity-vars__ before reading a file with ISO 8859/15,
  there is no way to autoload a charset.  (Can't be fixed without
  changing XEmacs core.)

### Planned, sooner or later:

- Fix the misfeatures.

- Fix JIS Roman (as an alternative to ASCII) support.

- More UI features (like highlighting unrepresentable chars in buffer).

- Integration to development tree (but probably not 21.4, this
  package should be good enough).

- Hook into MUAs.

- GNU Emacs support.

- Add coding-system and charset widgets for Customization.  The :set
  functions should do sanity and cross checks.

### Not planned any time soon:

- Extend to process buffers in some way, which looks very hard.

- Han-unity.  This is not entirely analogous to Latin unity, and
  needs to be treated very carefully.

## Implementation:

__latin-unity.el__ is the main library, providing the detection and translation
functionality, including a hook function to hang on `write-region-pre-hook'.

__latin-unity-vars.el__ contains the definition of variables common to
several modules.

__latin-unity-latin7.el__ defines ISO 8859/13 and the Latin-7 environment.

__latin-latin7-input.el__ contains a Quail input method for Latin 7.

__latin-unity-latin8.el__ defines ISO 8859/14 and the Latin-8 environment.

__latin-unity-latin9.el__ defines ISO 8859/15 and the Latin-9 environment.

__latin-euro-input.el__ contains Dave Love's Quail input method for Latin 9.

__latin-unity-latin10.el__ defines ISO 8859/16 and the Latin-10 environment.

(Latin-8 and Latin-10 do not have input methods yet.)

__latin-unity-tables.el__ contains the table of feasible character sets and
equivalent Mule characters from other character sets for the various Mule
representations of each character.  Automatically generated.  Used only when
Unicode support is not present.

__latin-unity-utils.el__ contains utilities for creating the equivalence table,
and dumping it to a file.  Used in preference to the precomputed table when
Unicode support is available.

__latin-unity-tests.el__ contains a (very incomplete) test suite using Martin
Buchholz's test-harness.el (distributed in the core in tests/automated).
