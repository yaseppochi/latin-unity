# Makefile for latin-unity

# Copyright (C) 2002, 2005 Free Software Foundation, Inc.

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# Last-modified: 2005 February 7

# The XEmacs CVS version is canonical.  Keep versions n'sync.
VERSION = 1.21
AUTHOR_VERSION = $(VERSION)
MAINTAINER = Stephen J. Turnbull <stephen@xemacs.org>
PACKAGE = latin-unity
PKG_TYPE = regular
CATEGORY = mule

# The Mule-UCS, leim, and fsf-compat requires will go away at some point
ifeq ($(wildcard ../mule-ucs),)
REQUIRES = mule-base latin-euro-standards leim fsf-compat dired
else
REQUIRES = mule-base latin-euro-standards mule-ucs leim fsf-compat dired
endif

# Latin-Unity is a unique package in that it does not compile or run
# under XEmacsen 21.1, at least not "out of the box".  So here's
# some really ugly Makefile voodoo that will allow people using XEmacs
# 21.1 to build the packages without it blowing up in their faces.
# Don't try this at home, kids.  SY.
include ../../Local.rules
CHECK_VERSION =	$(XEMACS) -batch -no-autoloads -eval '(princ (emacs-version>= 21 4))'

ifeq ($(shell $(CHECK_VERSION)),t)
ELCS = latin-unity.elc latin-unity-vars.elc latin-euro-input.elc \
       latin-unity-latin7.elc latin-latin7-input.elc latin-unity-latin9.elc \
       latin-unity-latin8.elc latin-unity-latin10.elc \
       latin-unity-utils.elc


EXTRA_SOURCES = latin-unity-tests.el latin-unity-tables.el

# for defvars and creation of ISO 8859/13 and ISO 8859/15 charsets and
# coding system
PRELOADS=-l cl-macs \
         -l latin-unity-vars.el

STANDARD_DOCS = t

DATA_1_FILES = ChangeLog Makefile README BLURB
DATA_1_DEST = $(PACKAGE)

# This should be "GENERATED_LISP" anyhow.
# GENERATED_ELCS += latin-unity-tables.elc

include ../../XEmacs.rules

# Experimental rule to build latin-unity-tables.el.
#latin-unity-tables.el: latin-unity-vars.elc latin-unity-utils.elc
#	$(BOOT_XEMACS) -l latin-unity-utils -f latin-unity-dump-tables
#	@echo "*** You may see a few warnings about ISO 8859/3. ***"
#	@echo "*** This is OK; it doesn't use all code points.  ***"

# We'd like this to be utf-8, but then pre-21.5.6 would have to depend on
# Mule-UCS
# #### This is broken by latin-unity-tests.el.
check: all
	@echo "make check is currently broken :-/"
	@echo "If you have an XEmacs core source tree, load tests/automated/test-harness"
	@echo "and run M-x test-emacs-test-file RET latin-unity-tests RET by hand."
#	xemacs -no-autoloads -batch \
#		-eval "(setq load-path (cons \"`pwd`/\" load-path))" \
#		-l latin-unity-vars -l latin-unity \
#		-f latin-unity-install -f latin-unity-test \
#		-eval "(write-file \"./latintest\" nil 'iso-2022-7)"
else
include ../../Version.rules

bad-version: autoloads
	@echo "************************ W A R N I N G *************************"
	@echo "Building $(PACKAGE) with versions less than 21.4 is unsupported by"
	@echo "the XEmacs Project."
	@echo "If you need this feature, contact the package maintainer directly:"
	@echo "$(MAINTAINER)"

endif
