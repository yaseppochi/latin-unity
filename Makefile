# Makefile for latin-unity

# Copyright (C) 2002 Free Software Foundation, Inc.

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

# Last-modified: 2002 October 24

# The XEmacs CVS version is canonical.  Keep versions n'sync.
VERSION = 1.05
AUTHOR_VERSION = 1.05
MAINTAINER = Stephen J. Turnbull <stephen@xemacs.org>
PACKAGE = latin-unity
PKG_TYPE = regular
# The Mule-UCS, leim, and fsf-compat requires will go away at some point
REQUIRES = mule-base mule-ucs leim fsf-compat
CATEGORY = mule

ELCS = latin-unity.elc latin-unity-vars.elc latin-euro-input.elc \
       latin-unity-latin7.elc latin-latin7-input.elc latin-unity-latin9.elc \
       latin-unity-latin8.elc latin-unity-latin10.elc \
       latin-unity-tables.elc latin-unity-utils.elc

EXTRA_SOURCES = latin-unity-tests.el

# for defvars and creation of ISO 8859/13 and ISO 8859/15 charsets and
# coding system
PRELOADS=-l cl-macs -l latin-unity-latin7.el -l latin-unity-latin9.el \
         -l latin-unity-vars.el

TEXI_FILES = $(PACKAGE).texi
INFO_FILES = $(PACKAGE).info

DATA_1_FILES = ChangeLog Makefile README BLURB
DATA_1_DEST = $(PACKAGE)

include ../../XEmacs.rules

GENERATED += custom-load.elc

ifeq ($(BUILD_WITHOUT_MULE),)

all:: auto-autoloads.elc $(ELCS) custom-load.elc $(INFO_FILES)

# Experimental rule to build latin-unity-tables.el.
# Note: `make latin-unity-tables.elc' Just Works
# Add latin-unity-tables.elc to GENERATED when it's well-tested.
latin-unity-tables.el: latin-unity-vars.elc latin-unity-utils.elc
	$(XEMACS) -batch -no-autoloads -l dired \
		-eval '(setq load-path (cons (default-directory) load-path))' \
		-l latin-unity-utils -f latin-unity-dump-tables

# We'd like this to be utf-8, but then pre-21.5.6 would have to depend on
# Mule-UCS
# #### This is broken by latin-unity-tests.el.
check: all
	@echo "make check is currently broken :-/"
#	xemacs -no-autoloads -batch \
#		-eval "(setq load-path (cons \"`pwd`/\" load-path))" \
#		-l latin-unity-vars -l latin-unity \
#		-f latin-unity-install -f latin-unity-test \
#		-eval "(write-file \"./latintest\" nil 'iso-2022-7)"

binkit: binkit-common

else
all::
	@echo Latin-Unity requires XEmacs/Mule to build

# Noop
binkit:

endif
