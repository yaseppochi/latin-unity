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

# The XEmacs CVS version is canonical.  Keep versions n'sync.
VERSION = 0.91
AUTHOR_VERSION = 0.99
MAINTAINER = Stephen J. Turnbull <stephen@xemacs.org>
PACKAGE = latin-unity
PKG_TYPE = regular
# The Mule-UCS require will go away at some point
REQUIRES = mule-base mule-ucs
CATEGORY = mule

ELCS = latin-unity.elc latin-unity-vars.elc \
       latin-unity-tables.elc latin-unity-utils.elc

# for defvars and creation of ISO 8859/15 charset and coding system
PRELOADS=-l cl-macs -l latin-unity-vars.el

TEXI_FILES = $(PACKAGE).texi
INFO_FILES = $(PACKAGE).info
MANUAL = $(PACKAGE)

DATA_1_FILES = ChangeLog Makefile README BLURB
DATA_1_DEST = $(PACKAGE)

include ../../XEmacs.rules

GENERATED += custom-load.elc

ifeq ($(BUILD_WITHOUT_MULE),)

all:: auto-autoloads.elc $(ELCS) custom-load.elc $(INFO_FILES)

# There should be a rule here to build latin-unity-tables.el.
# Then add latin-unity-tables.elc to GENERATED.

srckit: srckit-std

binkit: binkit-common

else
all::
	@echo Latin-Unity requires XEmacs/Mule to build

# Two noops
srckit:
binkit:

endif
