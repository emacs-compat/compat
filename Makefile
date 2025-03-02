### Makefile

# Copyright (C) 2021-2025 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

### Commentary:

# The Makefile is an internal tool used for Compat development and
# checking on the continuous integration system.

# make all/compile   Compile Elisp files without no-byte-compile marking
# make force-compile Compile *all* Elisp files to check for warnings
# make clean         Delete compiled *.elc and *.info files
# make test          Run the test suite
# make check         Sanity checking of the test suite

### Code:

.POSIX:
.PHONY: all compile force-compile test clean check
.SUFFIXES: .el .elc

ifeq ($(CI),true)
        STRICT = --eval '(setq compat-strict t byte-compile-error-on-warn t)'
else
        STRICT =
endif

EMACS = emacs
MAKEINFO = makeinfo
BYTEC = compat-25.elc \
	compat-26.elc \
	compat-27.elc \
	compat-28.elc \
	compat-29.elc \
	compat-30.elc \
	compat.elc \
	compat-macs.elc \
	compat-tests.elc

all: compile

compile: $(BYTEC)

force-compile:
	@sed -i "s/ no-byte-compile: t;/ no-byte-compile: nil;/" $(BYTEC:.elc=.el)
	@$(MAKE) compile
	@sed -i "s/ no-byte-compile: nil;/ no-byte-compile: t;/" $(BYTEC:.elc=.el)

test:
	$(EMACS) --version
	$(EMACS) -Q --batch -L . -l compat-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) $(BYTEC) compat.info

check:
	@echo "Check: All definitions must link to a test"
	@! (grep -E "^\\(compat-(def|guard)" *.el | grep -v "compat-tests:")
	@echo "Check: All definitions must use compat-def* macros"
	@! (grep -E "^\\(def" compat-[0-9][0-9].el)
	@echo "Check: Test links must be valid"
	@grep "(ert-deftest compat-" compat-tests.el | \
		grep -E -v "\\(ert-deftest compat-(function|loaded-features) \\(\\)" | \
		sed -E "s/\\(ert-deftest compat-| \\(\\).*//g" | sort > /tmp/compat-defs
	@grep "compat-tests:" *.el | \
		sed -E "s/.*<compat-tests:([^)]+)>|.*\\[\\[compat-tests:([^)]+)\\]\\]/\1\2/g" | \
		sort | uniq > /tmp/compat-links
	@diff /tmp/compat-defs /tmp/compat-links

$(BYTEC): compat-macs.el

.el.elc:
	@echo "Compiling $<"
	@$(EMACS) -Q --batch -L . $(STRICT) -f batch-byte-compile $<

compat.info: compat.texi
	$(MAKEINFO) $<
