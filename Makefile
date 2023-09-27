.POSIX:
.PHONY: all compile force-compile test clean check install
.SUFFIXES: .el .elc

DESTDIR ?=
PREFIX  ?= /usr
DATADIR ?= $(PREFIX)/share
ELDIR   ?= $(DATADIR)/emacs/site-lisp
INFODIR	?= $(DATADIR)/info


EMACS = emacs
MAKEINFO = makeinfo
GZIP	= gzip
INSTALL_INFO = install-info
RUNTIME = compat-25.elc \
	compat-26.elc \
	compat-27.elc \
	compat-28.elc \
	compat-29.elc \
	compat.elc
DEVELOPMENT = \
	compat-macs.elc \
	compat-tests.elc
BYTEC = $(RUNTIME) \
		$(DEVELOPMENT) \
INFOS = compat.info

all: compile

compile: $(BYTEC)

force-compile:
	sed -i "s/ no-byte-compile: t;/ no-byte-compile: nil;/" $(BYTEC:.elc=.el)
	make compile
	sed -i "s/ no-byte-compile: nil;/ no-byte-compile: t;/" $(BYTEC:.elc=.el)

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
	@ (diff /tmp/compat-defs /tmp/compat-defs)

install: $(addprefix install-,compat-runtime-el compat-info)

install-compat-runtime-el: $(RUNTIME) $(RUNTIME:.elc=.el)

install-compat-info: compat.info

install-%-el:
	$(if $<, install -m755 -d $(DESTDIR)$(ELDIR))
	$(if $<, install -m644 $^ $(DESTDIR)$(ELDIR))

install-%-info:
	$(if $<, install -m755 -d $(DESTDIR)$(INFODIR))
	$(if $<, $(INSTALL_INFO) --info-file=$< --info-dir=$(DESTDIR)$(INFODIR))
	$(if $<, install $^ $(DESTDIR)$(INFODIR))
	$(if $<, $(GZIP) -9nf $(DESTDIR)$(INFODIR)/$<)


$(BYTEC): compat-macs.el

.el.elc:
	@echo "Compiling $<"
	@$(EMACS) -Q --batch -L . \
		--eval '(setq compat-strict t byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

compat.info: compat.texi
	$(MAKEINFO) $<
