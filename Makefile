.POSIX:
.PHONY: all compile test clean
.SUFFIXES: .el .elc

EMACS = emacs
MAKEINFO = makeinfo
BYTEC = compat-help.elc \
	compat-font-lock.elc \
	compat-macs.elc \
	compat-24.elc \
	compat-25.elc \
	compat-26.elc \
	compat-27.elc \
	compat-28.elc \
	compat.elc

all: compile

compile: $(BYTEC)

test: compile
	$(EMACS) --version
	$(EMACS) -Q --batch -L . -l compat-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) $(BYTEC) compat.info

compat.el:	compat-macs.el
compat-24.el:	compat-macs.el
compat-25.el:	compat-macs.el
compat-26.el:	compat-macs.el
compat-27.el:	compat-macs.el
compat-28.el:	compat-macs.el

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

# compat.texi: MANUAL
# 	$(EMACS) -Q --batch $< -f org-texinfo-export-to-texinfo --kill

compat.info: compat.texi
	$(MAKEINFO) $<
