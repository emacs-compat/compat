.POSIX:
.PHONY: all compile test clean
.SUFFIXES: .el .elc

EMACS = emacs
MAKEINFO = makeinfo
BYTEC = compat-help.elc \
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
	$(EMACS) -Q --batch -L . -l compat-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) $(BYTEC) compat.info

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $^

# compat.texi: MANUAL
# 	$(EMACS) -Q --batch $< -f org-texinfo-export-to-texinfo --kill

compat.info: compat.texi
	$(MAKEINFO) $<
