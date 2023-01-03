.POSIX:
.PHONY: all compile test clean
.SUFFIXES: .el .elc

EMACS = emacs
MAKEINFO = makeinfo
BYTEC = compat-24.elc \
	compat-25.elc \
	compat-26.elc \
	compat-27.elc \
	compat-28.elc \
	compat-29.elc \
	compat.elc

all: compile

compile: $(BYTEC)

test: clean
	$(EMACS) --version
	$(EMACS) -Q --batch -L . -l compat-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(RM) $(BYTEC) compat.info

$(BYTEC): compat-macs.el

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

compat.info: compat.texi
	$(MAKEINFO) $<
