.POSIX:
.PHONY: all compile test clean
.SUFFIXES: .el .elc

EMACS = emacs
BYTEC = compat-macs.elc \
	compat-24.4.elc \
	compat-25.1.elc \
	compat-26.1.elc \
	compat-27.1.elc \
	compat-28.1.elc \
	compat.elc

all: compile test

compile: $(BYTEC)

test:
	$(EMACS) -Q --batch -L . -l compat-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(BYTEC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $^

