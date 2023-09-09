.POSIX:
.PHONY: all compile test clean check
.SUFFIXES: .el .elc

EMACS = emacs
MAKEINFO = makeinfo
BYTEC = compat-25.elc \
	compat-26.elc \
	compat-27.elc \
	compat-28.elc \
	compat-29.elc \
	compat.elc \
	compat-macs.elc \
	compat-tests.elc

all: compile

compile: $(BYTEC)

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

$(BYTEC): compat-macs.el

.el.elc:
	@echo "Compiling $<"
	@$(EMACS) -Q --batch -L . \
		--eval '(setq compat-strict t byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

compat.info: compat.texi
	$(MAKEINFO) $<
