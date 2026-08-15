EMACS ?= emacs
TEST_FILES := $(wildcard emacs/test/*-tests.el)

.PHONY: test test-emacs

test: test-emacs

test-emacs:
	$(EMACS) -Q --batch -L emacs $(foreach file,$(TEST_FILES),-l $(file)) -f ert-run-tests-batch-and-exit
