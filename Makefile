EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch
TEST_FILES := $(wildcard emacs/test/*-tests.el)
# Pure unit files deliberately exclude seam tests (real Magit/Transient).
UNIT_TEST_FILES := $(filter-out emacs/test/magpi-seam-tests.el,$(TEST_FILES))

# Prefer an explicit build root; otherwise discover Doom's straight tree.
MAGPI_STRAIGHT_BUILD ?= $(firstword \
	$(wildcard $(HOME)/.emacs.d/.local/straight/build-$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')) \
	$(wildcard $(HOME)/.emacs.d/.local/straight/build-30.2) \
	$(wildcard $(HOME)/.emacs.d/.local/straight/build))

SEAM_LFLAGS = \
	-L $(MAGPI_STRAIGHT_BUILD)/compat \
	-L $(MAGPI_STRAIGHT_BUILD)/cond-let \
	-L $(MAGPI_STRAIGHT_BUILD)/llama \
	-L $(MAGPI_STRAIGHT_BUILD)/seq \
	-L $(MAGPI_STRAIGHT_BUILD)/transient \
	-L $(MAGPI_STRAIGHT_BUILD)/with-editor \
	-L $(MAGPI_STRAIGHT_BUILD)/magit-section \
	-L $(MAGPI_STRAIGHT_BUILD)/magit


LIFE_LFLAGS = $(SEAM_LFLAGS) \
	-L $(MAGPI_STRAIGHT_BUILD)/timeout \
	-L $(MAGPI_STRAIGHT_BUILD)/spinner \
	-L $(MAGPI_STRAIGHT_BUILD)/pcre2el \
	-L $(MAGPI_STRAIGHT_BUILD)/pimacs

.PHONY: test test-unit test-seams test-emacs test-life test-life-pi compile xref instrument need-magit need-life
test: test-unit test-seams

test-emacs: test

need-magit:
	@test -n "$(MAGPI_STRAIGHT_BUILD)" || { \
	  echo "error: no straight build root; set MAGPI_STRAIGHT_BUILD"; exit 1; }
	@test -d "$(MAGPI_STRAIGHT_BUILD)/magit" || { \
	  echo "error: $(MAGPI_STRAIGHT_BUILD) lacks magit"; exit 1; }

need-life: need-magit
	@test -d "$(MAGPI_STRAIGHT_BUILD)/pimacs" || { \
	  echo "error: $(MAGPI_STRAIGHT_BUILD) lacks pimacs"; exit 1; }

test-unit:
	$(EMACS_BATCH) -L emacs -L emacs/test $(foreach file,$(UNIT_TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

test-seams: need-magit
	MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(SEAM_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-seam-tests.el \
		-f ert-run-tests-batch-and-exit

test-life: need-life
	MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(LIFE_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-life-play.el \
		--eval "(magpi-life-play-run 'git)"

test-life-pi: need-life
	PI_OFFLINE=1 MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(LIFE_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-life-play.el \
		--eval "(magpi-life-play-run 'pi)"

compile: need-life
	MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(LIFE_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-instrument.el \
		--eval "(kill-emacs (magpi-instrument-exit-code (magpi-instrument-compile)))"

xref: need-life
	MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(LIFE_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-instrument.el \
		--eval "(progn (magpi-instrument-load) (magpi-instrument-xref))"

instrument: need-life
	MAGPI_STRAIGHT_BUILD=$(MAGPI_STRAIGHT_BUILD) $(EMACS_BATCH) \
		$(LIFE_LFLAGS) -L emacs -L emacs/test \
		-l emacs/test/magpi-instrument.el \
		-f magpi-instrument
