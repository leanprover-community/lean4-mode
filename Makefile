EMACS ?= emacs

# Byte-compilation order matters: a file has to be compiled after the
# ones whose macros it uses.
SRCS = lean4-settings.el lean4-util.el lean4-syntax.el lean4-eri.el \
       lean4-fringe.el lean4-lake.el lean4-info.el lean4-input.el \
       lean4-mode.el

ELCS = $(SRCS:.el=.elc)

# The ERT suite.  Hermetic: it starts no language server, opens no
# network connection and needs no Lean toolchain.  Tests named
# lean4-live-*-test.el are the exception and are held back for the
# "test-live" target below.
LIVE_TESTS = $(sort $(wildcard test/lean4-live-*-test.el))
TESTS = $(filter-out $(LIVE_TESTS),$(sort $(wildcard test/lean4-*-test.el)))

# Dependencies go into a throw-away directory, so that running the
# checks never disturbs the package set of whoever runs them.
DEPS_DIR = .deps
DEPS = dash magit-section lsp-mode

BATCH = $(EMACS) -Q --batch \
	--eval '(require (quote package))' \
	--eval '(setq package-user-dir (expand-file-name "$(DEPS_DIR)"))' \
	--eval '(package-initialize)' \
	--directory .

.DEFAULT_GOAL := check
.PHONY: check compile checkdoc test test-live manual install-check deps clean

# Everything CI gates on.  "test" runs after "compile" so that the suite
# exercises the byte-compiled package, which is what users load.
check: compile test checkdoc

# Install what is needed to byte-compile this package.  Kept separate
# from "check" so that the checks themselves need no network.
deps:
	$(EMACS) -Q --batch \
		--eval '(require (quote package))' \
		--eval '(setq package-user-dir (expand-file-name "$(DEPS_DIR)"))' \
		--eval '(add-to-list (quote package-archives) (cons "melpa" "https://melpa.org/packages/") t)' \
		--eval '(package-initialize)' \
		--eval '(package-refresh-contents)' \
		--eval '(dolist (pkg (quote ($(DEPS)))) (unless (package-installed-p pkg) (package-install pkg)))'

# Warnings are promoted to errors: this package builds clean and should
# stay that way.
compile:
	@rm -f $(ELCS)
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(SRCS)

test:
	$(BATCH) --directory test \
		$(patsubst %,--load %,$(TESTS)) \
		--funcall ert-run-tests-batch-and-exit

# Tests which drive a real "lean --server".  Deliberately outside
# "check": they need a Lean toolchain, which is several gigabytes, so
# making every CI run fetch one is not worth it.  Run them by hand after
# a Lean upgrade, or whenever a fixture in the hermetic suite looks
# stale -- catching that drift is the whole point of them.  They skip
# themselves when no "lean" is on PATH.
test-live:
	$(BATCH) --directory test \
		$(patsubst %,--load %,$(LIVE_TESTS)) \
		--funcall ert-run-tests-batch-and-exit

# Install the working tree the way a user would and check that "C-h i"
# finds the manual.  Outside "check" because it needs network access;
# CI runs it as its own job.  Plain "$(EMACS) -Q" rather than $(BATCH),
# so that the install sees nothing of this directory's package set.
install-check:
	$(EMACS) -Q --batch --load admin/install-check.el -- package-vc .
	$(EMACS) -Q --batch --load admin/install-check.el -- straight .

# Documentation-style checks.  Any finding fails the build; see
# admin/checkdoc-batch.el for why that needs a wrapper.
checkdoc:
	$(BATCH) --load admin/checkdoc-batch.el

clean:
	rm -f $(ELCS)
	rm -rf $(DEPS_DIR)

# The manual is generated from README.org, and all three generated
# files are tracked.
#
# The .texi has to be: GNU- and NonGNU-Elpa accept Org files as package
# documentation but Melpa does not, so running Org is the one step its
# build system cannot do for us.
#
# The .info and the Info "dir" index are tracked so that "C-h i" works
# for people who install from source.  Nothing on that path builds a
# manual -- package-vc only does so when a package specification names
# a :doc file -- and `package-activate-1' only puts a package on
# `Info-directory-list' when it ships a "dir".  Once Lean4-Mode is
# listed on Melpa or NonGNU-Elpa, both of which build and index the
# manual themselves, these two can be dropped again.
#
# Regenerate all three after editing README.org; CI checks that they
# are up to date.  Depends on: GNU Emacs, Make and GNU Texinfo.
manual: lean4-mode.texi lean4-mode.info dir

lean4-mode.texi: README.org
	$(EMACS) --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-texinfo)'

lean4-mode.info: lean4-mode.texi
	makeinfo --no-split $< -o $@

# Built from scratch rather than updated in place, so that the result
# does not depend on whatever the file happened to contain before.
dir: lean4-mode.info
	rm -f $@
	install-info --dir=$@ $<
