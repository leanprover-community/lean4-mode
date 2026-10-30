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
.PHONY: check compile checkdoc test test-live deps clean

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

# Documentation-style checks.  Any finding fails the build; see
# admin/checkdoc-batch.el for why that needs a wrapper.
checkdoc:
	$(BATCH) --load admin/checkdoc-batch.el

clean:
	rm -f $(ELCS)
	rm -f lean4-mode.info
	rm -rf $(DEPS_DIR)

# GNU- and NonGNU-Elpa accept Org files as package documentation but
# Melpa does not, which is why the generated .texi is tracked: running
# Org is the one step Melpa cannot do for us.  Regenerate it after
# editing README.org.  Depends on: GNU Emacs and Make.
lean4-mode.texi: README.org
	$(EMACS) --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-texinfo)'

# The .info is deliberately not tracked.  Melpa compiles the .texi
# itself, generates the Info "dir" index and drops the .texi from the
# tarball; see package-build--generate-info-files.  Tracking a .info
# would be worse than redundant, because package-build only runs
# makeinfo "unless (file-exists-p info)" -- a stale tracked copy would
# be shipped as-is instead of being rebuilt.
#
# So this rule is only for reading the manual locally.  Depends on: GNU
# Texinfo.
lean4-mode.info: lean4-mode.texi
	makeinfo --no-split $< -o $@
