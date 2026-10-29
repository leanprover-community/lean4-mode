EMACS ?= emacs

# Byte-compilation order matters: a file has to be compiled after the
# ones whose macros it uses.
SRCS = lean4-settings.el lean4-util.el lean4-syntax.el lean4-eri.el \
       lean4-fringe.el lean4-lake.el lean4-info.el lean4-input.el \
       lean4-mode.el

ELCS = $(SRCS:.el=.elc)

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
.PHONY: check compile checkdoc deps clean

# Everything CI gates on.
check: compile checkdoc

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

# Documentation-style checks.  Any finding fails the build; see
# admin/checkdoc-batch.el for why that needs a wrapper.
checkdoc:
	$(BATCH) --load admin/checkdoc-batch.el

clean:
	rm -f $(ELCS)
	rm -rf $(DEPS_DIR)

# GNU- and NonGNU-Elpa accept Org files as package documentation but
# Melpa does not.  As long as Lean4-Mode is not distributed on GNU- or
# NonGNU-Elpa, it should ship with .texi and .info manuals.  This
# depends on: GNU Emacs, Make and GNU Texinfo.

lean4-mode.info lean4-mode.texi: README.org
	$(EMACS) --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-info)'
