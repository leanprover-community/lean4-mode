# GNU- and NonGNU-Elpa accept Org files as package documentation but
# Melpa does not.  As long as Lean4-Mode is not distributed on GNU- or
# NonGNU-Elpa, it should ship with .texi and .info manuals.  This
# depends on: GNU Emacs, Make, GNU Texinfo and the “sponge” program
# bundled in the “Moreutils” collection that is maintained by Joey
# Hess.

.PHONY: all autoload clean info
all : autoload info

clean:
	rm -f lean4-mode.info lean4-mode.texi lean4-mode-autoloads.el

autoload: lean4-mode-autoloads.el
lean4-mode-autoloads.el : $(wildcard *.el)
	emacs --batch \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "./$@")' \
		--eval "(require 'autoload)" \
		-f batch-update-autoloads '.'
	cat $@ | sponge | sed s/"This file is part of GNU Emacs"/"This file is NOT part of GNU Emacs"/ > $@

info: lean4-mode.info lean4-mode.texi
lean4-mode.info lean4-mode.texi : README.org
	emacs --batch \
		--eval "(require 'ox-texinfo)" \
		--eval '(find-file "$<")' \
		--eval '(org-texinfo-export-to-info)'
