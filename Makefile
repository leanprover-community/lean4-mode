# GNU- and NonGNU-Elpa accept Org files as package documentation but
# Melpa does not.  As long as Lean4-Mode is not distributed on GNU- or
# NonGNU-Elpa, it should ship with .texi and .info manuals.  This
# depends on: GNU Emacs, Make and GNU Texinfo.

lean4-mode.info lean4-mode.texi: README.org
	emacs --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-info)'
