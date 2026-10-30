;;; install-check.el --- Check that source installs provide C-h i  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Lean4-Mode is not on any package archive yet, so everybody installs
;; it from source.  Nothing on that path builds a manual, which is why
;; the repository tracks lean4-mode.info and the Info "dir" index as
;; well as the .texi.  This script installs the working tree the way a
;; user would and checks that "C-h i" really does find the manual, so
;; that those two files cannot quietly stop being enough.
;;
;; Usage:
;;
;;     emacs -Q --batch -l admin/install-check.el -- METHOD [CHECKOUT]
;;
;; METHOD is `package-vc' or `straight'.  CHECKOUT defaults to the
;; current directory.  Each run works inside a throwaway
;; `user-emacs-directory' and needs network access.

;;; Code:

(require 'seq)

(defvar lean4-install-check-method nil)
(defvar lean4-install-check-checkout nil)
(defvar lean4-install-check-home nil)

(defun lean4-install-check--fail (format &rest args)
  "Report FORMAT with ARGS as a failure and exit non-zero."
  (message "FAIL (%s): %s" lean4-install-check-method
           (apply #'format format args))
  (kill-emacs 1))

(defun lean4-install-check--copy-tree (destination)
  "Copy the checkout to DESTINATION, without build leftovers.
A user installs a clone, not a working tree that has been built in, so
the dependency directory and any stale .elc files are left behind."
  (copy-directory lean4-install-check-checkout destination nil t t)
  (dolist (junk '(".deps" ".git"))
    (ignore-errors (delete-directory (expand-file-name junk destination) t)))
  (dolist (elc (directory-files destination t "\\.elc\\'"))
    (delete-file elc)))

(defun lean4-install-check--assert-info ()
  "Fail unless the Lean4-Mode manual is reachable through Info.
Checks the three things that have to line up: the installed directory
is on `Info-directory-list', the Info directory node lists the manual,
and the manual itself opens."
  (require 'info)
  (let ((entry (seq-find (lambda (directory)
                           (and (stringp directory)
                                (string-match-p "lean4-mode" directory)))
                         Info-directory-list)))
    (unless entry
      (lean4-install-check--fail
       "no lean4-mode directory on Info-directory-list; a \"dir\" file is what puts it there"))
    (message "  Info-directory-list has %s" entry))
  (with-temp-buffer
    (Info-mode)
    (condition-case err
        (Info-directory)
      (error (lean4-install-check--fail "cannot open the Info directory: %s"
                                        (error-message-string err))))
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Lean4-Mode:" nil t)
      (lean4-install-check--fail
       "the Info directory does not list Lean4-Mode, so C-h i would not show it")))
  (with-temp-buffer
    (Info-mode)
    (condition-case err
        (Info-find-node "lean4-mode" "Top")
      (error (lean4-install-check--fail "cannot open the manual: %s"
                                        (error-message-string err)))))
  (message "  C-h i finds the manual."))

;;;; package-vc, which is what `use-package' uses for its :vc keyword

(defun lean4-install-check-package-vc ()
  "Install through `package-vc' the way `use-package' :vc does.
Dependencies are installed for real here, because resolving them reads
this package's own Package-Requires header, which is worth exercising."
  (require 'package)
  (require 'package-vc)
  (require 'use-package)
  (setq package-user-dir (expand-file-name "elpa" lean4-install-check-home))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  ;; Install from a copy, not from the checkout itself:
  ;; `package-vc-install-from-checkout' symlinks what it is given and
  ;; then writes a -pkg.el, an autoloads file and .elc files into it,
  ;; which would leave the working tree dirty.
  (let ((clone (expand-file-name "clone" lean4-install-check-home)))
    (lean4-install-check--copy-tree clone)
    ;; `use-package-handler/:vc' takes the local path from the
    ;; :load-path keyword and hands it to
    ;; `package-vc-install-from-checkout', so this is the declaration a
    ;; user would write against a local clone.
    (eval `(use-package lean4-mode
             :load-path ,clone
             :vc (:url "https://github.com/leanprover-community/lean4-mode")
             :defer t)
          t))
  (unless (package-installed-p 'lean4-mode)
    (lean4-install-check--fail "use-package :vc did not install the package"))
  (message "  installed %s"
           (package-desc-dir (cadr (assq 'lean4-mode package-alist))))
  (lean4-install-check--assert-info))

;;;; straight.el

(defun lean4-install-check-straight ()
  "Install through straight.el.
The repository is put where straight keeps its clones, so that the
working tree under test is what gets built.  Dependencies are declared
built-in rather than installed: straight would otherwise clone four
recipe repositories and the whole of lsp-mode's dependency tree, none
of which this repository controls, and none of which bears on whether
the manual is installed."
  (setq straight-base-dir lean4-install-check-home
        straight-check-for-modifications nil
        straight-built-in-pseudo-packages
        '(emacs nadvice python image-mode dash lsp-mode magit-section))
  (make-directory (expand-file-name "straight/repos/" lean4-install-check-home) t)
  (lean4-install-check--copy-tree
   (expand-file-name "straight/repos/lean4-mode" lean4-install-check-home))
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                          lean4-install-check-home))
        (bootstrap-version 7))
    (ignore bootstrap-version)
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  ;; :type nil -- the clone is already in place.  :inherit nil -- do not
  ;; go looking for a recipe, which would clone the recipe repositories.
  ;; :files mirrors the Melpa recipe.
  (straight-use-package
   '(lean4-mode :type nil :inherit nil :files (:defaults "data" "data/*.json")))
  (let ((build (expand-file-name "straight/build/lean4-mode/"
                                 lean4-install-check-home)))
    (unless (file-directory-p build)
      (lean4-install-check--fail "straight did not build the package"))
    (message "  built %s" build))
  (lean4-install-check--assert-info))

;;;; Entry point

;; Emacs leaves the "--" separator itself in `command-line-args-left'.
(let* ((args (remove "--" command-line-args-left))
       (method (car args))
       (checkout (or (cadr args) default-directory)))
  (setq command-line-args-left nil
        lean4-install-check-method method
        lean4-install-check-checkout (expand-file-name checkout)
        lean4-install-check-home
        (file-name-as-directory (make-temp-file "lean4-install-check" 'directory))
        user-emacs-directory lean4-install-check-home)
  (message "Checking a %s install of %s" method lean4-install-check-checkout)
  (pcase method
    ("package-vc" (lean4-install-check-package-vc))
    ("straight" (lean4-install-check-straight))
    (_ (lean4-install-check--fail
        "unknown method; expected `package-vc' or `straight'")))
  (message "OK (%s): the manual is installed and reachable." method)
  (kill-emacs 0))

;;; install-check.el ends here
