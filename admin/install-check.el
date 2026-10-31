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
;;
;; The run happens in two phases, in two processes.  The first installs;
;; the second starts afresh against the same `user-emacs-directory' and
;; looks for the manual.  Nobody reads the manual in the session that
;; installed the package -- they restart Emacs -- and the second phase
;; is that restart.  It also sidesteps activation inside the installing
;; session, which is not dependable: packages are activated as they
;; arrive there, and on Emacs 31.1 lsp-mode and magit-section both
;; reported "Unable to activate package".

;;; Code:

(require 'seq)
(require 'subr-x)

;; Native compilation runs in child processes, and those abort -- and so
;; dump core -- when the parent exits while they are still going.
;; Loading straight.el sets several off.  Nothing here depends on native
;; code, so switch it off rather than litter the machine with crash
;; reports.  Straight compiles its own sources while bootstrapping,
;; before any per-recipe :build property could apply, which is why
;; `straight-disable-native-compile' is set separately below.
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))
(setq native-comp-async-report-warnings-errors nil)

(defconst lean4-install-check-script (or load-file-name buffer-file-name)
  "This file, so that the second phase can load it in a fresh Emacs.")

(defvar lean4-install-check-method nil)
(defvar lean4-install-check-checkout nil)
(defvar lean4-install-check-home nil)

(defconst lean4-install-check-straight-recipe
  ;; :type nil -- the clone is already in place.  :inherit nil -- do not
  ;; go looking for a recipe, which would clone the recipe repositories.
  ;; :files mirrors the Melpa recipe.
  '(lean4-mode :type nil :inherit nil
               :files (:defaults "data" "data/*.json"))
  "Recipe used for the straight.el install.")

(defun lean4-install-check--fail (format &rest args)
  "Report FORMAT with ARGS as a failure and exit non-zero."
  (message "FAIL (%s): %s" lean4-install-check-method
           (apply #'format format args))
  (kill-emacs 1))

(defun lean4-install-check--copy-tree (destination)
  "Copy the checkout to DESTINATION, without build leftovers.
Used for straight.el, which builds whatever sits in its repos
directory.  Dropped along the way are the dependency directory and any
stale .elc files, which a fresh clone would not have either."
  (copy-directory lean4-install-check-checkout destination nil t t)
  (let ((deps (expand-file-name ".deps" destination)))
    (when (file-directory-p deps)
      (delete-directory deps t)))
  (dolist (elc (directory-files destination t "\\.elc\\'"))
    (delete-file elc)))

(defun lean4-install-check--straight-settings ()
  "Configure straight.el.  Must run before its bootstrap.
`straight-disable-native-compile' is set here rather than in the recipe
because straight compiles its own sources while bootstrapping, and
`defcustom' leaves an already-set value alone.  The dependencies are
declared built-in: straight would otherwise clone four recipe
repositories and the whole of lsp-mode's dependency tree, none of which
this repository controls, and none of which bears on whether the manual
is installed."
  (setq straight-base-dir lean4-install-check-home
        straight-check-for-modifications nil
        straight-disable-native-compile t
        straight-built-in-pseudo-packages
        '(emacs nadvice python image-mode dash lsp-mode magit-section)))

(defun lean4-install-check--straight-bootstrap ()
  "Load straight.el, fetching it first if this is the install phase."
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
    (load bootstrap-file nil 'nomessage)))

;;;; Phase one: install

(defun lean4-install-check-install-package-vc ()
  "Install through `package-vc' the way `use-package' :vc does.
Dependencies are installed for real here, because resolving them reads
this package's own Package-Requires header, which is worth exercising.

The checkout is passed as the :url, so that package-vc clones it as it
would any remote.  Deliberately not :load-path, which would send
`use-package' down `package-vc-install-from-checkout' instead: that
symlinks the directory rather than cloning it, and a symlinked package
is not what the overwhelming majority of users install.  Nor is it
equivalent: installed that way on Emacs 31.1, lean4-mode never reached
`Info-directory-list', though its autoloads did load and the real
directories beside it registered fine.

Cloning sees committed state only, which is what CI has anyway."
  (require 'package)
  (require 'package-vc)
  (require 'use-package)
  (setq package-user-dir (expand-file-name "elpa" lean4-install-check-home))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  ;; :rev :newest -- take the checked-out commit rather than hunting for
  ;; the latest revision that bumped the Version header.
  (eval `(use-package lean4-mode
           :vc (:url ,lean4-install-check-checkout :rev :newest)
           :defer t)
        t)
  (unless (package-installed-p 'lean4-mode)
    (lean4-install-check--fail "use-package :vc did not install the package"))
  (message "  installed %s"
           (package-desc-dir (cadr (assq 'lean4-mode package-alist)))))

(defun lean4-install-check-install-straight ()
  "Install through straight.el.
The repository is put where straight keeps its clones, so that the
working tree under test is what gets built."
  (lean4-install-check--straight-settings)
  (make-directory (expand-file-name "straight/repos/" lean4-install-check-home) t)
  (lean4-install-check--copy-tree
   (expand-file-name "straight/repos/lean4-mode" lean4-install-check-home))
  (lean4-install-check--straight-bootstrap)
  (straight-use-package lean4-install-check-straight-recipe)
  (let ((build (expand-file-name "straight/build/lean4-mode/"
                                 lean4-install-check-home)))
    (unless (file-directory-p build)
      (lean4-install-check--fail "straight did not build the package"))
    (message "  built %s" build)))

;;;; Phase two: a fresh Emacs, as if the user had restarted

(defun lean4-install-check--assert-info ()
  "Fail unless the Lean4-Mode manual is reachable through Info.
Asserts the two things a user would notice: the Info directory node
lists the manual, and the manual itself opens.

Deliberately not asserted: that some entry of `Info-directory-list'
has \"lean4-mode\" in its name.  That looks like the same question but
is not.  `package-vc-install-from-checkout' installs a symlink, so the
directory which ends up registered may be the link's target under
whatever name the clone has -- and whether it is resolved differs
between Emacs versions.  The list is only reported here, to make a
failure easier to read."
  (require 'info)
  (info-initialize)
  (message "  Info-directory-list: %S"
           (seq-remove (lambda (directory)
                         (and (stringp directory)
                              (string-match-p "\\`/usr\\|/share/info" directory)))
                       Info-directory-list))
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

(defun lean4-install-check-verify ()
  "Activate the installed package as a fresh session would, then check Info."
  (pcase lean4-install-check-method
    ("package-vc"
     (require 'package)
     (setq package-user-dir (expand-file-name "elpa" lean4-install-check-home))
     (package-initialize))
    ("straight"
     ;; A user's init file calls `straight-use-package' on every start;
     ;; with the package already built this only activates it.
     (lean4-install-check--straight-settings)
     (lean4-install-check--straight-bootstrap)
     (straight-use-package lean4-install-check-straight-recipe)))
  (lean4-install-check--assert-info))

(defun lean4-install-check--verify-in-fresh-emacs ()
  "Re-run this script in a fresh Emacs to do the checking."
  (message "  restarting Emacs to look for the manual")
  (let* ((buffer (generate-new-buffer " *lean4-install-check*"))
         (status (call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil buffer nil
                  "-Q" "--batch" "--load" lean4-install-check-script
                  "--" "verify" lean4-install-check-method
                  lean4-install-check-home)))
    ;; Forward whatever it said; `call-process' would otherwise leave the
    ;; second phase's diagnostics in a buffer nobody ever reads.
    (with-current-buffer buffer
      (unless (zerop (buffer-size))
        (message "%s" (string-trim (buffer-string)))))
    (kill-buffer buffer)
    (unless (equal status 0)
      (lean4-install-check--fail
       "a fresh Emacs could not find the manual (exit %s)" status))))

;;;; Entry point

;; Emacs leaves the "--" separator itself in `command-line-args-left'.
(let* ((args (remove "--" command-line-args-left))
       (verify (equal (car args) "verify")))
  (setq command-line-args-left nil)
  (if verify
      (setq lean4-install-check-method (nth 1 args)
            lean4-install-check-home (nth 2 args)
            user-emacs-directory lean4-install-check-home)
    (setq lean4-install-check-method (nth 0 args)
          lean4-install-check-checkout (expand-file-name
                                        (or (nth 1 args) default-directory))
          lean4-install-check-home (file-name-as-directory
                                    (make-temp-file "lean4-install-check"
                                                    'directory))
          user-emacs-directory lean4-install-check-home))
  (cond
   (verify
    (lean4-install-check-verify))
   (t
    (message "Checking a %s install of %s"
             lean4-install-check-method lean4-install-check-checkout)
    (pcase lean4-install-check-method
      ("package-vc" (lean4-install-check-install-package-vc))
      ("straight" (lean4-install-check-install-straight))
      (_ (lean4-install-check--fail
          "unknown method; expected `package-vc' or `straight'")))
    (lean4-install-check--verify-in-fresh-emacs)
    (message "OK (%s): the manual is installed and reachable."
             lean4-install-check-method)))
  (kill-emacs 0))

;;; install-check.el ends here
