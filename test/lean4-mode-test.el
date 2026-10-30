;;; lean4-mode-test.el --- Tests for lean4-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for what entering `lean4-mode' sets up, for the compile
;; command it builds, and for how it picks an LSP workspace root.

;;; Code:

(require 'lean4-test-helper)

;;; Mode setup.

(ert-deftest lean4-test-mode-is-a-prog-mode ()
  "`lean4-mode' derives from `prog-mode' and uses the Lean syntax table."
  (lean4-test-with-buffer ""
    (should (eq 'lean4-mode major-mode))
    (should (derived-mode-p 'prog-mode))
    (should (eq lean4-syntax-table (syntax-table)))))

(ert-deftest lean4-test-mode-buffer-local-settings ()
  "Entering the mode configures comments and disables tab indentation."
  (lean4-test-with-buffer ""
    (should (equal "--" comment-start))
    (should (equal "" comment-end))
    (should (equal 1 comment-padding))
    (should comment-use-syntax)
    (should-not indent-tabs-mode)))

(ert-deftest lean4-test-mode-sets-the-lean-input-method ()
  "The Lean input method is switched on, so that \\\\alpha expands."
  (lean4-test-with-buffer ""
    (should (equal "Lean" current-input-method))))

(ert-deftest lean4-test-mode-handles-dot-lean-files ()
  "Visiting a .lean file selects a Lean mode."
  (should (eq 'lean4-select-mode (cdr (assoc "\\.lean\\'" auto-mode-alist)))))

(ert-deftest lean4-test-mode-select-mode-without-autodetection ()
  "With autodetection off, the Lean executable is never consulted.
`lean4-select-mode' must not shell out on every .lean file visited."
  (lean4-test-with-buffer ""
    (let ((lean4-autodetect-lean3 nil)
          (lean4-mode-hook nil))
      (cl-letf (((symbol-function 'lean4--version)
                 (lambda (&rest _) (error "lean4--version should not run"))))
        (lean4-select-mode)
        (should (eq 'lean4-mode major-mode))))))

;;; Compile command.

(ert-deftest lean4-test-mode-compile-string-without-lake ()
  "Without Lake, the command is the Lean executable applied to the file."
  (should (equal "/bin/lean --opt Foo.lean"
                 (lean4-compile-string nil "/bin/lean" "--opt" "Foo.lean"))))

(ert-deftest lean4-test-mode-compile-string-with-lake ()
  "With Lake, the command runs inside \"lake env\"."
  (should (equal "/bin/lake env /bin/lean --opt Foo.lean"
                 (lean4-compile-string "/bin/lake" "/bin/lean" "--opt"
                                       "Foo.lean"))))

;;; LSP workspace root.

(ert-deftest lean4-test-mode-workspace-root-is-the-outermost-toolchain ()
  "A file in a child package is served by its parent project.
`lean4-create-lsp-workspace' keeps walking up past the nearest
lean-toolchain and registers the outermost one, so that editing a file
in a sub-package uses the settings of the project that contains it."
  (lean4-test-with-directory root '("lean-toolchain"
                                    "pkg/lean-toolchain"
                                    "pkg/Sub/Mod.lean")
    ;; Only meaningful if no ancestor of the temporary directory has a
    ;; lean-toolchain of its own.
    (skip-unless (not (locate-dominating-file
                       (file-name-directory (directory-file-name root))
                       "lean-toolchain")))
    (let (added)
      (cl-letf (((symbol-function 'lsp-workspace-folders-add)
                 (lambda (dir) (push dir added))))
        (lean4-test-with-file-buffer (expand-file-name "pkg/Sub/Mod.lean" root)
          (lean4-create-lsp-workspace)))
      (should (equal 1 (length added)))
      (should (equal (file-truename root) (file-truename (car added)))))))

(ert-deftest lean4-test-mode-workspace-root-needs-a-toolchain ()
  "With no lean-toolchain above the file, no workspace folder is added."
  (lean4-test-with-directory root '("Sub/Mod.lean")
    (skip-unless (not (locate-dominating-file root "lean-toolchain")))
    (let (added)
      (cl-letf (((symbol-function 'lsp-workspace-folders-add)
                 (lambda (dir) (push dir added))))
        (lean4-test-with-file-buffer (expand-file-name "Sub/Mod.lean" root)
          (lean4-create-lsp-workspace)))
      (should-not added))))

(ert-deftest lean4-test-mode-workspace-root-needs-a-file ()
  "A buffer not backed by a file gets no workspace folder."
  (let (added)
    (cl-letf (((symbol-function 'lsp-workspace-folders-add)
               (lambda (dir) (push dir added))))
      (lean4-test-with-buffer "def a : Nat := 0\n"
        (lean4-create-lsp-workspace)))
    (should-not added)))

;;; Keys.

(ert-deftest lean4-test-mode-binds-its-commands ()
  "Entering the mode installs the documented key bindings."
  (lean4-test-with-buffer ""
    (should (eq 'lean4-std-exe (key-binding lean4-keybinding-std-exe1)))
    (should (eq 'lean4-std-exe (key-binding lean4-keybinding-std-exe2)))
    (should (eq 'lean4-tab-indent (key-binding lean4-keybinding-tab-indent)))
    (should (eq 'lean4-toggle-info
                (key-binding lean4-keybinding-lean4-toggle-info)))
    (should (eq 'lean4-lake-build (key-binding lean4-keybinding-lake-build)))
    (should (eq 'lean4-refresh-file-dependencies
                (key-binding lean4-keybinding-refresh-file-dependencies)))))

(ert-deftest lean4-test-mode-tab-indents-with-eri-at-the-margin ()
  "TAB cycles indentation only when there is nothing but space before point.
Further into the line it has to fall back to the normal TAB command, so
that completion and the like keep working."
  (let (called)
    (cl-letf (((symbol-function 'lean4-eri-indent)
               (lambda (&rest _) (setq called 'eri)))
              ((symbol-function 'indent-for-tab-command)
               (lambda (&rest _) (setq called 'tab))))
      (lean4-test-with-buffer "def a : Nat := 0\n  -!-"
        (lean4-tab-indent)
        (should (eq 'eri called)))
      (setq called nil)
      (lean4-test-with-buffer "def a : Nat -!-:= 0\n"
        (lean4-tab-indent)
        (should (eq 'tab called))))))

;;; Lean version.

(ert-deftest lean4-test-mode-version-is-parsed-from-the-executable ()
  "`lean4--version' reads the version out of \"lean -v\" output."
  (cl-letf (((symbol-function 'lean4-get-executable)
             (lambda (name) (concat "/fake/" name)))
            ((symbol-function 'call-process)
             (lambda (&rest _)
               (insert "Lean (version 4.35.0, commit abcdef, Release)\n")
               0)))
    (should (equal '(4 35 0) (lean4--version)))))

(ert-deftest lean4-test-mode-version-rejects-unexpected-output ()
  "Output that does not announce a version is an error, not a wrong answer."
  (cl-letf (((symbol-function 'lean4-get-executable)
             (lambda (name) (concat "/fake/" name)))
            ((symbol-function 'call-process)
             (lambda (&rest _) (insert "command not found\n") 0)))
    (should-error (lean4--version) :type 'search-failed)))

;;; Server command.

(ert-deftest lean4-test-mode-server-cmd-prefers-lake-serve ()
  "From Lake 3.1.0 on, the server is started through \"lake serve\".
Lake knows how to build the dependencies of the file being edited,
which plain \"lean --server\" does not."
  (cl-letf (((symbol-function 'lean4-get-executable)
             (lambda (name) (concat "/fake/" name)))
            ((symbol-function 'process-lines) (lambda (&rest _) '("5.0.0"))))
    (should (equal '("/fake/lake" "serve") (lean4--server-cmd)))))

(ert-deftest lean4-test-mode-server-cmd-falls-back-for-old-lake ()
  "Lake older than 3.1.0 cannot serve, so Lean is asked directly."
  (cl-letf (((symbol-function 'lean4-get-executable)
             (lambda (name) (concat "/fake/" name)))
            ((symbol-function 'process-lines) (lambda (&rest _) '("3.0.0"))))
    (should (equal '("/fake/lean" "--server") (lean4--server-cmd)))))

(ert-deftest lean4-test-mode-server-cmd-falls-back-without-lake ()
  "A missing Lake is a fallback, not an error."
  (cl-letf (((symbol-function 'lean4-get-executable)
             (lambda (name) (concat "/fake/" name)))
            ((symbol-function 'process-lines)
             (lambda (&rest _) (error "No such file or directory, lake"))))
    (should (equal '("/fake/lean" "--server") (lean4--server-cmd)))))

(ert-deftest lean4-test-mode-is-registered-with-lsp ()
  "lsp-mode is told which language id `lean4-mode' buffers speak."
  (should (equal "lean" (cdr (assq 'lean4-mode lsp-language-id-configuration)))))

;;; Running Lean over the current file.

(ert-deftest lean4-test-mode-execute-without-lake ()
  "With no Lake package above the file, Lean is run on the file directly."
  (lean4-test-with-directory root '("Mod.lean")
    (skip-unless (not (locate-dominating-file root #'lean4-root-dir-p)))
    (lean4-test-with-file-buffer (expand-file-name "Mod.lean" root)
      (let (command)
        (cl-letf (((symbol-function 'compile)
                   (lambda (cmd &rest _) (setq command cmd)))
                  ((symbol-function 'lean4-get-executable)
                   (lambda (name) (concat "/fake/" name))))
          (lean4-execute))
        (should (equal (format "/fake/lean  %s"
                               (shell-quote-argument
                                (expand-file-name "Mod.lean" root)))
                       command))))))

(ert-deftest lean4-test-mode-execute-inside-a-lake-package ()
  "Inside a Lake package, Lean is run through \"lake env\".
That is what puts the package's dependencies on Lean's search path."
  (lean4-test-with-directory root '("lakefile.toml" "Mod.lean")
    (lean4-test-with-file-buffer (expand-file-name "Mod.lean" root)
      (let (command directory)
        (cl-letf (((symbol-function 'compile)
                   (lambda (cmd &rest _)
                     (setq command cmd directory default-directory)))
                  ((symbol-function 'lean4-get-executable)
                   (lambda (name) (concat "/fake/" name))))
          (lean4-execute "--opt"))
        (should (equal (format "/fake/lake env /fake/lean --opt %s"
                               (shell-quote-argument
                                (expand-file-name "Mod.lean" root)))
                       command))
        (should (equal (file-truename root) (file-truename directory)))))))

(ert-deftest lean4-test-mode-execute-restores-the-buffer-state ()
  "Executing leaves `compile-command' and `default-directory' as they were."
  (lean4-test-with-directory root '("lakefile.toml" "Mod.lean")
    (lean4-test-with-file-buffer (expand-file-name "Mod.lean" root)
      (setq default-directory temporary-file-directory)
      (setq-local compile-command "untouched")
      (cl-letf (((symbol-function 'compile) (lambda (&rest _) nil))
                ((symbol-function 'lean4-get-executable)
                 (lambda (name) (concat "/fake/" name))))
        (lean4-execute))
      (should (equal "untouched" compile-command))
      (should (equal temporary-file-directory default-directory)))))

(provide 'lean4-mode-test)
;;; lean4-mode-test.el ends here
