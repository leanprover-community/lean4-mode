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

(provide 'lean4-mode-test)
;;; lean4-mode-test.el ends here
