;;; lean4-util-test.el --- Tests for lean4-util  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for locating the Lean installation and for the save hook.

;;; Code:

(require 'lean4-test-helper)

(ert-deftest lean4-test-util-get-executable ()
  "An executable is looked for under \"bin\" of the configured root."
  (lean4-test-with-directory root '("bin/lean")
    (let ((lean4-rootdir (directory-file-name root))
          (lean4-executable-name "lean"))
      (should (equal (expand-file-name "bin/lean" root)
                     (lean4-get-executable "lean"))))))

(ert-deftest lean4-test-util-get-rootdir-rejects-a-root-without-lean ()
  "A configured root which holds no Lean is reported rather than used."
  (lean4-test-with-directory root '("README")
    (let ((lean4-rootdir (directory-file-name root))
          (lean4-executable-name "lean"))
      (should-error (lean4-get-rootdir) :type 'error))))

(ert-deftest lean4-test-util-get-rootdir-uses-configured-root ()
  "A configured root which does hold Lean is used as it stands."
  (lean4-test-with-directory root '("bin/lean")
    (let ((lean4-rootdir (directory-file-name root))
          (lean4-executable-name "lean"))
      (should (equal (directory-file-name root) (lean4-get-rootdir))))))

(ert-deftest lean4-test-util-whitespace-cleanup-is-opt-in ()
  "Trailing whitespace is only removed when the user asked for it."
  (let ((content "def a : Nat := 0   \ndef b : Nat := 1\t\n"))
    (lean4-test-with-buffer content
      (let ((lean4-delete-trailing-whitespace nil))
        (lean4-whitespace-cleanup))
      (should (equal content (buffer-string))))
    (lean4-test-with-buffer content
      (let ((lean4-delete-trailing-whitespace t))
        (lean4-whitespace-cleanup))
      (should (equal "def a : Nat := 0\ndef b : Nat := 1\n"
                     (buffer-string))))))

(provide 'lean4-util-test)
;;; lean4-util-test.el ends here
