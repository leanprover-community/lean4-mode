;;; lean4-lake-test.el --- Tests for lean4-lake  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for locating the root of a Lake package.

;;; Code:

(require 'lean4-test-helper)

(ert-deftest lean4-test-lake-root-dir-p ()
  "A directory is a Lake root when it holds either flavour of lakefile."
  (lean4-test-with-directory root '("lean/lakefile.lean"
                                    "toml/lakefile.toml"
                                    "neither/Mod.lean")
    (should (lean4-root-dir-p (expand-file-name "lean" root)))
    (should (lean4-root-dir-p (expand-file-name "toml" root)))
    (should-not (lean4-root-dir-p (expand-file-name "neither" root)))))

(ert-deftest lean4-test-lake-find-dir ()
  "The nearest ancestor holding a lakefile is the package root."
  (lean4-test-with-directory root '("pkg/lakefile.toml" "pkg/Sub/Mod.lean")
    (lean4-test-with-file-buffer (expand-file-name "pkg/Sub/Mod.lean" root)
      (should (equal (file-truename (expand-file-name "pkg/" root))
                     (file-truename (lean4-lake-find-dir)))))))

(ert-deftest lean4-test-lake-find-dir-without-lakefile ()
  "With no lakefile anywhere above, there is no package root."
  (lean4-test-with-directory root '("Sub/Mod.lean")
    (lean4-test-with-file-buffer (expand-file-name "Sub/Mod.lean" root)
      ;; `locate-dominating-file' walks all the way to "/", so this only
      ;; means anything if no ancestor of the temporary directory has a
      ;; lakefile in it -- which is why the check is skipped otherwise.
      (skip-unless (not (locate-dominating-file root #'lean4-root-dir-p)))
      (should-not (lean4-lake-find-dir)))))

(ert-deftest lean4-test-lake-find-dir-needs-a-file ()
  "A buffer not backed by a file has no package root."
  (lean4-test-with-buffer "def a : Nat := 0\n"
    (should-not (lean4-lake-find-dir))))

(ert-deftest lean4-test-lake-find-dir-safe-signals ()
  "The checked variant reports a missing lakefile as an error."
  (lean4-test-with-buffer "def a : Nat := 0\n"
    (should-error (lean4-lake-find-dir-safe) :type 'error)))

(provide 'lean4-lake-test)
;;; lean4-lake-test.el ends here
