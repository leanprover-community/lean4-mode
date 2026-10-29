;;; checkdoc-batch.el --- Run checkdoc in batch mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; `checkdoc-file' reports its findings through the warning system and
;; always exits successfully, which makes it useless as a CI gate.  Count
;; what it reports and exit non-zero if anything turned up.

;;; Code:

(require 'checkdoc)

;; checkdoc describes its verb-voice check as experimental, and it
;; misfires here: it flags verbs anywhere in the summary line rather
;; than only the leading one, so "Check if DIR contains ..." draws a
;; complaint about "contains" despite already being imperative.
(setq checkdoc-verb-check-experimental-flag nil)

(defun lean4-checkdoc-batch ()
  "Run checkdoc over every lean4-*.el file in `default-directory'.
Exit with status 1 if checkdoc reported anything, 0 otherwise."
  (let ((findings 0)
        (files (sort (directory-files default-directory nil
                                      "\\`lean4-.*\\.el\\'")
                     #'string<)))
    (advice-add 'display-warning :before
                (lambda (&rest _) (setq findings (1+ findings)))
                '((name . lean4-checkdoc-count)))
    (unwind-protect
        (dolist (file files)
          (checkdoc-file file))
      (advice-remove 'display-warning 'lean4-checkdoc-count))
    (message "checkdoc: %d file(s) checked, %d finding(s)"
             (length files) findings)
    (kill-emacs (if (zerop findings) 0 1))))

(lean4-checkdoc-batch)

;;; checkdoc-batch.el ends here
