;;; lean4-lake.el --- Lake integration for lean4-mode -*- lexical-binding: t -*-

;; SPDX-License-Identifier: Apache-2.0
;; This file is not part of GNU Emacs.

;;; License:

;; Released under Apache 2.0 license as described in the file LICENSE.

;;; Commentary:

;; FIXME

;;; Code:

(require 'lean4-util)
(require 'lean4-settings)

(defun lean4-lake-find-dir-in (dir)
  (when dir
    (or (when (f-exists? (f-join dir "lakefile.lean")) dir)
	(lean4-lake-find-dir-in (f-parent dir)))))

(defun lean4-lake-find-dir ()
  (and (buffer-file-name)
       (lean4-lake-find-dir-in (f-dirname (buffer-file-name)))))

(defun lean4-lake-find-dir-safe ()
  (or (lean4-lake-find-dir)
      (error "cannot find lakefile.lean for %s" (buffer-file-name))))

(defun lean4-lake-build ()
  "Call lake build"
  (interactive)
  (let ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (compile (concat (lean4-get-executable lean4-lake-name) " build"))))

(provide 'lean4-lake)
;;; lean4-lake.el ends here
