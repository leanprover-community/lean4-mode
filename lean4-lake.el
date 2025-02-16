;;; lean4-lake.el --- Lean4-Mode Lake Integration  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;; Commentary:

;; This library provides integration with Lake, Lean 4 build system and package
;; manager.

;;; Code:

(require 'lean4-util)
(require 'lean4-settings)

(defun lean4-lake-find-dir-in (dir)
  "Find a parent directory of DIR with file \"lakefile.lean\" or
  \"lakefile.toml\"."
  (locate-dominating-file dir "lakefile.lean"))

(defun lean4-lake-find-dir ()
  "Find a parent directory of the current file with file \"lakefile.lean\"."
  (and (buffer-file-name)
       (lean4-lake-find-dir-in (directory-file-name (buffer-file-name)))))

(defun lean4-lake-find-dir-safe ()
  "Call `lean4-lake-find-dir', error on failure."
  (or (lean4-lake-find-dir)
      (error "Cannot find lakefile.lean for %s" (buffer-file-name))))

(defun lean4-lake-build ()
  "Call lake build."
  (interactive)
  (let ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (compile (concat (lean4-get-executable lean4-lake-name) " build"))))

(provide 'lean4-lake)
;;; lean4-lake.el ends here
