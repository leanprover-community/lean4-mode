;;; lean4-lake.el --- Lake integration for lean4-mode -*- lexical-binding: t -*-

;; SPDX-License-Identifier: Apache-2.0
;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at:
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This library provides integration with Lake, Lean 4 build system and package
;; manager.

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
