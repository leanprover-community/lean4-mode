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
  "Find a parent directory of DIR with file \"lakefile.lean\"."
  (when dir
    (or (when (file-exists-p (expand-file-name "lakefile.lean" dir)) dir)
	(lean4-lake-find-dir-in (file-name-directory (directory-file-name dir))))))

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

(defun lean4-lake-file-path (dir)
  "Return the full path of lakefile.lean in DIR."
  (expand-file-name "lakefile.lean" dir))

(defun lean4-lake-file-exists-p (dir)
  "Check if lakefile.lean exists in DIR."
  (file-exists-p (lean4-lake-file-path dir)))

(defun lean4-lake-read-file (file)
  "Read contents of FILE into a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun lean4-lake-extract-executables (content)
  "Extract executable names from lakefile CONTENT."
  (with-temp-buffer
    (insert content)
    (lean4-lake-extract-executables-from-buffer)))

(defun lean4-lake-extract-executables-from-buffer ()
  "Extract executable names from the current buffer."
  (let ((executables '()))
    (setq executables (append executables (lean4-lake-extract-guillemet-executables)))
    (setq executables (append executables (lean4-lake-extract-quoted-executables)))
    (setq executables (append executables (lean4-lake-extract-unquoted-executables)))
    executables))

(defun lean4-lake-extract-guillemet-executables ()
  "Extract executable names enclosed in guillemets («»)."
  (lean4-lake-extract-executables-with-pattern "lean_exe\\s-+«\\([^»]+\\)»\\s-+where"))

(defun lean4-lake-extract-quoted-executables ()
  "Extract executable names enclosed in double quotes."
  (lean4-lake-extract-executables-with-pattern "lean_exe\\s-+\"\\([^\"]+\\)\"\\s-+where"))

(defun lean4-lake-extract-unquoted-executables ()
  "Extract unquoted executable names."
  (lean4-lake-extract-executables-with-pattern "lean_exe\\s-+\\([^\\s-\n]+\\)\\s-+where"))

(defun lean4-lake-extract-executables-with-pattern (pattern)
  "Extract executable names matching PATTERN."
  (let (executables)
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (push (match-string 1) executables))
    executables))

(defun lean4-lake-get-executables (lake-dir)
  "Get executables from lakefile.lean in LAKE-DIR."
  (let ((lakefile (expand-file-name "lakefile.lean" lake-dir)))
    (when (file-exists-p lakefile)
      (lean4-lake-extract-executables
       (with-temp-buffer
         (insert-file-contents lakefile)
         (buffer-string))))))

(defun lean4-lake-exe ()
  "Open a menu of executables from lakefile.lean and run the selected one."
  (interactive)
  (let* ((lake-dir (lean4-lake-find-dir-safe))
         (lake-default-directory (file-name-as-directory lake-dir))
         (executables (lean4-lake-get-executables lake-dir)))
    (if executables
        (let* ((selected (completing-read "Select executable: " executables nil t))
               (buffer-name (format "*lean4-lake-exe-%s*" selected))
               (existing-buffer (get-buffer buffer-name)))
          (when existing-buffer
            (kill-buffer existing-buffer))
          (when selected
            (let ((buffer (get-buffer-create buffer-name)))
              (with-current-buffer buffer
                (erase-buffer)
                (compilation-mode))
              (display-buffer buffer)
              (let ((default-directory lake-default-directory))
                (start-process selected buffer
                               (lean4-get-executable lean4-lake-name) "exe" selected)))))
      (message "No executables found in lakefile.lean. Make sure the file contains 'lean_exe' declarations."))))

(provide 'lean4-lake)
;;; lean4-lake.el ends here
