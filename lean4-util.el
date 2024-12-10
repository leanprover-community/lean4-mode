;;; lean4-util.el --- Lean4-Mode Utilities  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

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

;; This library provides utilities for `lean4-mode'.

;;; Code:

(require 'compat)

(defcustom lean4-executable-name
  (if (eq system-type 'windows-nt) "lean.exe" "lean")
  "Name of lean executable."
  :group 'lean4
  :type 'string)

(defcustom lean4-rootdir nil
  "Full pathname of lean root directory.  It should be defined by user."
  :group 'lean4
  :type 'string)

(defun lean4-setup-rootdir ()
  "Search for lean executable in variable `exec-path'.
Try to find an executable named `lean4-executable-name' in variable `exec-path'.
On succsess, return path to the directory with this executable."
  (let ((root (executable-find lean4-executable-name)))
    (when root
      (setq lean4-rootdir (file-name-directory
                           (directory-file-name
                            (file-name-directory root)))))
    lean4-rootdir))

(defun lean4-get-rootdir ()
  "Search for lean executable in `lean4-rootdir' and variable `exec-path'.
First try to find an executable named `lean4-executable-name' in
`lean4-rootdir'.  On failure, search in variable `exec-path'."
  (if lean4-rootdir
      (let ((lean4-path (expand-file-name lean4-executable-name (expand-file-name "bin" lean4-rootdir))))
        (unless (file-exists-p lean4-path)
          (error "Incorrect `lean4-rootdir' value, path '%s' does not exist" lean4-path))
        lean4-rootdir)
    (or
     (lean4-setup-rootdir)
     (error
      (concat "Lean was not found in the `exec-path' and `lean4-rootdir' is not defined. "
              "Please set it via M-x customize-variable RET lean4-rootdir RET.")))))

(defun lean4-get-executable (exe-name)
  "Return fullpath of lean executable EXE-NAME."
  (file-name-concat (lean4-get-rootdir) "bin" exe-name))

(defcustom lean4-delete-trailing-whitespace nil
  "Automatically delete trailing shitespace.
Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :group 'lean4
  :type 'boolean)

(defun lean4-whitespace-cleanup ()
  "Delete trailing whitespace if `lean4-delete-trailing-whitespace' is t."
  (when lean4-delete-trailing-whitespace
      (delete-trailing-whitespace)))

(defcustom lean4-lake-name
  (if (eq system-type 'windows-nt) "lake.exe" "lake")
  "Name of lake executable."
  :group 'lake
  :type 'string)

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

(provide 'lean4-util)
;;; lean4-util.el ends here
