;;; lean4-settings.el --- Lean4-Mode User-Options  -*- lexical-binding: t; -*-

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

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'lsp-mode)

(defgroup lean4 nil
  "Major mode for Lean4 programming language and theorem prover."
  :group 'languages
  :link '(info-link :tag "Info Manual" "(lean4-mode)")
  :link '(url-link
          :tag "Website"
          "https://github.com/leanprover-community/lean4-mode")
  :link '(emacs-library-link :tag "Library Source" "lean4-mode.el")
  :prefix "lean4-")

(defcustom lean4-rootdir nil
  "Full pathname of lean root directory.  It should be defined by user."
  :group 'lean4
  :type 'string)

(defcustom lean4-executable-name
  (if (eq system-type 'windows-nt) "lean.exe" "lean")
  "Name of lean executable."
  :group 'lean4
  :type 'string)

(defcustom lean4-lake-name
  (if (eq system-type 'windows-nt) "lake.exe" "lake")
  "Name of lake executable."
  :group 'lake
  :type 'string)

(defcustom lean4-memory-limit 1024
  "Memory limit for lean process in megabytes."
  :group 'lean4
  :type 'number)

(defcustom lean4-timeout-limit 100000
  "Deterministic timeout limit.

It is approximately the maximum number of memory allocations in thousands."
  :group 'lean4
  :type 'number)

(defcustom lean4-extra-arguments nil
  "Extra command-line arguments to the lean process."
  :group 'lean4
  :type '(list string))

(defcustom lean4-delete-trailing-whitespace nil
  "Automatically delete trailing shitespace.
Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.
Set this variable to t to highlight inaccessible names in the info display
using `font-lock-comment-face' instead of the `✝` suffix used by Lean."
  :group 'lean4
  :type 'boolean)

(defcustom lean4-show-file-progress t
  "Highlight file progress in the current buffer."
  :group 'lean4
  :type 'boolean)


(defcustom lean4-autodetect-lean3 nil
  "Autodetect Lean version.
Use elan to check if current project uses Lean 3 or Lean 4 and initialize the
right mode when visiting a file.  If elan has a default Lean version, Lean files
outside a project will default to that mode."
  :group 'lean4
  :type 'boolean)

(provide 'lean4-settings)
;;; lean4-settings.el ends here
