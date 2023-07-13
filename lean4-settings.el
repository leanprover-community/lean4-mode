;;; lean4-settings.el --- Custom variables for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
;; SPDX-License-Identifier: Apache-2.0
;;

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

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defgroup lean4 nil
  "Lean 4 programming language and theorem prover."
  :prefix "lean4-"
  :group 'languages
  :link '(url-link :tag "Website" "http://leanprover.github.io")
  :link '(url-link :tag "Github"  "https://github.com/leanprover/lean4"))

(defgroup lean4-keybinding nil
  "Keybindings for lean4-mode."
  :prefix "lean4-"
  :group 'lean)

(defconst lean4-default-executable-name
  (cl-case system-type
    (windows-nt "lean.exe")
    (t          "lean"))
  "Default executable name of Lean.")

(defconst lean4-default-lake-name
  (cl-case system-type
    (windows-nt "lake.exe")
    (t          "lake"))
  "Default executable name of Lake.")

(defcustom lean4-rootdir nil
  "Full pathname of lean root directory.  It should be defined by user."
  :group 'lean
  :type 'string)

(defcustom lean4-executable-name lean4-default-executable-name
  "Name of lean executable."
  :group 'lean
  :type 'string)

(defcustom lean4-lake-name lean4-default-lake-name
  "Name of lake executable."
  :group 'lake
  :type 'string)

(defcustom lean4-memory-limit 1024
  "Memory limit for lean process in megabytes."
  :group 'lean
  :type 'number)

(defcustom lean4-timeout-limit 100000
  "Deterministic timeout limit.

It is approximately the maximum number of memory allocations in thousands."
  :group 'lean
  :type 'number)

(defcustom lean4-extra-arguments nil
  "Extra command-line arguments to the lean process."
  :group 'lean
  :type '(list string))

(defcustom lean4-delete-trailing-whitespace nil
  "Automatically delete trailing shitespace.
Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :group 'lean
  :type 'boolean)

(defcustom lean4-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.
Set this variable to t to highlight inaccessible names in the info display
using `font-lock-comment-face' instead of the `✝` suffix used by Lean."
  :group 'lean
  :type 'boolean)

(defcustom lean4-show-file-progress t
  "Highlight file progress in the current buffer."
  :group 'lean
  :type 'boolean)


(defcustom lean4-autodetect-lean3 nil
  "Autodetect Lean version.
Use elan to check if current project uses Lean 3 or Lean 4 and initialize the
right mode when visiting a file.  If elan has a default Lean version, Lean files
outside a project will default to that mode."
  :group 'lean
  :type 'boolean)

(defcustom lean4-keybinding-std-exe1 (kbd "C-c C-x")
  "Main Keybinding for `lean4-std-exe'."
  :group 'lean4-keybinding :type 'key-sequence)
(defcustom lean4-keybinding-std-exe2 (kbd "C-c C-l")
  "Alternative Keybinding for `lean4-std-exe'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-show-key (kbd "C-c C-k")
  "Lean Keybinding for `quail-show-key'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-server-restart (kbd "C-c C-r")
  "Lean Keybinding for server-restart."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-server-switch-version (kbd "C-c C-s")
  "Lean Keybinding for `lean4-server-switch-version'."
  :group 'lean4-keybinding :type 'key-sequence)
(defcustom lean4-keybinding-tab-indent (kbd "TAB")
  "Lean Keybinding for `lean4-tab-indent'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-auto-complete (kbd "S-SPC")
  "Lean Keybinding for auto completion."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-hole (kbd "C-c SPC")
  "Lean Keybinding for hole manipulation."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-lean4-toggle-info (kbd "C-c C-i")
  "Lean Keybinding for `lean4-toggle-info'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-lake-build (kbd "C-c C-p C-l")
  "Lean Keybinding for `lean4-lake-build'."
  :group 'lean4-keybinding :type 'key-sequence)
(defcustom lean4-keybinding-refresh-file-dependencies (kbd "C-c C-d")
  "Lean Keybinding for `lean4-refresh-file-dependencies'."
  :group 'lean4-keybinding :type 'key-sequence)

(provide 'lean4-settings)
;;; lean4-settings.el ends here
