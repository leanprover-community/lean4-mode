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

(defgroup lean4 nil
  "Major mode for Lean4 programming language and theorem prover."
  :group 'languages
  :link '(info-link :tag "Info Manual" "(lean4-mode)")
  :link '(url-link
          :tag "Website"
          "https://github.com/leanprover-community/lean4-mode")
  :link '(emacs-library-link :tag "Library Source" "lean4-mode.el")
  :prefix "lean4-")

(defgroup lean4-keybinding nil
  "Keybindings for lean4-mode."
  :prefix "lean4-"
  :group 'lean4)

(defcustom lean4-mode-hook (list #'lsp)
  "Hook run after entering `lean4-mode'."
  :options '(flycheck-mode lsp)
  :type 'hook
  :group 'lean4)

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

(defcustom lean4-keybinding-std-exe1 (kbd "C-c C-x")
  "Main Keybinding for `lean4-execute'."
  :group 'lean4-keybinding :type 'key-sequence)
(defcustom lean4-keybinding-std-exe2 (kbd "C-c C-l")
  "Alternative Keybinding for `lean4-execute'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-show-key (kbd "C-c C-k")
  "Lean Keybinding for `quail-show-key'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-server-restart (kbd "C-c C-r")
  "Lean Keybinding for server-restart."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-tab-indent (kbd "TAB")
  "Lean Keybinding for `lean4-tab-indent'."
  :group 'lean4-keybinding  :type 'key-sequence)
(defcustom lean4-keybinding-auto-complete (kbd "S-SPC")
  "Lean Keybinding for auto completion."
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
