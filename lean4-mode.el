;;; lean4-mode.el --- Major mode for Lean language  -*- lexical-binding: t; -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Copyright (c) 2014, 2015 Soonho Kong. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Yury G. Kudryashov <urkud@urkud.name>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (compat "28.1") (dash "2.18.0") (magit-section "2.90.1") (lsp-mode "8.0.0"))
;; URL: https://github.com/leanprover-community/lean4-mode
;; SPDX-License-Identifier: Apache-2.0
;; Version: 1.1.2

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

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;; See the README.md for more advanced features and the
;; associated keybindings.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'pcase)
(require 'lsp-mode)
(require 'lean4-eri)
(require 'lean4-syntax)
(require 'lean4-info)
(require 'lean4-fringe)
(require 'lean4-input)
(require 'lean4-markdown)
(require 'lean4-lsp)

;; Declare symbols defined in external dependencies.  This silences
;; byte-compiler warnings:
(defvar compilation-mode-font-lock-keywords)
(defvar flycheck-after-syntax-check-hook)
(defvar flycheck-disabled-checkers)
(defvar flycheck-mode)
(declare-function flycheck-list-errors "ext:flycheck")
(declare-function flymake-proc-init-create-temp-buffer-copy "flymake-proc")
(declare-function quail-show-key "quail")

(defgroup lean4 nil
  "Major mode for Lean4 programming language and theorem prover."
  :group 'languages
  :link '(info-link :tag "Info Manual" "(lean4-mode)")
  :link '(url-link
          :tag "Website"
          "https://github.com/leanprover-community/lean4-mode")
  :link '(emacs-library-link :tag "Library Source" "lean4-mode.el")
  :prefix "lean4-")

(defcustom lean4-mode-hook (list #'lsp)
  "Hook run after entering `lean4-mode'."
  :options '(flycheck-mode lsp)
  :type 'hook
  :group 'lean4)

(defcustom lean4-location-root nil
  "Lean4 project root, used as `default-directory'."
  :type 'directory
  :group 'lean4)

(defun lean4-execute ()
  "Execute Lean in the current buffer."
  (interactive)
  (let* ((default-directory
          (or lean4-location-root default-directory)))
    (compile (if-let* ((file (buffer-file-name)))
                 (concat "lean " file " ")
               "lean "))))

(defun lean4-lake-build ()
  "Build Lean4 project with Lake."
  (interactive)
  (let* ((default-directory
          (or lean4-location-root default-directory)))
    (compile "lake build ")))

(defvar-keymap lean4-mode-map
  :doc "Keymap for `lean4-mode'."
  "C-c C-x"     #'lean4-execute
  "C-c C-l"     #'lean4-execute
  "C-c C-k"     #'quail-show-key
  "TAB"         #'lean4-tab-indent
  "C-c C-i"     #'lean4-toggle-info
  "C-c C-p C-l" #'lean4-lake-build
  "C-c C-d"     #'lean4-refresh-file-dependencies)

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  `("Lean 4"
    ["Execute lean"         lean4-execute           t]
    ["Toggle info display"  lean4-toggle-info       t]
    ["Restart lean process" lsp-workspace-restart   t]
    ["Customize lean4-mode" (customize-group 'lean) t]))

(defconst lean4-hooks-alist
  '(;; info view
    ;; update errors immediately, but delay querying goal
    (flycheck-after-syntax-check-hook . lean4-info-buffer-redisplay-debounced)
    (post-command-hook . lean4-info-buffer-redisplay-debounced)
    (lsp-on-idle-hook . lean4-info-buffer-refresh))
  "Hooks which lean4-mode needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

(defun lean4-mode-setup ()
  "Default lean4-mode setup."
  ;; Right click menu sources
  ;;(setq lean4-right-click-item-functions '(lean4-info-right-click-find-definition
  ;;                                        lean4-hole-right-click))
  ;; Flycheck
  (setq-local flycheck-disabled-checkers '())
  ;; Lean massively benefits from semantic tokens, so change default to enabled
  (setq-local lsp-semantic-tokens-enable t)
  (lean4-create-lsp-workspace))

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean language.

\\{lean4-mode-map}"
  :syntax-table lean4-syntax-table
  :group 'lean4
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-start-skip) "[-/]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-/\\|\\s>\\)")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-defaults) lean4-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set 'compilation-mode-font-lock-keywords '())
  (lean4-input-set)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  (pcase-dolist (`(,hook . ,fn) lean4-hooks-alist)
    (add-hook hook fn nil 'local))
  (lean4-mode-setup))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.lean\\'" . lean4-mode))

;; Use utf-8 encoding
;;;### autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(provide 'lean4-mode)
;;; lean4-mode.el ends here
