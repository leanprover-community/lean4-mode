;;; lean4-mode.el --- Lean4 major mode  -*- lexical-binding: t; -*-

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

;; Provides a major mode for the Lean4 programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean4 users.

;; See the README.md for more advanced features and the
;; associated keybindings.

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'dash)

(require 'lean4-eri)
(require 'lean4-fringe)
(require 'lean4-info)
(require 'lean4-input)
(require 'lean4-lsp)
(require 'lean4-markdown)
(require 'lean4-syntax)

;; Declare symbols defined in external dependencies.  This silences
;; byte-compiler warnings:
(defvar compilation-mode-font-lock-keywords)
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

(defcustom lean4-mode-hook (list #'lean4-input-set #'lean4-lsp-mode)
  "Hook run after entering `lean4-mode'."
  :options '(flycheck-mode lean4-input-set lean4-lsp-mode)
  :type 'hook
  :group 'lean4)

(defcustom lean4-location-root nil
  "Lean4 project root, used as `default-directory'."
  :type 'directory
  :group 'lean4)

(defun lean4-execute ()
  "Execute Lean4 in the current buffer."
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
  "TAB"         #'lean4-eri-tab
  "C-c C-i"     #'lean4-info-mode
  "C-c C-p C-l" #'lean4-lake-build)

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean4 major mode."
  `("Lean4"
    ["Execute Lean4" lean4-execute           t]
    ["Toggle Info"   lean4-info-mode         t]
    ["Customize"     (customize-group 'lean) t]))

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean4"
  "Major mode for Lean4 language.

\\{lean4-mode-map}"
  :syntax-table lean4-syntax-table
  :group 'lean4
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults lean4-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-indent-function 'common-lisp-indent-function)
  (lean4-input-set)
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.lean\\'" . lean4-mode))

;; Use utf-8 encoding
;;;###autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(provide 'lean4-mode)
;;; lean4-mode.el ends here
