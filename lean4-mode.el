;;; lean4-mode.el --- A major mode for the Lean language -*- lexical-binding: t -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Copyright (c) 2014, 2015 Soonho Kong. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Sebastian Ullrich <sebasti@nullri.ch>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (flycheck "30") (magit-section "2.90.1") (lsp-mode "8.0.0"))
;; URL: https://github.com/leanprover/lean4-mode
;; SPDX-License-Identifier: Apache-2.0

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

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;; See the README.md for more advanced features and the
;; associated keybindings.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'pcase)
(require 'flycheck)
(require 'lsp-mode)
(require 'lean4-eri)
(require 'lean4-util)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-info)
(require 'lean4-dev)
(require 'lean4-fringe)
(require 'lean4-lake)

;; Silence byte-compiler
(defvar lsp--cur-version)
(defvar markdown-code-lang-modes)
(defvar compilation-mode-font-lock-keywords)
(declare-function lean-mode "ext:lean-mode")
(declare-function flymake-proc-init-create-temp-buffer-copy "flymake-proc")
(declare-function quail-show-key "quail")

(defun lean4-compile-string (lake-name exe-name args file-name)
  "Command to run EXE-NAME with extra ARGS and FILE-NAME.
If LAKE-NAME is nonempty, then prepend \"LAKE-NAME env\" to the command
\"EXE-NAME ARGS FILE-NAME\"."
  (if lake-name
      (format "%s env %s %s %s" lake-name exe-name args file-name)
      (format "%s %s %s" exe-name args file-name)))

(defun lean4-create-temp-in-system-tempdir (file-name prefix)
  "Create a temp lean file and return its name.
The new file has prefix PREFIX (defaults to `flymake') and the same extension as
FILE-NAME."
  (make-temp-file (or prefix "flymake") nil (file-name-extension file-name)))

(defun lean4-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  (when (called-interactively-p 'any)
    (setq arg (read-string "arg: " arg)))
  (let* ((cc compile-command)
	 (dd default-directory)
	 (use-lake (lean4-lake-find-dir))
	 (default-directory (if use-lake (lean4-lake-find-dir) dd))
         (target-file-name
          (or
           (buffer-file-name)
           (flymake-proc-init-create-temp-buffer-copy 'lean4-create-temp-in-system-tempdir))))
    (compile (lean4-compile-string
	      (if use-lake (shell-quote-argument (expand-file-name (lean4-get-executable lean4-lake-name))) nil)
              (shell-quote-argument (expand-file-name (lean4-get-executable lean4-executable-name)))
              (or arg "")
              (shell-quote-argument (expand-file-name target-file-name))))
    ;; restore old value
    (setq compile-command cc)
    (setq default-directory dd)))

(defun lean4-std-exe ()
  "Execute Lean in the current buffer."
  (interactive)
  (lean4-execute))

(defun lean4-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (lsp-notify
   "textDocument/didClose"
   `(:textDocument ,(lsp--text-document-identifier)))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (lsp-buffer-language)
               :version lsp--cur-version
               :text (lsp--buffer-content)))))

(defun lean4-tab-indent ()
  "Lean 4 function for TAB indent."
  (interactive)
  (cond ((looking-back (rx line-start (* white)) nil)
         (lean4-eri-indent))
        (t (indent-for-tab-command))))

(defun lean4-set-keys ()
  "Setup Lean 4 keybindings."
  (local-set-key lean4-keybinding-std-exe1                  #'lean4-std-exe)
  (local-set-key lean4-keybinding-std-exe2                  #'lean4-std-exe)
  (local-set-key lean4-keybinding-show-key                  #'quail-show-key)
  (local-set-key lean4-keybinding-tab-indent                #'lean4-tab-indent)
  ;; (local-set-key lean4-keybinding-hole                      #'lean4-hole)
  (local-set-key lean4-keybinding-lean4-toggle-info         #'lean4-toggle-info)
  ;; (local-set-key lean4-keybinding-lean4-message-boxes-toggle #'lean4-message-boxes-toggle)
  (local-set-key lean4-keybinding-lake-build                #'lean4-lake-build)
  (local-set-key lean4-keybinding-refresh-file-dependencies #'lean4-refresh-file-dependencies)
  ;; This only works as a mouse binding due to the event, so it is not abstracted
  ;; to avoid user confusion.
  ;; (local-set-key (kbd "<mouse-3>")                         #'lean4-right-click-show-menu)
  )

(define-abbrev-table 'lean4-abbrev-table
  '())

(defvar lean4-mode-map (make-sparse-keymap)
  "Keymap used in Lean mode.")

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  `("Lean 4"
    ["Execute lean"         lean4-execute                      t]
    ["Toggle info display"  lean4-toggle-info                  t]
    ["List of errors"       flycheck-list-errors               flycheck-mode]
    ["Restart lean process" lsp-workspace-restart              t]
    ["Customize lean4-mode" (customize-group 'lean)            t]))

(defconst lean4-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (before-save-hook . lean4-whitespace-cleanup)
    ;; info view
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

(defun lean4-create-lsp-workspace ()
  "Create an LSP workspace.
This will allow us to use Emacs when a repo contains multiple lean packages."
  (when-let ((file-name (buffer-file-name))
             (root (vc-find-root (buffer-file-name)
                                 "lakefile.lean")))
    (lsp-workspace-folders-add root)))

;; Automode List
;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean.
\\{lean4-mode-map}
Invokes `lean4-mode-hook'."
  :syntax-table lean4-syntax-table
  :abbrev-table lean4-abbrev-table
  :group 'lean
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-start-skip) "[-/]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-/\\|\\s>\\)")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-defaults) lean4-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set 'compilation-mode-font-lock-keywords '())
  (require 'lean4-input)
  (set-input-method "Lean")
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (lean4-set-keys)
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  ;; (abbrev-mode 1)
  (pcase-dolist (`(,hook . ,fn) lean4-hooks-alist)
    (add-hook hook fn nil 'local))
  (lean4-mode-setup))

(defun lean4--version ()
  "Return Lean version as a list `(MAJOR MINOR PATCH)'."
  (with-temp-buffer
    (call-process (lean4-get-executable "lean") nil (list t nil) nil "-v")
    (goto-char (point-min))
    (re-search-forward (rx bol "Lean (version " (group (+ digit) (+ "." (+ digit)))))
    (version-to-list (match-string 1))))

(defun lean4-show-version ()
  "Print Lean 4 version."
  (interactive)
  (message "Lean %s" (mapconcat #'number-to-string (lean4--version) ".")))

;;;###autoload
(defun lean4-select-mode ()
  "Automatically select mode (Lean 3 vs Lean 4)."
  (if (and lean4-autodetect-lean3
           (eq 3 (car (lean4--version))))
      (lean-mode)
    (lean4-mode)))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(push '("\\.lean$" . lean4-select-mode) auto-mode-alist)

;;;###autoload
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("lean" . lean4-select-mode)))

;; Use utf-8 encoding
;;;### autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

;; LSP init
;; Ref: https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/
(add-to-list 'lsp-language-id-configuration
             '(lean4-mode . "lean"))

(defun lean4--server-cmd ()
  "Return Lean server command.
If found lake version at least 3.1.0, then return '/path/to/lake serve',
otherwise return '/path/to/lean --server'."
  (condition-case nil
      (if (string-version-lessp (car (process-lines (lean4-get-executable "lake") "--version")) "3.1.0")
          `(,(lean4-get-executable lean4-executable-name) "--server")
        `(,(lean4-get-executable "lake") "serve"))
    (error `(,(lean4-get-executable lean4-executable-name) "--server"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lean4--server-cmd)
                  :major-modes '(lean4-mode)
                  :server-id 'lean4-lsp
                  :notification-handlers (ht ("$/lean/fileProgress" #'lean4-fringe-update))
                  :semantic-tokens-faces-overrides '(:types (("leanSorryLike" . font-lock-warning-face)))))

(add-hook 'lean4-mode-hook #'lsp)

(provide 'lean4-mode)
;;; lean4-mode.el ends here
