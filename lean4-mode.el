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

;; For more information, see the README.org which is also provided as
;; Info manual.

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'lean4-eri)
(require 'lean4-fringe)
(require 'lean4-info)
(require 'lean4-syntax)
(require 'lean4-util)

(require 'dash)
(require 'lsp-mode)

;; Declare symbols defined in external dependencies.  This silences
;; byte-compiler warnings:
(defvar compilation-mode-font-lock-keywords)
(defvar flycheck-after-syntax-check-hook)
(defvar flycheck-disabled-checkers)
(defvar flycheck-mode)
(defvar lsp--cur-version)
(defvar markdown-code-lang-modes)
(declare-function flycheck-list-errors "ext:flycheck")
(declare-function flymake-proc-init-create-temp-buffer-copy "flymake-proc")
(declare-function lean-mode "ext:lean-mode")
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
  (let* ((use-lake (lean4-lake-find-dir))
         (default-directory (if use-lake (lean4-lake-find-dir)
                              default-directory))
         (target-file-name
          (or
           (buffer-file-name)
           (flymake-proc-init-create-temp-buffer-copy 'lean4-create-temp-in-system-tempdir))))
    (compile (lean4-compile-string
              (if use-lake (shell-quote-argument (expand-file-name (lean4-get-executable lean4-lake-name))) nil)
              (shell-quote-argument (expand-file-name (lean4-get-executable lean4-executable-name)))
              (or arg "")
              (shell-quote-argument (expand-file-name target-file-name))))))

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
    ;; TODO: Bug#91: We offers a Flycheck-based menu-item when
    ;; Flycheck is in use.  Users who use built-in Flymake should also
    ;; be offered a working menu-item.  Alternatively, the menu-item
    ;; could also be dropped for both cases.
    ["List of errors"       flycheck-list-errors    (bound-and-true-p flycheck-mode)]
    ["Restart lean process" lsp-workspace-restart   t]
    ["Customize lean4-mode" (customize-group 'lean) t]))

(defun lean4-lsp-create-workspace ()
  "Create an LSP workspace.

Starting from `(buffer-file-name)`, repeatedly look up the
directory hierarchy for a directory containing a file
\"lean-toolchain\", and use the last such directory found, if any.
This allows us to edit files in child packages using the settings
of the parent project."
  (let (root)
    (when-let ((file-name (buffer-file-name)))
      (while-let ((dir (locate-dominating-file file-name "lean-toolchain")))
        ;; We found a toolchain file, but maybe it belongs to a package.
        ;; Continue looking until there are no more toolchain files.
        (setq root dir
              file-name (file-name-directory (directory-file-name dir)))))
    (when root
      (lsp-workspace-folders-add root))))

(defun lean4-lsp-semantic-token-enable ()
  "Buffer-locally enable `lsp-mode's support for semantic tokens."
  (interactive)
  (setq-local lsp-semantic-tokens-enable t))

(defun lean4-set-input-method ()
  "Setup the input method for `lean4-mode'."
  (interactive)
  (require 'lean4-input)
  (set-input-method "Lean4"))

(defcustom lean4-mode-hook
  (list #'lean4-set-input-method
        #'lean4-lsp-semantic-token-enable
        #'lean4-lsp-create-workspace
        #'lsp)
  "Hook run after entering `lean4-mode'.

Note that there's no need to add `lsp-diagnostics-mode' to this hook as
it will be called by `lsp'.  Similarly, `flycheck-mode' should not be
added here because it will be called by `lsp' if the variable
`lsp-diagnostics-provider' is set accordingly."
  :options '(lean4-set-input-method
             lean4-lsp-semantic-token-enable
             lean4-lsp-create-workspace
             lsp)
  :type 'hook
  :group 'lean4)

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean4"
  "Major mode for Lean4 programming language and theorem prover.

\\{lean4-mode-map}"
  :syntax-table lean4-syntax-table
  :group 'lean4

  (setq-local comment-end
              "")
  (setq-local comment-end-skip
              "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding
              1)
  (setq-local comment-start
              "--")
  (setq-local comment-start-skip
              "[-/]-[ \t]*")
  (setq-local comment-use-syntax
              t)
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local font-lock-defaults
              lean4-font-lock-defaults)
  (setq-local lisp-indent-function
              #'common-lisp-indent-function)

  ;; Clean up whitespace before saving.
  (add-hook 'before-save-hook
            #'lean4-whitespace-cleanup
            nil 'local)
  (add-hook 'post-command-hook
            #'lean4-info-buffer-redisplay-debounced
            nil 'local)

  ;; Turn off modes that may interfere with our indentation
  ;; (`lean4-eri').
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  (indent-tabs-mode -1)

  ;; Flycheck:
  (setq-local flycheck-disabled-checkers
              nil)
  ;; In Info View, update errors immediately, but delay querying goal.
  (add-hook 'flycheck-after-syntax-check-hook
            #'lean4-info-buffer-redisplay-debounced
            nil 'local)

  ;; lsp-mode:
  (add-hook 'lsp-on-idle-hook
            #'lean4-info-buffer-refresh
            nil 'local))

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

(defcustom lean4-autodetect-lean3 nil
  "Autodetect Lean version.
Use elan to check if current project uses Lean 3 or Lean 4 and initialize the
right mode when visiting a file.  If elan has a default Lean version, Lean files
outside a project will default to that mode."
  :group 'lean4
  :type 'boolean)

;;;###autoload
(defun lean4-select-mode ()
  "Automatically select mode (Lean 3 vs Lean 4)."
  (if (and lean4-autodetect-lean3
           (eq 3 (car (lean4--version))))
      (lean-mode)
    (lean4-mode)))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.lean\\'" . lean4-select-mode))

;;;###autoload
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes
               '("lean" . lean4-select-mode)))

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

(provide 'lean4-mode)
;;; lean4-mode.el ends here
