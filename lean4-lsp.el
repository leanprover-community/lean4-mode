;;; lean4-lsp.el --- Lean4 lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Mekeor Melire

;; This file is NOT part of GNU Emacs.

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

;; This file connects `lean4-mode' and `lsp-mode' packages.

;;; Code:

(require 'lsp-mode)

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

(defun lean4-create-lsp-workspace ()
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

;;;; Registration

(add-to-list 'lsp-language-id-configuration
             '(lean4-mode . "lean"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("lake" "serve"))
                  :major-modes '(lean4-mode)
                  :server-id 'lean4-lsp
                  :notification-handlers (ht ("$/lean/fileProgress" #'lean4-fringe-update))
                  :semantic-tokens-faces-overrides '(:types (("leanSorryLike" . font-lock-warning-face)))))

(provide 'lean4-lsp)
;;; lean4-lsp.el ends here
