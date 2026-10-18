;;; lean4-markdown.el --- Lean4 markdown  -*- lexical-binding: t; -*-

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

;; This file mutates `markdown-code-lang-modes' so that
;; `markdown-mode' supports Lean4 code blocks.

;;; Code:

(require 'markdown-mode)

(add-to-list 'markdown-code-lang-modes
             '("lean4" . lean4-mode))

(provide 'lean4-markdown)
;;; lean4-markdown.el ends here
