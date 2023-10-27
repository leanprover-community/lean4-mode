;;; lean4-dev.el --- Development commands for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2017 Microsoft Corporation. All rights reserved.

;; Author: Sebastian Ullrich
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

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

;; This library currently provides `lean4-diff-test-file' command.

;;; Code:

(require 'lean4-util)

(defun lean4-diff-test-file ()
  "Use interactive ./test_input.sh on file of current buffer."
  (interactive)
  (save-buffer)
                                        ; yes: auto-agree to copying missing files
  (message (shell-command-to-string (format "yes | PATH=%s/bin:$PATH LEAN_NIX_ARGS=--quiet ./test_single.sh -i \"%s\"" (lean4-get-rootdir) (file-name-nondirectory (buffer-file-name))))))

(provide 'lean4-dev)
;;; lean4-dev.el ends here
