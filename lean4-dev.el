;;; lean4-debug.el --- Debug mode for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2017 Microsoft Corporation. All rights reserved.

;; Author: Sebastian Ullrich

;; This file is not part of GNU Emacs.

;;; License:

;; Released under Apache 2.0 license as described in the file LICENSE.

;;; Code:

(require 'f)
(require 'lean4-util)

(defun lean4-diff-test-file ()
  "Use interactive ./test_input.sh on file of current buffer"
  (interactive)
  (save-buffer)
  ; yes: auto-agree to copying missing files
  (message (shell-command-to-string (format "yes | PATH=%s/bin:$PATH LEAN_NIX_ARGS=--quiet ./test_single.sh -i \"%s\"" (lean4-get-rootdir) (f-filename (buffer-file-name))))))

(provide 'lean4-dev)
;;; lean4-dev.el ends here
