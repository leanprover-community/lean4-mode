;;; lean4-debug.el --- Debug mode for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.

;; Author: Soonho Kong
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

;; This library provides a debug mode for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defvar lean4-debug-mode nil)

(defvar lean4-debug-buffer-name "*lean4-debug*")

(defun lean4-turn-on-debug-mode (&optional print-msg)
  (interactive)
  (when (or (called-interactively-p 'any) print-msg)
    (message "lean: turn on debug mode"))
  (get-buffer-create lean4-debug-buffer-name)
  (buffer-disable-undo lean4-debug-buffer-name)
  (display-buffer lean4-debug-buffer-name 'display-buffer-reuse-window
                  '((reusable-frames . t)))
  (setq lean4-debug-mode t))

(defun lean4-turn-off-debug-mode (&optional print-msg)
  (interactive)
  (when (eq major-mode 'lean4-mode)
    (when (or (called-interactively-p 'any) print-msg)
      (message "lean: turn off debug mode"))
    (setq lean4-debug-mode nil)))

(defun lean4-output-to-buffer (buffer-name format-string args)
  (with-current-buffer
      (get-buffer-create buffer-name)
    (save-selected-window
      (ignore-errors
        (select-window (get-buffer-window buffer-name t)))
      (goto-char (point-max))
      (insert (apply #'format format-string args)))))

(defun lean4-debug (format-string &rest args)
  "Display a message at the bottom of the *lean4-debug* buffer."
  (when lean4-debug-mode
    (let ((time-str (format-time-string "%T.%3N" (current-time))))
      (lean4-output-to-buffer lean4-debug-buffer-name
                             (concat "%s -- " format-string "\n")
                             (cons (propertize time-str 'face 'font-lock-keyword-face)
                                   args)))))

(provide 'lean4-debug)
;;; lean4-debug.el ends here
