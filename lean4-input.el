;;; lean4-input.el --- The Lean input method (based/copied from Agda) -*- lexical-binding: t -*-
;;;
;;; DISCLAIMER: This file is based on agda-input.el provided with the Agda language.
;;; We did minor modifications
;; SPDX-License-Identifier: Apache-2.0
;;

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
;;
;;;; A highly customisable input method which can inherit from other
;; Quail input methods.  By default the input method is geared towards
;; the input of mathematical and other symbols in Lean programs.
;;
;; Use M-x customize-group lean4-input to customise this input method.
;; Note that the functions defined under "Functions used to tweak
;; translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the
;; ones added specifically for this one.
;;
;; Use lean4-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

;;; Code:

(require 'quail)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'map)

;; Quail is quite stateful, so be careful when editing this code.  Note
;; that with-temp-buffer is used below whenever buffer-local state is
;; modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun lean4-input-concat-map (f xs)
  "Concat (map F XS)."
  (apply #'append (mapcar f xs)))

(defun lean4-input-to-string-list (s)
  "Convert a string S to a list of one-character strings.
First remove all space and newline characters."
  (lean4-input-concat-map
   (lambda (c) (if (member c (string-to-list " \n"))
              nil
            (list (string c))))
   (string-to-list s)))

(defun lean4-input-character-range (from to)
  "A string consisting of the characters from FROM to TO."
  (let (seq)
    (dotimes (i (1+ (- to from)))
      (setq seq (cons (+ from i) seq)))
    (concat (nreverse seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs

(defun lean4-input-compose (f g)
  "\\x -> concatMap F (G x)."
  (lambda (x) (lean4-input-concat-map f (funcall g x))))

(defun lean4-input-or (f g)
  "\\x -> F x ++ G x."
  (lambda (x) (append (funcall f x) (funcall g x))))

(defun lean4-input-nonempty ()
  "Only keep pairs with a non-empty first component."
  (lambda (x) (if (> (length (car x)) 0) (list x))))

(defun lean4-input-prepend (prefix)
  "Prepend PREFIX to all key sequences."
    (lambda (x) `((,(concat prefix (car x)) . ,(cdr x)))))

(defun lean4-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
  (lambda (x)
    (if (equal (substring (car x) 0 (length prefix)) prefix)
        (list x))))

(defun lean4-input-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX."
  (lambda (x)
    (if (equal (substring (car x)
                          (- (length (car x)) (length suffix)))
               suffix)
        (list x))))

(defun lean4-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
  (lambda (x) (unless (member (car x) ss) (list x))))

(defun lean4-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
  (lambda (x) `((,(substring (car x) n) . ,(cdr x)))))

(defun lean4-input-drop-end (n)
  "Drop N characters from the end of each key sequence."
  (lambda (x)
    `((,(substring (car x) 0 (- (length (car x)) n)) .
       ,(cdr x)))))

(defun lean4-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (lean4-input-compose
   (lean4-input-drop-beginning (length prefix))
   (lean4-input-prefix prefix)))

(defun lean4-input-drop-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX.
This suffix is dropped."
  (lean4-input-compose
   (lean4-input-drop-end (length suffix))
   (lean4-input-suffix suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is set to 'lean4-input-incorporate-changed-setting
;; so that the input method gets updated immediately when users
;; customize it. However, the setup functions cannot be run before all
;; variables have been defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not performed
;; until lean4-input-setup is called at the end of this file.

(defgroup lean4-input nil
  "The Lean input method.
After tweaking these settings you may want to inspect the resulting
translations using `lean4-input-show-translations'."
  :group 'lean
  :group 'leim)

(defcustom lean4-input-inherit
  `(("TeX" . (lean4-input-compose
              (lean4-input-drop '("geq" "leq" "bullet" "qed" "par"))
              (lean4-input-or
               (lean4-input-drop-prefix "\\")
               (lean4-input-or
                (lean4-input-compose
                 (lean4-input-drop '("^o"))
                 (lean4-input-prefix "^"))
                (lean4-input-prefix "_"))))))
  "List of parent Quail input methods.
Translations from these methods will be inherited by the Lean
input method (with the exception of translations corresponding to
ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same signature as
the argument of `lean4-input-add-translations'.

The inherited translation pairs are added last, after
`lean4-input-user-translations' and `lean4-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `lean4-input-setup' in
order for the change to take effect."
  :group 'lean4-input
  :set 'lean4-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom lean4-input-data-directory
  (expand-file-name "data/" (file-name-directory (or load-file-name (buffer-file-name))))
  "Directory in which translations.json resides."
  :group 'lean4-input
  :type 'directory)

(defcustom lean4-input-user-translations nil
  "A list of translations specific to the Lean input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

These translation pairs are included first, before thoseinherited
from other input methods."
  :group 'lean4-input
  :set 'lean4-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun lean4-input-get-translations (qp)
  "Return all translations from the Quail package QP.
Result is a list of pairs (KEY-SEQUENCE . TRANSLATION)
that contains all translations from QP Except for those corresponding to ASCII."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package" qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defun lean4-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "Lean")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (lean4-input-get-translations qp)))))))

(defun lean4-input-add-translations (trans)
  "Add the given translations TRANS to the Lean input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION).  The
translations are appended to the current translations."
  (with-temp-buffer
    (map-do (lambda (key tr)
              (when key
                (quail-defrule (concat "\\" key)
                               tr
                               "Lean" t)))
            trans)))

(defun lean4-input-inherit-package (qp &optional fun)
  "Inherit translations from the Quail package QP.
Add all translations from the Quail package QP (except for those
corresponding to ASCII) to the list of Lean Quail rules.

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (lean4-input-get-translations qp)))
    (lean4-input-add-translations
     (if fun (lean4-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defvar json-key-type)
(declare-function json-read "json")

(defun lean4-input-setup ()
  "Set up the Lean input method.
Use customisable variables and parent input methods to setup Lean input method."

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package "Lean" "UTF-8" "∏" t ; guidance
     "Lean input method.
The purpose of this input method is to edit Lean programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

  (lean4-input-add-translations (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
                                        lean4-input-user-translations))
  (with-temp-buffer
    (insert-file-contents (expand-file-name
                           "abbreviations.json"
                           lean4-input-data-directory))
    (thread-last
      (let ((json-key-type 'string)) ;; make sure json key is a string.
        ;; Prefer emacs native support implemented in C (since 27.1).
        ;; Back-up is still useful in case Emacs in not compiled `--with-json`.
        (if (fboundp 'json-parse-buffer)
            (json-parse-buffer)
          (require 'json)
          (json-read)))
      (map-filter (lambda (_ s) (not (string-match-p "\\$CURSOR" s))))
      (map-apply (lambda (k s) (cons k (vector s))))
      lean4-input-add-translations))
  (dolist (def lean4-input-inherit)
    (lean4-input-inherit-package (car def)
                                (eval (cdr def)))))

(defun lean4-input-incorporate-changed-setting (sym val)
  "Update the Lean input method.
Set SYM default value to VAL, then call `lean4-input-setup'.
Suitable for use in the :set field of `defcustom'."
  (set-default sym val)
  (lean4-input-setup))

;; Set up the input method.

(cl-eval-when (load eval)
  (lean4-input-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative details

(provide 'lean4-input)
;;; lean4-input.el ends here

(defun lean4-input-export-translations ()
  "Export the current translations in a javascript format.
Print (input, output) pairs in Javascript format to the buffer
*lean4-translations*.  The output can be copy-pasted to
leanprover.github.io/tutorial/js/input-method.js"
  (interactive)
  (with-current-buffer
      (get-buffer-create "*lean4-translations*")
    (let ((exclude-list '("\\newline")))
      (insert "var corrections = {")
      (--each
          (--filter (not (member (car it) exclude-list))
                    (lean4-input-get-translations "Lean"))
        (let* ((input (substring (car it) 1))
               (outputs (cdr it)))
          (insert (format "%s:\"" (prin1-to-string input)))
          (cond ((vectorp outputs)
                 (insert (elt outputs 0)))
                (t (insert-char outputs)))
          (insert (format "\",\n"))))
      (insert "};"))))

(defun lean4-input-export-translations-to-stdout ()
  "Print current translations to stdout."
  (lean4-input-export-translations)
  (with-current-buffer "*lean4-translations*"
    (princ (buffer-string))))
