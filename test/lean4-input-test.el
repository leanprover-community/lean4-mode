;;; lean4-input-test.el --- Tests for lean4-input  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the Lean input method: the combinators used to reshape
;; inherited translation tables, and the translations themselves.

;;; Code:

(require 'lean4-test-helper)
(require 'lean4-input)

;;; Utility functions.

(ert-deftest lean4-test-input-concat-map ()
  "`lean4-input-concat-map' maps and appends in one pass."
  (should-not (lean4-input-concat-map #'list nil))
  (should (equal '(1 1 2 2)
                 (lean4-input-concat-map (lambda (x) (list x x)) '(1 2))))
  ;; A function returning nil drops the element.
  (should (equal '(2) (lean4-input-concat-map
                       (lambda (x) (and (cl-evenp x) (list x)))
                       '(1 2 3)))))

(ert-deftest lean4-test-input-to-string-list ()
  "The string is split per character, with whitespace dropped."
  (should (equal '("a" "b") (lean4-input-to-string-list "a b")))
  (should (equal '("a" "b") (lean4-input-to-string-list "a\nb")))
  (should-not (lean4-input-to-string-list " \n")))

(ert-deftest lean4-test-input-character-range ()
  "`lean4-input-character-range' is inclusive at both ends."
  (should (equal "abc" (lean4-input-character-range ?a ?c)))
  (should (equal "a" (lean4-input-character-range ?a ?a))))

;;; Translation-pair combinators.
;;
;; Each combinator takes one (KEY . TRANSLATION) pair and returns a
;; list of pairs, so that it can drop or multiply the pair it is given.

(ert-deftest lean4-test-input-nonempty ()
  "Pairs with an empty key sequence are dropped."
  (should (equal '(("a" . ?x)) (funcall (lean4-input-nonempty) '("a" . ?x))))
  (should-not (funcall (lean4-input-nonempty) '("" . ?x))))

(ert-deftest lean4-test-input-prepend ()
  "`lean4-input-prepend' extends the key sequence on the left."
  (should (equal '(("_a" . ?x))
                 (funcall (lean4-input-prepend "_") '("a" . ?x)))))

(ert-deftest lean4-test-input-prefix-and-suffix ()
  "The prefix and suffix combinators filter without rewriting."
  (should (equal '(("ab" . ?x)) (funcall (lean4-input-prefix "a") '("ab" . ?x))))
  (should-not (funcall (lean4-input-prefix "b") '("ab" . ?x)))
  (should (equal '(("ab" . ?x)) (funcall (lean4-input-suffix "b") '("ab" . ?x))))
  (should-not (funcall (lean4-input-suffix "a") '("ab" . ?x))))

(ert-deftest lean4-test-input-drop ()
  "`lean4-input-drop' removes the listed key sequences."
  (should-not (funcall (lean4-input-drop '("a" "b")) '("a" . ?x)))
  (should (equal '(("c" . ?x)) (funcall (lean4-input-drop '("a" "b")) '("c" . ?x)))))

(ert-deftest lean4-test-input-drop-beginning-and-end ()
  "The drop-beginning and drop-end combinators trim the key sequence."
  (should (equal '(("cd" . ?x))
                 (funcall (lean4-input-drop-beginning 2) '("abcd" . ?x))))
  (should (equal '(("ab" . ?x))
                 (funcall (lean4-input-drop-end 2) '("abcd" . ?x)))))

(ert-deftest lean4-test-input-drop-prefix-and-suffix ()
  "These filter on an affix and strip it from what survives."
  (should (equal '(("b" . ?x))
                 (funcall (lean4-input-drop-prefix "a") '("ab" . ?x))))
  (should-not (funcall (lean4-input-drop-prefix "z") '("ab" . ?x)))
  (should (equal '(("a" . ?x))
                 (funcall (lean4-input-drop-suffix "b") '("ab" . ?x))))
  (should-not (funcall (lean4-input-drop-suffix "z") '("ab" . ?x))))

(ert-deftest lean4-test-input-compose-and-or ()
  "`lean4-input-compose' chains combinators; `lean4-input-or' unions them."
  (let ((both (lean4-input-or (lean4-input-prepend "1")
                              (lean4-input-prepend "2"))))
    (should (equal '(("1a" . ?x) ("2a" . ?x)) (funcall both '("a" . ?x)))))
  (let ((chained (lean4-input-compose (lean4-input-prepend "1")
                                      (lean4-input-prepend "2"))))
    (should (equal '(("12a" . ?x)) (funcall chained '("a" . ?x))))))

;;; The input method itself.

(ert-deftest lean4-test-input-package-exists ()
  "Requiring lean4-input defines a Quail package named \"Lean\"."
  (should (quail-package "Lean")))

(ert-deftest lean4-test-input-translates-abbreviations ()
  "Backslash abbreviations produce the characters Lean users expect.
`quail-lookup-key' returns a list whose first element is the character
the key sequence maps to; further elements are longer continuations."
  (with-temp-buffer
    (activate-input-method "Lean")
    (should (equal ?α (car (quail-lookup-key "\\alpha"))))
    (should (equal ?→ (car (quail-lookup-key "\\to"))))
    (should (equal ?ℕ (car (quail-lookup-key "\\N"))))
    (should (equal ?∉ (car (quail-lookup-key "\\notin"))))
    (should-not (quail-lookup-key "\\nosuchabbreviation"))))

(ert-deftest lean4-test-input-has-translations-from-the-data-file ()
  "The bundled abbreviations.json is loaded, not just the inherited maps.
The file holds a few thousand entries, so a map this large can only
have come from it."
  (should (> (length (lean4-input-get-translations "Lean")) 1000)))

(ert-deftest lean4-test-input-user-translations-are-honoured ()
  "A translation added by the user reaches the input method."
  (unwind-protect
      (progn
        (let ((lean4-input-user-translations '(("lean4test" . ("⊕")))))
          (lean4-input-setup))
        (with-temp-buffer
          (activate-input-method "Lean")
          (should (equal ?⊕ (car (quail-lookup-key "\\lean4test"))))))
    ;; Rebuild the stock method, so that later tests see it unmodified.
    (lean4-input-setup)))

(provide 'lean4-input-test)
;;; lean4-input-test.el ends here
