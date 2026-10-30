;;; lean4-syntax-test.el --- Tests for lean4-syntax  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the syntax table and the font-lock keywords.

;;; Code:

(require 'lean4-test-helper)

;;; Syntax table.

(ert-deftest lean4-test-syntax-line-comment ()
  "A \"--\" comment runs to the end of its line and no further."
  (lean4-test-with-buffer "def a : Nat := 0 -- tail-!- comment\ndef b : Nat := 1\n"
    (should (nth 4 (syntax-ppss)))
    (forward-line 1)
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest lean4-test-syntax-block-comment ()
  "A \"/- -/\" comment spans lines."
  (lean4-test-with-buffer "/- line one\n   line-!- two -/\ndef a : Nat := 0\n"
    (should (nth 4 (syntax-ppss)))
    (goto-char (point-max))
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest lean4-test-syntax-block-comments-nest ()
  "Block comments nest, so an inner \"-/\" does not close the outer one."
  (lean4-test-with-buffer "/- outer /- inner -/ still-!- outer -/\ndef a : Nat := 0\n"
    (should (nth 4 (syntax-ppss)))
    (goto-char (point-max))
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest lean4-test-syntax-guillemets-are-a-comment ()
  "Guillemets delimit a generic comment.
That is how `«escaped identifiers»' get a face of their own, and it is
why the text between them is not parsed as Lean code."
  (lean4-test-with-buffer "def «weird name-!-» : Nat := 0\n"
    (should (nth 4 (syntax-ppss)))
    (should (eq ?< (char-syntax ?«)))
    (should (eq ?> (char-syntax ?»)))))

(ert-deftest lean4-test-syntax-word-constituents ()
  "Identifier characters Lean allows are word constituents.
Greek letters, subscripts, double-struck letters and the trailing
`'!?_' characters all belong inside a Lean identifier, so word motion
has to treat them as part of one."
  (lean4-test-with-buffer ""
    (dolist (char '(?a ?Z ?0 ?α ?ω ?ℕ ?₁ ?ₙ ?' ?_ ?! ??))
      (should (eq ?w (char-syntax char))))))

(ert-deftest lean4-test-syntax-operator-characters ()
  "Operator characters are punctuation, not word constituents."
  (lean4-test-with-buffer ""
    (dolist (char '(?# ?$ ?% ?& ?* ?+ ?< ?= ?> ?@ ?^ ?| ?~ ?:))
      (should (eq ?. (char-syntax char))))))

(ert-deftest lean4-test-syntax-string-delimiter ()
  "Double quotes delimit strings, and backslash is an escape."
  (lean4-test-with-buffer "def s : String := \"hel-!-lo\"\n"
    (should (nth 3 (syntax-ppss)))
    (should (eq ?\" (char-syntax ?\")))
    (should (eq ?/ (char-syntax ?\\)))))

;;; Font lock.

(ert-deftest lean4-test-syntax-font-lock-keywords ()
  "Declaration keywords get the keyword face."
  (lean4-test-with-buffer "theorem t : True := trivial\nstructure S where\n"
    (should (eq 'font-lock-keyword-face (lean4-test-face-at "theorem")))
    (should (eq 'font-lock-keyword-face (lean4-test-face-at "structure")))))

(ert-deftest lean4-test-syntax-font-lock-declaration-names ()
  "The name a declaration introduces gets the function-name face."
  (lean4-test-with-buffer
      (concat "theorem t1 : True := trivial\n"
              "def d1 : Nat := 0\n"
              "class inductive CI : Type\n")
    (should (eq 'font-lock-function-name-face (lean4-test-face-at "t1")))
    (should (eq 'font-lock-function-name-face (lean4-test-face-at "d1")))
    ;; "class inductive" is two keywords; the name is what follows both.
    (should (eq 'font-lock-function-name-face (lean4-test-face-at "CI")))))

(ert-deftest lean4-test-syntax-font-lock-sorry-is-a-warning ()
  "`sorry' is fontified as a warning, since it admits a goal unproved."
  (lean4-test-with-buffer "theorem t : False := by sorry\n"
    (should (eq 'font-lock-warning-face (lean4-test-face-at "sorry")))))

(ert-deftest lean4-test-syntax-font-lock-types ()
  "The sort names get the type face."
  (lean4-test-with-buffer "def d : Prop := True\ndef e : Type := Nat\n"
    (should (eq 'font-lock-type-face (lean4-test-face-at "Prop")))
    (should (eq 'font-lock-type-face (lean4-test-face-at "Type")))))

(ert-deftest lean4-test-syntax-font-lock-attributes ()
  "Attribute lists get the preprocessor face."
  (lean4-test-with-buffer "@[simp] theorem t : True := trivial\n"
    (should (eq 'font-lock-preprocessor-face (lean4-test-face-at "@[simp]")))))

(ert-deftest lean4-test-syntax-font-lock-literals ()
  "Numerals and string literals get their own faces."
  (lean4-test-with-buffer "def n : Nat := 42\ndef s : String := \"hello\"\n"
    (should (eq 'font-lock-constant-face (lean4-test-face-at "42")))
    (should (eq 'font-lock-string-face (lean4-test-face-at "\"hello\"")))))

(ert-deftest lean4-test-syntax-font-lock-set-option ()
  "In `set_option NAME VALUE', the option name is fontified as a constant."
  (lean4-test-with-buffer "set_option maxHeartbeats 400000\n"
    (should (eq 'font-lock-keyword-face (lean4-test-face-at "set_option")))
    (should (eq 'font-lock-constant-face (lean4-test-face-at "maxHeartbeats")))))

(ert-deftest lean4-test-syntax-font-lock-commands ()
  "The `#'-prefixed commands get the keyword face."
  (lean4-test-with-buffer "#check Nat\n#eval 1 + 1\n"
    (should (eq 'font-lock-keyword-face (lean4-test-face-at "#check")))
    (should (eq 'font-lock-keyword-face (lean4-test-face-at "#eval")))))

(provide 'lean4-syntax-test)
;;; lean4-syntax-test.el ends here
