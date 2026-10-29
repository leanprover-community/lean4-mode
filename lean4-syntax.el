;;; lean4-syntax.el --- Lean4-Mode Syntax Definitions  -*- lexical-binding: t; -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.

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

;; This library defines syntaxes for `lean4-mode'.

;;; Code:

(require 'dash)
(require 'rx)

(defconst lean4-keywords1
  '("import" "prelude" "protected" "private" "noncomputable"
    "unsafe" "partial" "renaming" "hiding" "begin" "constant"
    "variable" "variables" "theorem" "example" "abbrev"
    "open" "export" "axiom" "inductive" "with"
    "structure" "universe" "universes" "hide"
    "precedence" "match_syntax" "match" "nomatch" "infix" "infixl" "infixr" "notation" "postfix" "prefix" "instance"
    "end" "this" "using" "using_well_founded" "namespace" "section"
    "attribute" "local" "set_option" "extends" "include" "class"
    "attributes" "raw" "have" "show" "suffices" "by" "in" "at" "do" "let" "for" "unless" "break" "continue"
    "try" "catch" "finally" "where" "rec" "mut" "forall" "fun"
    "exists" "if" "then" "else" "from" "init_quot" "return"
    "mutual" "def" "run_cmd" "declare_syntax_cat" "syntax" "macro_rules" "macro" "scoped" "elab"
    "initialize" "builtin_initialize" "register_builtin_option" "induction" "cases" "generalizing" "unif_hint" "deriving")
  "Lean keywords ending with `word' (not symbol).")
(defconst lean4-keywords1-regexp
  (eval `(rx word-start (or ,@lean4-keywords1) word-end)))
(defconst lean4-constants
  '("#" "@" "!" "$" "->" "∼" "↔" "/" "==" "=" ":=" "<->" "/\\" "\\/" "∧" "∨"
    "≠" "<" ">" "≤" "≥" "¬" "<=" ">=" "⁻¹" "⬝" "▸" "+" "*" "-" "/" "λ"
    "→" "∃" "∀" "∘" "×" "Σ" "Π" "~" "||" "&&" "≃" "≡" "≅"
    "ℕ" "ℤ" "ℚ" "ℝ" "ℂ" "𝔸"
    "⬝e" "⬝i" "⬝o" "⬝op" "⬝po" "⬝h" "⬝v" "⬝hp" "⬝vp" "⬝ph" "⬝pv" "⬝r" "◾" "◾o"
    "∘n" "∘f" "∘fi" "∘nf" "∘fn" "∘n1f" "∘1nf" "∘f1n" "∘fn1"
    "^c" "≃c" "≅c" "×c" "×f" "×n" "+c" "+f" "+n" "ℕ₋₂")
  "Lean constants.")
(defconst lean4-constants-regexp (regexp-opt lean4-constants))
(defconst lean4-numerals-regexp
  (eval `(rx word-start
             (one-or-more digit) (optional (and "." (zero-or-more digit)))
             word-end)))

(defconst lean4-warnings '("sorry") "Lean warnings.")
(defconst lean4-warnings-regexp
  (eval `(rx word-start (or ,@lean4-warnings) word-end)))
(defconst lean4-debugging '("unreachable!" "panic!" "assert!" "dbg_trace") "Lean debugging.")
(defconst lean4-debugging-regexp
  (eval `(rx word-start (or ,@lean4-debugging) word-end)))


(defconst lean4-syntax-table
  (let ((st (make-syntax-table)))
    ;; Matching parens
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; comment
    (modify-syntax-entry ?/ ". 14nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?« "<" st)
    (modify-syntax-entry ?» ">" st)

    ;; Word constituent
    (--each (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                  ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                  ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
                  ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)
      (modify-syntax-entry it "w" st))
    (--each (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
      (modify-syntax-entry it "w" st))
    (--each (list ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;;?λ
                  ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ
                  ?φ ?χ ?ψ ?ω)
      (modify-syntax-entry it "w" st))
    (--each (list ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ
                  ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ
                  ?Ϥ ?ϥ ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ
                  ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ ?ϻ)
      (modify-syntax-entry it "w" st))
    (--each (list ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ
                  ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ
                  ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ
                  ?ἦ ?ἧ ?Ἠ ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ
                  ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ ?Ἶ ?Ἷ
                  ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ
                  ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ
                  ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ
                  ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ
                  ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
                  ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ
                  ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ ?ᾒ ?ᾓ ?ᾔ
                  ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ
                  ?ᾣ ?ᾤ ?ᾥ ?ᾦ ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ
                  ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά ?ᾼ ?᾽
                  ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ
                  ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ
                  ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
                  ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ
                  ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ ?Ώ ?ῼ ?´ ?῾)
      (modify-syntax-entry it "w" st))
    (--each (list ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ
                  ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔ ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ
                  ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩ ?K ?Å ?ℬ
                  ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻
                  ?ℼ ?ℽ ?ℾ ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊
                  ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏)
      (modify-syntax-entry it "w" st))
    (--each (list ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀
                  ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ ?ₛ ?ₜ
                  ?' ?_ ?! ??)
      (modify-syntax-entry it "w" st))

    ;; Lean operator chars
    (--each (string-to-list "#$%&*+<=>@^|~:")
      (modify-syntax-entry it "." st))

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst lean4-font-lock-defaults
  `((;; attributes
     (,(rx word-start "attribute" word-end (zero-or-more whitespace) (group (one-or-more "[" (zero-or-more (not (in "]"))) "]" (zero-or-more whitespace))))
      (1 'font-lock-preprocessor-face))
     (,(rx (group "@[" (zero-or-more (not (in "]"))) "]"))
      (1 'font-lock-preprocessor-face))
     (,(rx (group "#" (or "eval" "print" "reduce" "help" "check" "lang" "check_failure" "synth")))
      (1 'font-lock-keyword-face))
     ;; mutual definitions "names"
     (,(rx word-start
           "mutual"
           word-end
           (zero-or-more whitespace)
           word-start
           (or "inductive" "definition" "def")
           word-end
           (group (zero-or-more (not (in " \t\n\r{([,"))) (zero-or-more (zero-or-more whitespace) "," (zero-or-more whitespace) (not (in " \t\n\r{([,")))))
      (1 'font-lock-function-name-face))
     ;; declarations
     (,(rx word-start
           (group (or "inductive" (group "class" (zero-or-more whitespace) "inductive") "instance" "structure" "class" "theorem" "axiom" "lemma" "definition" "def" "constant"))
           word-end (zero-or-more whitespace)
           (group (zero-or-more "{" (zero-or-more (not (in "}"))) "}" (zero-or-more whitespace)))
           (zero-or-more whitespace)
           (group (zero-or-more (not (in " \t\n\r{([")))))
      (4 'font-lock-function-name-face))
     ;; Constants which have a keyword as subterm
     (,(rx (or "∘if")) . 'font-lock-constant-face)
     ;; Keywords
     ("\\(set_option\\)[ \t]*\\([^ \t\n]*\\)" (2 'font-lock-constant-face))
     (,lean4-keywords1-regexp . 'font-lock-keyword-face)
     (,(rx word-start (group "example") ".") (1 'font-lock-keyword-face))
     (,(rx (or "∎")) . 'font-lock-keyword-face)
     ;; Types
     (,(rx word-start (or "Prop" "Type" "Type*" "Sort" "Sort*") symbol-end) . 'font-lock-type-face)
     (,(rx word-start (group (or "Prop" "Type" "Sort")) ".") (1 'font-lock-type-face))
     ;; String
     ("\"[^\"]*\"" . 'font-lock-string-face)
     ;; Debugging builtins
     (,lean4-debugging-regexp . 'font-lock-warning-face)
     ;; ;; Constants
     (,lean4-constants-regexp . 'font-lock-constant-face)
     (,lean4-numerals-regexp . 'font-lock-constant-face)
     ;; place holder
     (,(rx symbol-start "_" symbol-end) . 'font-lock-preprocessor-face)
     ;; warnings
     (,lean4-warnings-regexp . 'font-lock-warning-face)
     ;; escaped identifiers
     (,(rx (and (group "«") (group (one-or-more (not (in "»")))) (group "»")))
      (1 font-lock-comment-face t)
      (2 nil t)
      (3 font-lock-comment-face t)))))

;; Syntax Highlighting for Lean Info Mode
(defconst lean4-info-font-lock-defaults
  (let ((new-entries
         `(;; Please add more after this:
           (,(rx (group (+ symbol-start (+ (or word (char ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀))) symbol-end (* white))) ":")
            (1 'font-lock-variable-name-face))
           (,(rx white ":" white)
            . 'font-lock-keyword-face)
           (,(rx "⊢" white)
            . 'font-lock-keyword-face)
           (,(rx "[" (group "stale") "]")
            (1 'font-lock-warning-face))
           (,(rx line-start "No Goal" line-end)
            . 'font-lock-constant-face)))
        (inherited-entries (car lean4-font-lock-defaults)))
    `(,(-concat new-entries inherited-entries))))

(provide 'lean4-syntax)
;;; lean4-syntax.el ends here
