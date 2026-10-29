;;; lean4-eri-test.el --- Tests for lean4-eri  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the indentation engine: its list helpers, the points it
;; derives from the lines above, and the cycle TAB walks through.

;;; Code:

(require 'lean4-test-helper)

;;; List helpers.

(ert-deftest lean4-test-eri-maximum ()
  "`lean4-eri-maximum' is nil on the empty list, the maximum otherwise."
  (should-not (lean4-eri-maximum nil))
  (should (equal 7 (lean4-eri-maximum '(7))))
  (should (equal 7 (lean4-eri-maximum '(3 7 5))))
  (should (equal -1 (lean4-eri-maximum '(-3 -1 -5)))))

(ert-deftest lean4-test-eri-take ()
  "`lean4-eri-take' returns a prefix, and the whole list if asked for more."
  (should-not (lean4-eri-take 0 '(1 2 3)))
  (should (equal '(1 2) (lean4-eri-take 2 '(1 2 3))))
  (should (equal '(1 2 3) (lean4-eri-take 3 '(1 2 3))))
  (should (equal '(1 2 3) (lean4-eri-take 9 '(1 2 3))))
  (should-not (lean4-eri-take 2 nil)))

(ert-deftest lean4-test-eri-split ()
  "`lean4-eri-split' cuts a sorted list at the given value.
Everything in the first half is <= the pivot and everything in the
second half is > it."
  (should (equal '((1 2) . (3 4)) (lean4-eri-split 2 '(1 2 3 4))))
  ;; The pivot need not occur in the list.
  (should (equal '((1 2) . (5 6)) (lean4-eri-split 3 '(1 2 5 6))))
  ;; Pivot below everything, and above everything.
  (should (equal '(nil . (1 2)) (lean4-eri-split 0 '(1 2))))
  (should (equal '((1 2) . nil) (lean4-eri-split 9 '(1 2))))
  (should (equal '(nil . nil) (lean4-eri-split 1 nil))))

;;; Line predicates.

(ert-deftest lean4-test-eri-current-line-length ()
  "`lean4-eri-current-line-length' excludes the newline."
  (lean4-test-with-buffer "abcd\n-!-x\n"
    (should (equal 1 (lean4-eri-current-line-length)))
    (forward-line -1)
    (should (equal 4 (lean4-eri-current-line-length)))))

(ert-deftest lean4-test-eri-current-line-empty ()
  "A line of nothing but whitespace counts as empty."
  (lean4-test-with-buffer "abcd\n    \n-!-\n"
    (should (lean4-eri-current-line-empty))
    (forward-line -1)
    (should (lean4-eri-current-line-empty))
    (forward-line -1)
    (should-not (lean4-eri-current-line-empty))))

;;; Indentation points.

;; For the fixture
;;
;;   ab cd
;;   <point on the empty line below>
;;
;; the points on the line above are columns 0 and 3, the columns of
;; characters at the start of the line or preceded by whitespace.  To
;; those is added column 2, two steps in from the indentation of the
;; nearest non-empty line above.  The current indentation, 0, is
;; removed, which leaves (2 3).

(ert-deftest lean4-test-eri-calculate-indentation-points ()
  "The points come from the columns above, plus one new nesting step."
  (lean4-test-with-buffer "ab cd\n-!-"
    (should (equal '(2 3) (lean4-eri-calculate-indentation-points nil)))
    (should (equal '(3 2) (lean4-eri-calculate-indentation-points t)))))

(ert-deftest lean4-test-eri-calculate-indentation-points-on-line ()
  "Only columns left of MAX are reported, in ascending order."
  (lean4-test-with-buffer "f x y = g 3-!-\n"
    (should (equal '(0 2 4 6 8 10)
                   (lean4-eri-calculate-indentation-points-on-line nil)))
    (should (equal '(0 2 4) (lean4-eri-calculate-indentation-points-on-line 5)))))

(ert-deftest lean4-test-eri-indent-cycles-and-wraps ()
  "Repeated indentation walks through the points, then starts over."
  (lean4-test-with-buffer "ab cd\n-!-"
    (lean4-eri-indent)
    (should (equal 2 (current-indentation)))
    (lean4-eri-indent)
    (should (equal 3 (current-indentation)))
    ;; Past the last point, the cycle wraps round to the first column.
    (lean4-eri-indent)
    (should (equal 0 (current-indentation)))
    (lean4-eri-indent)
    (should (equal 2 (current-indentation)))))

(ert-deftest lean4-test-eri-indent-reverse-cycles-backwards ()
  "`lean4-eri-indent-reverse' walks the same points the other way."
  (lean4-test-with-buffer "ab cd\n-!-"
    (lean4-eri-indent-reverse)
    (should (equal 3 (current-indentation)))
    (lean4-eri-indent-reverse)
    (should (equal 2 (current-indentation)))))

(provide 'lean4-eri-test)
;;; lean4-eri-test.el ends here
