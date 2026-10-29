;;; lean4-info-test.el --- Tests for lean4-info  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the *Lean Goal* buffer.  Goals and diagnostics are built
;; by hand rather than fetched, and lsp-mode's buffer state is stubbed,
;; so the renderer runs with no server attached.

;;; Code:

(require 'lean4-test-helper)

(defun lean4-test-info-diagnostic (message start-line end-line)
  "Build a Lean diagnostic for MESSAGE spanning START-LINE to END-LINE.
Both lines are zero-based, as on the wire.  Lean's diagnostics carry a
`fullRange' covering the whole declaration alongside the `range'
pointing at the offending token; the info view partitions on the
former."
  (lean4-test-object
   `(:range (:start (:line ,start-line :character 0)
             :end (:line ,start-line :character 1))
     :fullRange (:start (:line ,start-line :character 0)
                 :end (:line ,end-line :character 1))
     :severity 1
     :message ,message)))

(defun lean4-test-info-positions (&rest strings)
  "Return the positions of STRINGS in the info buffer, nil when absent."
  (let ((text (lean4-test-info-text)))
    (mapcar (lambda (string) (string-search string text)) strings)))

;;; Diagnostic accessors.

(ert-deftest lean4-test-info-diagnostic-full-range-lines ()
  "The full-range accessors read the declaration's span, not the token's."
  (let ((diagnostic
         (lean4-test-payload
          "{\"range\":{\"start\":{\"line\":3,\"character\":2},
                      \"end\":{\"line\":3,\"character\":7}},
            \"fullRange\":{\"start\":{\"line\":3,\"character\":0},
                          \"end\":{\"line\":5,\"character\":9}},
            \"severity\":1,
            \"message\":\"declaration uses 'sorry'\"}")))
    (should (equal 3 (lean4-diagnostic-full-start-line diagnostic)))
    (should (equal 5 (lean4-diagnostic-full-end-line diagnostic)))))

;;; Rendering.

(ert-deftest lean4-test-info-renders-goals ()
  "Open goals are listed, one section each, under a \"Goals\" heading.
Each goal is its own `magit-section', which is why magit annotates the
heading with their number in place of its trailing colon."
  (lean4-test-with-info-buffer
    (lean4-test-with-buffer "theorem t (a b : Nat) : a + b = b + a := by\n  sorry\n"
      (setq lean4-goals ["a b : Nat\n⊢ a + b = b + a"
                         "a b : Nat\n⊢ b + a = a + b"])
      (lean4-test-with-lsp 1 nil
        (lean4-info-buffer-render)))
    (let ((text (lean4-test-info-text)))
      (should (string-search "Goals (2)" text))
      (should (string-search "⊢ a + b = b + a" text))
      (should (string-search "⊢ b + a = a + b" text)))))

(ert-deftest lean4-test-info-renders-no-goals ()
  "An empty goal list is reported as the proof being complete."
  (lean4-test-with-info-buffer
    (lean4-test-with-buffer "theorem t : True := by trivial\n"
      (setq lean4-goals [])
      (lean4-test-with-lsp 0 nil
        (lean4-info-buffer-render)))
    (should (string-search "goals accomplished" (lean4-test-info-text)))))

(ert-deftest lean4-test-info-renders-term-goal ()
  "An expected type is shown under its own heading."
  (lean4-test-with-info-buffer
    (lean4-test-with-buffer "def d : Nat := 0\n"
      (setq lean4-term-goal "⊢ Nat")
      (lean4-test-with-lsp 0 nil
        (lean4-info-buffer-render)))
    (let ((text (lean4-test-info-text)))
      (should (string-search "Expected type:" text))
      (should (string-search "⊢ Nat" text)))))

(ert-deftest lean4-test-info-omits-empty-sections ()
  "Headings for goals and messages are left out when there is nothing to show."
  (lean4-test-with-info-buffer
    (lean4-test-with-buffer "def d : Nat := 0\n"
      (lean4-test-with-lsp 0 nil
        (lean4-info-buffer-render)))
    (let ((text (lean4-test-info-text)))
      (should-not (string-search "Goals:" text))
      (should-not (string-search "Expected type:" text))
      (should-not (string-search "Messages" text)))))

(ert-deftest lean4-test-info-partitions-diagnostics-around-point ()
  "Diagnostics are split into those at, below and above the cursor line.
The split uses the diagnostic's full range: one covering the cursor's
line counts as \"here\" even when the token it points at is elsewhere."
  (lean4-test-with-info-buffer
    (let ((diagnostics (list (lean4-test-info-diagnostic "msg-above" 0 2)
                             (lean4-test-info-diagnostic "msg-here" 4 6)
                             (lean4-test-info-diagnostic "msg-below" 8 9))))
      (lean4-test-with-buffer "def a : Nat := 0\n"
        (lean4-test-with-lsp 5 diagnostics
          (lean4-info-buffer-render))))
    (pcase-let ((`(,here-heading ,here
                   ,below-heading ,below
                   ,above-heading ,above)
                 (lean4-test-info-positions
                  "Messages here:" "msg-here"
                  "Messages below:" "msg-below"
                  "Messages above:" "msg-above")))
      ;; Every message lands in its own section, and the sections appear
      ;; in the order here, below, above.
      (should (and here-heading here below-heading below above-heading above))
      (should (< here-heading here below-heading below above-heading above)))))

(ert-deftest lean4-test-info-render-creates-the-buffer ()
  "Rendering works even before the info buffer has been displayed.
The visibility check lives in `lean4-info-buffer-redisplay'; the
renderer itself is callable on its own."
  (unwind-protect
      (progn
        (when (get-buffer lean4-info-buffer-name)
          (kill-buffer lean4-info-buffer-name))
        (let ((lean4-goals ["⊢ True"])
              (lean4-term-goal nil))
          (lean4-test-with-buffer "def d : Nat := 0\n"
            (lean4-test-with-lsp 0 nil
              (lean4-info-buffer-render))))
        (should (get-buffer lean4-info-buffer-name))
        (should (string-search "⊢ True" (lean4-test-info-text))))
    (when (get-buffer lean4-info-buffer-name)
      (kill-buffer lean4-info-buffer-name))))

(ert-deftest lean4-test-info-redisplay-needs-a-visible-buffer ()
  "`lean4-info-buffer-redisplay' does nothing while the view is hidden."
  (unwind-protect
      (save-window-excursion
        (lean4-ensure-info-buffer lean4-info-buffer-name)
        ;; Leave a single window, showing something other than the view.
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create " *not-the-info-buffer*"))
        (should-not (get-buffer-window lean4-info-buffer-name t))
        (let ((lean4-goals ["⊢ True"]))
          (lean4-test-with-buffer "def d : Nat := 0\n"
            (lean4-test-with-lsp 0 nil
              (lean4-info-buffer-redisplay))))
        (should (equal "" (lean4-test-info-text))))
    (when (get-buffer lean4-info-buffer-name)
      (kill-buffer lean4-info-buffer-name))))

;;; Inaccessible names.

(ert-deftest lean4-test-info-dims-inaccessible-names ()
  "Lean's dagger marks a name the user cannot refer to.
The dagger is dropped and the name is dimmed instead, so that goals
stay readable."
  (with-temp-buffer
    (let ((lean4-highlight-inaccessible-names t))
      (lean4-info--insert-highlight-inaccessible-names "a✝ : Nat"))
    (should (equal "a : Nat" (buffer-substring-no-properties
                              (point-min) (point-max))))
    (should (eq 'font-lock-comment-face
                (get-text-property (point-min) 'font-lock-face)))))

(ert-deftest lean4-test-info-inaccessible-names-is-opt-out ()
  "With the option off, the dagger is left in place."
  (with-temp-buffer
    (let ((lean4-highlight-inaccessible-names nil))
      (lean4-info--insert-highlight-inaccessible-names "a✝ : Nat"))
    (should (equal "a✝ : Nat" (buffer-substring-no-properties
                               (point-min) (point-max))))))

;;; Error buttons.

(ert-deftest lean4-test-info-error-button-visits-the-location ()
  "Following a message button moves point to the reported line and column."
  (let ((source (generate-new-buffer " *source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "line one\nline two\nline three\n"))
          (save-window-excursion
            (lean4-info--error-button-action (list source 2 5)))
          (with-current-buffer source
            (should (equal 2 (line-number-at-pos)))
            (should (equal 5 (current-column)))))
      (kill-buffer source))))

(ert-deftest lean4-test-info-error-button-tolerates-a-dead-buffer ()
  "A button pointing at a killed buffer is inert rather than an error."
  (let ((source (generate-new-buffer " *source*")))
    (kill-buffer source)
    (should-not (lean4-info--error-button-action (list source 1 0)))))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
