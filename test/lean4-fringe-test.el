;;; lean4-fringe-test.el --- Tests for lean4-fringe  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the file-progress overlays.  No server is involved: the
;; `$/lean/fileProgress' payloads are built by hand, and the workspace
;; the notification handler is called with is a bare struct.

;;; Code:

(require 'lean4-test-helper)

(defun lean4-test-fringe-overlays ()
  "Return the progress overlays in the current buffer."
  (seq-filter (lambda (ov) (eq 'lean4-fringe-face (overlay-get ov 'face)))
              (overlays-in (point-min) (point-max))))

(defun lean4-test-fringe-item (start-line start-char end-line end-char kind)
  "Build one progress item spanning the given range, of the given KIND.
START-LINE, START-CHAR, END-LINE and END-CHAR are zero-based, as on the
wire."
  (lean4-test-object
   `(:range (:start (:line ,start-line :character ,start-char)
             :end (:line ,end-line :character ,end-char))
     :kind ,kind)))

;;; Payload accessors.

(ert-deftest lean4-test-fringe-region ()
  "A progress range becomes a pair of buffer positions."
  (lean4-test-with-buffer "abc\ndef\n"
    ;; Line 0 starts at position 1; line 1 starts after "abc\n", at 5.
    (should (equal '(1 . 5) (lean4-fringe-region
                             (lean4-test-fringe-item 0 0 1 0 1))))
    (should (equal '(2 . 4) (lean4-fringe-region
                             (lean4-test-fringe-item 0 1 0 3 1))))))

(ert-deftest lean4-test-fringe-face-depends-on-kind ()
  "Kind 1 means \"being processed\"; anything else is a fatal error."
  (should (eq 'lean4-fringe-fringe-processing-face
              (lean4-fringe-fringe-face (lean4-test-object '(:kind 1)))))
  (should (eq 'lean4-fringe-fringe-fatal-error-face
              (lean4-fringe-fringe-face (lean4-test-object '(:kind 2))))))

;;; Overlay drawing.

(ert-deftest lean4-test-fringe-draws-one-overlay-per-range ()
  "Each progress range gets an overlay carrying the matching fringe face."
  (lean4-test-with-buffer "abc\ndef\nghi\n"
    (setq lean4-fringe-data (vector (lean4-test-fringe-item 0 0 0 3 1)
                                    (lean4-test-fringe-item 2 0 2 3 2)))
    (let ((lean4-show-file-progress t))
      (lean4-fringe-update-progress-overlays))
    (let ((overlays (lean4-test-fringe-overlays)))
      (should (equal 2 (length overlays)))
      (should (equal '(1 4 9 12)
                     (sort (append (mapcar #'overlay-start overlays)
                                   (mapcar #'overlay-end overlays))
                           #'<))))))

(ert-deftest lean4-test-fringe-redraw-replaces-stale-overlays ()
  "Redrawing clears the overlays from the previous notification."
  (lean4-test-with-buffer "abc\ndef\nghi\n"
    (let ((lean4-show-file-progress t))
      (setq lean4-fringe-data (vector (lean4-test-fringe-item 0 0 0 3 1)))
      (lean4-fringe-update-progress-overlays)
      (should (equal 1 (length (lean4-test-fringe-overlays))))
      ;; Lean reports an empty list once a file is fully elaborated.
      (setq lean4-fringe-data nil)
      (lean4-fringe-update-progress-overlays)
      (should-not (lean4-test-fringe-overlays)))))

(ert-deftest lean4-test-fringe-respects-the-user-option ()
  "No overlays are drawn when file progress is switched off."
  (lean4-test-with-buffer "abc\ndef\n"
    (setq lean4-fringe-data (vector (lean4-test-fringe-item 0 0 0 3 1)))
    (let ((lean4-show-file-progress nil))
      (lean4-fringe-update-progress-overlays))
    (should-not (lean4-test-fringe-overlays))))

;;; Notification handling.

(ert-deftest lean4-test-fringe-update-targets-the-right-buffer ()
  "Progress is recorded only in the buffer the notification is about.
A workspace holds every buffer the server manages, so the handler has
to match on the URI rather than assume there is just one."
  (lean4-test-with-directory root '("Mod.lean" "Other.lean")
    (let ((target (generate-new-buffer " *target*"))
          (other (generate-new-buffer " *other*")))
      (unwind-protect
          (let (uri workspace params)
            (with-current-buffer target
              (setq buffer-file-name (expand-file-name "Mod.lean" root))
              (insert "abc\ndef\n")
              (setq uri (lsp--buffer-uri)))
            (with-current-buffer other
              (setq buffer-file-name (expand-file-name "Other.lean" root))
              (insert "abc\ndef\n"))
            (setq workspace (make-lsp--workspace :buffers (list target other))
                  params (lean4-test-object
                          `(:textDocument (:uri ,uri :version 1)
                            :processing [(:range (:start (:line 0 :character 0)
                                                  :end (:line 0 :character 3))
                                          :kind 1)])))
            (lean4-fringe-update workspace params)
            (should (buffer-local-value 'lean4-fringe-data target))
            (should-not (buffer-local-value 'lean4-fringe-data other)))
        (dolist (buffer (list target other))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest lean4-test-fringe-update-schedules-a-redraw ()
  "The overlays are redrawn from a timer, after the notification burst.
Lean sends progress in bursts while elaborating, so drawing is deferred
by `lean4-fringe-delay-sec' and coalesced."
  (lean4-test-with-directory root '("Mod.lean")
    (let ((buffer (generate-new-buffer " *target*")))
      (unwind-protect
          (with-current-buffer buffer
            (setq buffer-file-name (expand-file-name "Mod.lean" root))
            (insert "abc\ndef\n")
            ;; The redraw reads `lean4-show-file-progress' from inside
            ;; the timer, by which point any binding made here would be
            ;; long gone, so this test relies on the default value.
            (should lean4-show-file-progress)
            (let* ((lean4-fringe-delay-sec 0.01)
                   (workspace (make-lsp--workspace :buffers (list buffer)))
                   (params (lean4-test-object
                            `(:textDocument (:uri ,(lsp--buffer-uri) :version 1)
                              :processing [(:range
                                            (:start (:line 0 :character 0)
                                             :end (:line 0 :character 3))
                                            :kind 1)]))))
              ;; Nothing is drawn synchronously.
              (lean4-fringe-update workspace params)
              (should-not (lean4-test-fringe-overlays))
              (should (lean4-test-wait
                       (lambda () (lean4-test-fringe-overlays))))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'lean4-fringe-test)
;;; lean4-fringe-test.el ends here
