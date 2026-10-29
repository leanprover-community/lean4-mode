;;; lean4-live-protocol-test.el --- Live protocol tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests which drive a real `lean --server'.  Run them with
;; "make test-live"; they are not part of "make check", because they
;; need a Lean toolchain.
;;
;; Their job is not to re-test lean4-mode's logic -- the hermetic suite
;; does that -- but to catch the fixtures it uses drifting away from
;; what Lean actually sends.  So every assertion here feeds real server
;; output into the same accessors the package uses, and is structural
;; rather than textual: Lean's pretty-printer changes between releases,
;; while the shape of the protocol does not.
;;
;; The transport is raw JSON-RPC rather than lsp-mode, which keeps
;; these tests independent of lsp-mode's startup machinery.  Bytes are
;; read with `no-conversion' coding and frames sliced by byte count,
;; because Content-Length counts bytes and Lean's goals are full of
;; multibyte characters.

;;; Code:

(require 'lean4-test-helper)

(defvar lean4-live-process nil
  "The running Lean server, while a session is open.")

(defvar lean4-live-path nil
  "Absolute name of the Lean file the session is about.")

(defvar lean4-live-uri nil
  "URI of `lean4-live-path'.")

(defvar lean4-live--pending ""
  "Bytes received from the server which are not yet a complete frame.")

(defvar lean4-live--messages nil
  "Decoded messages received so far, newest first.")

(defun lean4-live-available-p ()
  "Return non-nil when a Lean toolchain is on PATH."
  (and (executable-find "lean") t))

(defun lean4-live--drain ()
  "Move every complete frame out of the pending bytes into the queue."
  (catch 'incomplete
    (while t
      (unless (string-match "Content-Length: \\([0-9]+\\)\r\n\r\n"
                            lean4-live--pending)
        (throw 'incomplete nil))
      (let* ((size (string-to-number (match-string 1 lean4-live--pending)))
             (start (match-end 0))
             (end (+ start size)))
        ;; `length' is a byte count here: the process coding system is
        ;; `no-conversion', so the accumulator is unibyte.
        (when (< (length lean4-live--pending) end)
          (throw 'incomplete nil))
        (push (decode-coding-string
               (substring lean4-live--pending start end) 'utf-8)
              lean4-live--messages)
        (setq lean4-live--pending (substring lean4-live--pending end))))))

(defun lean4-live--filter (_process chunk)
  "Accumulate CHUNK and extract any complete frames from it."
  (setq lean4-live--pending (concat lean4-live--pending chunk))
  (lean4-live--drain))

(defun lean4-live-send (message)
  "Frame MESSAGE, a plist, and send it to the server."
  (let ((json (json-serialize message)))
    (process-send-string
     lean4-live-process
     (format "Content-Length: %d\r\n\r\n%s" (string-bytes json) json))))

(defun lean4-live-wait (regexp &optional timeout)
  "Wait for a received message matching REGEXP and return it parsed.
Return nil if nothing matched within TIMEOUT seconds, 60 by default.
Lean has to elaborate the file before it can answer, so be generous."
  (let ((deadline (+ (float-time) (or timeout 60)))
        (found nil))
    (while (and (not found) (< (float-time) deadline))
      (setq found (seq-find (lambda (message) (string-match-p regexp message))
                            lean4-live--messages))
      (unless found
        (accept-process-output lean4-live-process 0.05)))
    (and found (lean4-test-payload found))))

(defun lean4-live-matching (regexp)
  "Return every message received so far which matches REGEXP, parsed."
  (mapcar #'lean4-test-payload
          (seq-filter (lambda (message) (string-match-p regexp message))
                      (reverse lean4-live--messages))))

(defmacro lean4-live-with-session (content &rest body)
  "Open a Lean server on a temporary file holding CONTENT, then run BODY.
The file has been elaborated up to the point where Lean has published
diagnostics for it before BODY starts, so that BODY does not have to
race the server.  The server is killed afterwards."
  (declare (indent 1) (debug (form body)))
  `(lean4-test-with-directory root '("Mod.lean")
     (let* ((lean4-live--pending "")
            (lean4-live--messages nil)
            (lean4-live-path (expand-file-name "Mod.lean" root))
            (lean4-live-uri (lsp--path-to-uri lean4-live-path))
            (lean4-live-process nil))
       (unwind-protect
           (progn
             (write-region ,content nil lean4-live-path nil 'silent)
             (setq lean4-live-process
                   (make-process
                    :name "lean4-live"
                    :command (list (executable-find "lean") "--server")
                    :connection-type 'pipe
                    :coding 'no-conversion
                    :noquery t
                    :filter #'lean4-live--filter))
             (lean4-live-send
              `(:jsonrpc "2.0" :id 1 :method "initialize"
                :params (:processId ,(emacs-pid)
                         :rootUri ,(lsp--path-to-uri (directory-file-name root))
                         :capabilities (:textDocument (:hover (:dynamicRegistration :false))))))
             (should (lean4-live-wait "\"id\":1"))
             (lean4-live-send '(:jsonrpc "2.0" :method "initialized" :params ()))
             (lean4-live-send
              `(:jsonrpc "2.0" :method "textDocument/didOpen"
                :params (:textDocument (:uri ,lean4-live-uri
                                        :languageId "lean"
                                        :version 1
                                        :text ,,content))))
             (should (lean4-live-wait "publishDiagnostics"))
             ,@body)
         (when (process-live-p lean4-live-process)
           (delete-process lean4-live-process))))))

(defconst lean4-live-fixture
  "theorem t (a b : Nat) : a + b = b + a := by\n  sorry\ndef d : Nat := 0\n"
  "A Lean file with an open goal, a warning and a term to ask about.")

;;; File progress.

(ert-deftest lean4-live-file-progress-fits-our-interface ()
  "Real progress notifications feed the fringe accessors unchanged.
By the time diagnostics have been published the progress sequence is
over, so the notifications collected here are the complete set."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    (let ((notifications (lean4-live-matching "fileProgress"))
          (items-seen 0))
      (should notifications)
      (dolist (notification notifications)
        (let* ((params (lsp-get notification :params))
               (document (lsp-get params :textDocument))
               (processing (lsp-get params :processing)))
          (should (equal lean4-live-uri (lsp-get document :uri)))
          (should (vectorp processing))
          (lean4-test-with-buffer lean4-live-fixture
            (seq-doseq (item processing)
              (setq items-seen (1+ items-seen))
              (let ((region (lean4-fringe-region item)))
                (should (consp region))
                (should (integerp (car region)))
                (should (integerp (cdr region))))
              (should (facep (lean4-fringe-fringe-face item)))))))
      ;; Lean reports at least one range as being processed.
      (should (> items-seen 0)))))

;;; Diagnostics.

(ert-deftest lean4-live-diagnostics-carry-a-full-range ()
  "Lean's diagnostics really do carry the `fullRange' we depend on.
It is an extension to the standard Diagnostic, and the info view
partitions messages around point with it, so losing it would quietly
break the whole view."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    ;; Lean publishes an empty set of diagnostics as soon as it starts on
    ;; the file, and the real ones only once it has finished, so wait for
    ;; a payload that actually carries one.
    (let* ((notification (lean4-live-wait "\"diagnostics\":\\[{"))
           (diagnostics (lsp-get (lsp-get notification :params) :diagnostics)))
      ;; The `sorry' in the fixture guarantees at least one message.
      (should (> (length diagnostics) 0))
      (seq-doseq (diagnostic diagnostics)
        (should (integerp (lean4-diagnostic-full-start-line diagnostic)))
        (should (integerp (lean4-diagnostic-full-end-line diagnostic)))
        (should (stringp (lsp-get diagnostic :message)))))))

(ert-deftest lean4-live-diagnostics-render-in-the-info-view ()
  "Real diagnostics can be rendered by the info view."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    (let* ((notification (lean4-live-wait "\"diagnostics\":\\[{"))
           (diagnostics (append
                         (lsp-get (lsp-get notification :params) :diagnostics)
                         nil)))
      (lean4-test-with-info-buffer
        (lean4-test-with-buffer lean4-live-fixture
          (lean4-test-with-lsp 0 diagnostics
            (lean4-info-buffer-render)))
        (should (string-match-p "Messages" (lean4-test-info-text)))))))

;;; Goals.

(ert-deftest lean4-live-plain-goal-fits-our-interface ()
  "A real `$/lean/plainGoal' response destructures as the callback expects."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    ;; Line 1, character 2: the `sorry'.
    (lean4-live-send
     `(:jsonrpc "2.0" :id 2 :method "$/lean/plainGoal"
       :params (:textDocument (:uri ,lean4-live-uri)
                :position (:line 1 :character 2))))
    (let* ((response (lean4-live-wait "\"id\":2"))
           (result (lsp-get response :result))
           (goals (-let (((&lean:PlainGoal? :goals) result)) goals)))
      (should (vectorp goals))
      (should (> (length goals) 0))
      (should (seq-every-p #'stringp goals))
      ;; Structural, not textual: every goal states a target.
      (should (seq-every-p (lambda (goal) (string-search "⊢" goal)) goals))
      ;; And the view can render them.
      (lean4-test-with-info-buffer
        (lean4-test-with-buffer lean4-live-fixture
          (setq lean4-goals goals)
          (lean4-test-with-lsp 1 nil
            (lean4-info-buffer-render)))
        (should (string-match-p "Goals" (lean4-test-info-text)))))))

(ert-deftest lean4-live-plain-term-goal-fits-our-interface ()
  "A real `$/lean/plainTermGoal' response destructures as expected."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    ;; Line 2, character 15: the `0' of "def d : Nat := 0".
    (lean4-live-send
     `(:jsonrpc "2.0" :id 3 :method "$/lean/plainTermGoal"
       :params (:textDocument (:uri ,lean4-live-uri)
                :position (:line 2 :character 15))))
    (let* ((response (lean4-live-wait "\"id\":3"))
           (result (lsp-get response :result))
           (goal (-let (((&lean:PlainTermGoal? :goal) result)) goal)))
      (should (stringp goal))
      (should (string-search "⊢" goal)))))

(ert-deftest lean4-live-plain-goal-outside-a-proof ()
  "Asking for tactic goals where there are none is answered, not refused.
The info view sends this request from `post-command-hook' wherever
point happens to be, so the empty answer has to be well formed."
  (skip-unless (lean4-live-available-p))
  (lean4-live-with-session lean4-live-fixture
    ;; Line 2 is a plain definition, with no tactic block.
    (lean4-live-send
     `(:jsonrpc "2.0" :id 4 :method "$/lean/plainGoal"
       :params (:textDocument (:uri ,lean4-live-uri)
                :position (:line 2 :character 0))))
    (let ((response (lean4-live-wait "\"id\":4")))
      (should response)
      ;; Either no result at all, or a result with no goals; never an error.
      (should-not (lsp-get response :error))
      (let ((goals (-let (((&lean:PlainGoal? :goals) (lsp-get response :result)))
                     goals)))
        (should (or (null goals) (equal 0 (length goals))))))))

(provide 'lean4-live-protocol-test)
;;; lean4-live-protocol-test.el ends here
