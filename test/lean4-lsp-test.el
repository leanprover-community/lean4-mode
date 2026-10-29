;;; lean4-lsp-test.el --- Integration tests against lsp-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Tests for the seams between lean4-mode and lsp-mode: what the
;; registered client declares, that Lean's custom notifications reach
;; our handlers through lsp-mode's own dispatch, and that the requests
;; we issue carry the method and parameters the Lean server expects.
;;
;; No server runs here, and no session is started.  Notifications are
;; delivered by calling `lsp--on-notification' with a workspace built
;; for the test, and outgoing requests are captured at
;; `lsp-request-async'.  Driving a whole lsp-mode session in batch
;; would pin the suite to lsp-mode's startup internals, which is a bad
;; trade for a dependency that is updated weekly; the wire itself is
;; covered by the live test in lean4-live-test.el.
;;
;; One asymmetry to keep in mind: lsp-mode builds outgoing parameters
;; as plists whatever LSP_USE_PLISTS says, because they come from plain
;; `list' calls, while incoming objects follow the representation it was
;; compiled for.  So assertions on what we send use `plist-get', and
;; payloads we receive are built with `lean4-test-payload'.

;;; Code:

(require 'lean4-test-helper)

(defun lean4-test-lsp-client ()
  "Return the lsp-mode client lean4-mode registers."
  (gethash 'lean4-lsp lsp-clients))

(defun lean4-test-lsp-workspace (&rest buffers)
  "Return a workspace for the Lean client, managing BUFFERS."
  (make-lsp--workspace :client (lean4-test-lsp-client) :buffers buffers))

;;; What the client declares.

(ert-deftest lean4-test-lsp-client-is-registered ()
  "lean4-mode registers a client for `lean4-mode' buffers."
  (let ((client (lean4-test-lsp-client)))
    (should client)
    (should (equal 'lean4-lsp (lsp--client-server-id client)))
    (should (memq 'lean4-mode (lsp--client-major-modes client)))))

(ert-deftest lean4-test-lsp-client-handles-file-progress ()
  "The client routes Lean's progress notification to the fringe handler."
  (should (eq 'lean4-fringe-update
              (gethash "$/lean/fileProgress"
                       (lsp--client-notification-handlers
                        (lean4-test-lsp-client))))))

(ert-deftest lean4-test-lsp-client-overrides-the-sorry-face ()
  "Tokens Lean marks as sorry-like are fontified as a warning.
Lean has no standard semantic token type for `sorry', so the client
maps its custom type onto the warning face."
  (should (equal 'font-lock-warning-face
                 (cdr (assoc "leanSorryLike"
                             (plist-get (lsp--client-semantic-tokens-faces-overrides
                                         (lean4-test-lsp-client))
                                        :types))))))

(ert-deftest lean4-test-lsp-language-id ()
  "lsp-mode is told that `lean4-mode' buffers hold the \"lean\" language."
  (should (equal "lean" (cdr (assq 'lean4-mode lsp-language-id-configuration))))
  (lean4-test-with-buffer ""
    (should (equal "lean" (lsp-buffer-language)))))

;;; Incoming notifications, through lsp-mode's dispatch.

(ert-deftest lean4-test-lsp-file-progress-reaches-the-right-buffer ()
  "A progress notification is dispatched into the buffer it names.
This goes through `lsp--on-notification', so it covers the handler
table in the registered client as well as the handler itself."
  (lean4-test-with-directory root '("Mod.lean" "Other.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by trivial\n"
      (let ((target (current-buffer)))
        (lean4-test-with-source-buffer (expand-file-name "Other.lean" root)
            "theorem u : True := by trivial\n"
          (let ((other (current-buffer))
                (uri (buffer-local-value 'buffer-file-name target)))
            (lsp--on-notification
             (lean4-test-lsp-workspace target other)
             (lean4-test-object
              `(:jsonrpc "2.0"
                :method "$/lean/fileProgress"
                :params (:textDocument (:uri ,(lsp--path-to-uri uri) :version 1)
                         :processing [(:range (:start (:line 0 :character 0)
                                               :end (:line 0 :character 7))
                                       :kind 1)]))))
            (should (buffer-local-value 'lean4-fringe-data target))
            (should-not (buffer-local-value 'lean4-fringe-data other))))))))

(ert-deftest lean4-test-lsp-unknown-lean-notification-is-ignored ()
  "An unrecognised \"$/\" notification is not an error.
Lean adds notifications over time, and an older lean4-mode must not
start warning at a newer server."
  (should-not (lsp--on-notification
               (lean4-test-lsp-workspace)
               (lean4-test-object
                '(:jsonrpc "2.0" :method "$/lean/somethingNew" :params nil)))))

;;; Outgoing goal requests.

(defmacro lean4-test-with-captured-requests (var &rest body)
  "Evaluate BODY with `lsp-request-async' captured into VAR.
VAR is bound to a list of (METHOD PARAMS CALLBACK ARGS), oldest first,
which grows as BODY issues requests.  Redisplay requests are counted in
`lean4-test-renders' instead of being run, so that BODY leaves no timer
behind."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var nil)
         (lean4-test-renders 0))
     (cl-letf (((symbol-function 'lsp-request-async)
                (lambda (method params callback &rest args)
                  (setq ,var (append ,var (list (list method params
                                                      callback args))))))
               ((symbol-function 'lean4-info-buffer-redisplay-debounced)
                (lambda (&rest _)
                  (setq lean4-test-renders (1+ lean4-test-renders)))))
       ,@body)))

(ert-deftest lean4-test-lsp-refresh-asks-for-both-goals ()
  "Refreshing the view requests the tactic goal and the expected type."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by\n  trivial\n"
      (let ((source (current-buffer)))
        (lean4-test-with-active-info-buffer source
          (forward-line 1)
          (lean4-test-with-captured-requests requests
            (lean4-info-buffer-refresh)
            (should (equal '("$/lean/plainGoal" "$/lean/plainTermGoal")
                           (mapcar #'car requests)))
            ;; Both carry the document and the position of point.
            (dolist (request requests)
              (let ((params (nth 1 request)))
                (should (equal (lsp--path-to-uri
                                (buffer-local-value 'buffer-file-name source))
                               (plist-get (plist-get params :textDocument) :uri)))
                (should (equal 1 (plist-get (plist-get params :position) :line)))))))))))

(ert-deftest lean4-test-lsp-goal-requests-are-cancellable-and-ignore-errors ()
  "Each goal request is tick-scoped, separately cancellable, and quiet.
Point moves constantly, so a request has to be superseded rather than
queued; and a server which does not know the method must not fill the
echo area with errors."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by trivial\n"
      (let ((source (current-buffer)))
        (lean4-test-with-active-info-buffer source
          (lean4-test-with-captured-requests requests
            (lean4-info-buffer-refresh)
            (let ((tokens (mapcar (lambda (r) (plist-get (nth 3 r) :cancel-token))
                                  requests)))
              (should (equal '(:plain-goal :plain-term-goal) tokens)))
            (dolist (request requests)
              (should (equal 'tick (plist-get (nth 3 request) :mode)))
              (should (eq #'ignore (plist-get (nth 3 request) :error-handler))))))))))

(ert-deftest lean4-test-lsp-plain-goal-response-is-stored ()
  "The tactic goals from a response are kept and a redisplay requested."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by\n  trivial\n"
      (let ((source (current-buffer)))
        (lean4-test-with-active-info-buffer source
          (lean4-test-with-captured-requests requests
            (lean4-info-buffer-refresh)
            (funcall (nth 2 (car requests))
                     (lean4-test-payload
                      "{\"goals\":[\"a b : Nat\\n⊢ a + b = b + a\"],
                        \"rendered\":\"ignored\"}"))
            (should (equal ["a b : Nat\n⊢ a + b = b + a"] lean4-goals))
            (should (> lean4-test-renders 0))))))))

(ert-deftest lean4-test-lsp-plain-term-goal-response-is-stored ()
  "The expected type from a response is kept."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "def d : Nat := 0\n"
      (let ((source (current-buffer)))
        (lean4-test-with-active-info-buffer source
          (lean4-test-with-captured-requests requests
            (lean4-info-buffer-refresh)
            (funcall (nth 2 (nth 1 requests))
                     (lean4-test-payload "{\"goal\":\"⊢ Nat\"}"))
            (should (equal "⊢ Nat" lean4-term-goal))))))))

(ert-deftest lean4-test-lsp-empty-goal-response-clears-the-view ()
  "A response with no goals clears what was there before.
Otherwise a finished proof would keep showing the goals it had."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by trivial\n"
      (let ((source (current-buffer)))
        (lean4-test-with-active-info-buffer source
          (setq lean4-goals ["stale goal"])
          (lean4-test-with-captured-requests requests
            (lean4-info-buffer-refresh)
            (funcall (nth 2 (car requests))
                     (lean4-test-payload "{\"goals\":[]}"))
            (should (equal [] lean4-goals))))))))

(ert-deftest lean4-test-lsp-refresh-is-skipped-while-hidden ()
  "Nothing is requested while the goal view is not on display.
The view is refreshed from `post-command-hook', so a hidden view has to
cost nothing."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "theorem t : True := by trivial\n"
      (unwind-protect
          (save-window-excursion
            (lean4-ensure-info-buffer lean4-info-buffer-name)
            (delete-other-windows)
            (switch-to-buffer (current-buffer))
            (should-not (lean4-info-buffer-active lean4-info-buffer-name))
            (lean4-test-with-captured-requests requests
              (lean4-info-buffer-refresh)
              (should-not requests)))
        (when (get-buffer lean4-info-buffer-name)
          (kill-buffer lean4-info-buffer-name))))))

;;; Reloading a file's dependencies.

(ert-deftest lean4-test-lsp-refresh-file-dependencies ()
  "Reloading closes and reopens the document, in that order.
Closing and reopening is what makes the server rebuild the file's
imports; the reopen has to carry the current text and version."
  (lean4-test-with-directory root '("Mod.lean")
    (lean4-test-with-source-buffer (expand-file-name "Mod.lean" root)
        "import Mathlib\ntheorem t : True := by trivial\n"
      (let ((uri (lsp--path-to-uri (buffer-file-name)))
            sent)
        (cl-letf (((symbol-function 'lsp-notify)
                   (lambda (method params) (push (cons method params) sent))))
          (lean4-refresh-file-dependencies))
        (setq sent (nreverse sent))
        (should (equal '("textDocument/didClose" "textDocument/didOpen")
                       (mapcar #'car sent)))
        ;; The close names the document being reloaded.
        (should (equal uri (plist-get (plist-get (cdr (car sent)) :textDocument)
                                      :uri)))
        ;; The reopen carries its language, version and current text.
        (let ((document (plist-get (cdr (nth 1 sent)) :textDocument)))
          (should (equal uri (plist-get document :uri)))
          (should (equal "lean" (plist-get document :languageId)))
          (should (equal "import Mathlib\ntheorem t : True := by trivial\n"
                         (plist-get document :text))))))))

(provide 'lean4-lsp-test)
;;; lean4-lsp-test.el ends here
