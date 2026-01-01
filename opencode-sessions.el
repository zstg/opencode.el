;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'comint)
(require 'diff)
(require 'diff-mode)
(require 'markdown-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'vtable)

(defvar opencode-sessions-buffer nil
  "Buffer of current opencode session.")

(defvar opencode-session-control-mode-map
  (define-keymap
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session))

(with-eval-after-load 'evil
  (declare-function evil-define-key "evil-core")
  (evil-define-key 'normal opencode-session-control-mode-map
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session))

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "TAB" 'opencode-cycle-session-agent
    "C-c r" 'opencode-rename-session))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-directory nil
  "Directory for the current opencode session buffer.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar opencode-assistant-messages
  nil
  "An alist mapping all currently updating assistant message ids, to start pos.")

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-agents'."
  (interactive)
  (let* ((pos (cl-position opencode-session-agent opencode-agents :test 'equal))
         (next (nth (1+ pos) opencode-agents)))
    (setf opencode-session-agent (or next (car opencode-agents))))
  (force-mode-line-update))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (format "[🤖 %s] %s  "
          (pcase opencode-session-agent
            ("Planner-Sisyphus" "Planner")
            (_ opencode-session-agent))
          (pcase opencode-session-status
            ("busy" "⏳")
            ("idle" "🚀")
            (_ ""))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (when-let ((buffer (gethash session-id opencode-session-buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq opencode-session-status status)
        (force-mode-line-update)))))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              mode-line-process '(:eval (opencode--session-status-indicator))
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (visual-line-mode)
  (font-lock-mode -1)
  (add-hook 'comint-input-filter-functions 'opencode--render-input-markdown nil t))

(defun opencode-yank-code-block ()
  "Yank the markdown code block under point."
  (interactive)
  (save-excursion
    (markdown-backward-block)
    (copy-region-as-kill (point)
                         (progn (markdown-forward-block)
                                (point)))))

(defun opencode-kill-session (session)
  "Kill SESSION."
  (opencode-api-delete-session ((alist-get 'id session))
      result
    (if result
        (opencode-sessions-redisplay)
      (error "Unable to delete session"))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  (opencode-api-send-message (opencode-session-id)
      `((agent . ,opencode-session-agent)
        (model (providerID . opencode) (modelID . grok-code))
        (parts ((type . text) (text . ,string))))
      _result))

(defun opencode-session--message-updated (info)
  "Handle message.updated event with INFO."
  (let-alist info
    (pcase .role
      ("assistant" (if .finish
                       (setf opencode-assistant-messages
                             (assoc-delete-all .id opencode-assistant-messages))
                     (push (cons .id nil) opencode-assistant-messages))))))

(defun opencode--render-last-block (type start)
  "Render block of TYPE (reasoning or text) since START with PROCESS."
  (when (seq-contains-p '(reasoning text) type)
    (let* ((end (opencode--session-process-position))
           (inhibit-read-only t)
           (text (opencode--render-markdown (buffer-substring start end))))
      (delete-region start end)
      (cl-case type
        (reasoning (opencode--insert-reasoning-block text))
        (text (opencode--output text))))))

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((and (eq ?\n (char-before pos))
            (not (eq ?\n (char-before (1- pos)))))
       "\n")
      (t "")))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (comint-output-filter (get-buffer-process (current-buffer)) string))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for line in logo-left
             for index from 0
             do
             (opencode--output (propertize line 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize (nth index logo-right) 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output."
  (opencode--output (propertize "> " 'invisible t))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight))

(defun opencode-session--update-part (part delta)
  "Display PART, partial message output. DELTA is new text since last update."
  (let-alist part
    (when-let ((buffer (gethash .sessionID opencode-session-buffers))
               (process (get-buffer-process buffer))
               (message-parts (assoc-string .messageID opencode-assistant-messages)))
      (with-current-buffer buffer
        (let ((last-type (cadr message-parts))
              (last-start (cddr message-parts)))
          (cl-flet ((maybe-render-last-and-update-message-parts
                      (type)
                      (unless (eq type last-type)
                        (when last-start
                          (opencode--render-last-block last-type last-start)
                          (opencode--maybe-insert-block-spacing))
                        (setf (cdr message-parts) (cons type
                                                        (marker-position (process-mark process)))))))
            (pcase .type
              ((and "reasoning" (guard delta))
               (maybe-render-last-and-update-message-parts 'reasoning)
               (opencode--insert-reasoning-block delta))
              ((and "text" (guard delta))
               (maybe-render-last-and-update-message-parts 'text)
               (opencode--output delta))
              ("tool" (maybe-render-last-and-update-message-parts 'tool)
               (when (string-equal .state.status "running")
                 (opencode--insert-tool-block .tool .state.input)))
              ("step-finish"
               (when (string-equal "stop" .reason)
                 (opencode--render-last-block last-type last-start)
                 (opencode--maybe-insert-block-spacing)
                 (opencode--show-prompt))))))))))

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defun opencode--margin (face)
  "Return margin string for FACE."
  (propertize ">" 'display
              `((margin left-margin)
                ,(propertize "▎" 'face
                             face))))

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  (let ((ov (make-overlay start (1- end)))
        (margin (opencode--margin face)))
    (overlay-put ov 'line-prefix margin)
    (overlay-put ov 'wrap-prefix margin)))

(defun opencode--render-markdown (string)
  "Render STRING in `gfm-view-mode'."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (gfm-view-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun opencode--render-input-markdown (input)
  "Rerender comint INPUT as markdown."
  (let ((inhibit-read-only t))
    (delete-region (opencode--session-process-position)
                   (point))
    (insert (opencode--render-markdown input))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (dolist (part (alist-get 'parts message))
    (let-alist part
      (pcase .type
        ("text" (insert .text)))))
  (insert "\n")
  (let ((comint-input-sender #'opencode--highlight-input))
    (comint-send-input)))

(defun opencode--format-edit-diff (old-string new-string)
  "Generate diff output comparing OLD-STRING to NEW-STRING."
  (with-temp-buffer
    (let ((old-buf (current-buffer)))
      (insert old-string)
      (insert "\n")
      (with-temp-buffer
        (let ((new-buf (current-buffer)))
          (insert new-string)
          (insert "\n")
          (with-temp-buffer
            (let ((diff-buf (current-buffer))
                  (inhibit-read-only t))
              ;; Run diff synchronously into this temp buffer
              (diff-no-select old-buf new-buf nil t diff-buf)
              (delay-mode-hooks (diff-mode))
              (font-lock-ensure)
              ;; Delete first 3 lines (diff command, ---, +++)
              (goto-char (point-min))
              (forward-line 3)
              (delete-region (point-min) (point))
              ;; Delete last 2 lines (diff finished timestamp)
              (goto-char (point-max))
              (forward-line -2)
              (delete-region (point) (point-max))
              ;; Return the diff content
              (buffer-string))))))))

(defun opencode--render-todos (todos)
  "Render TODOS as markdown todo list."
  (opencode--render-markdown
   (mapconcat
    (lambda (todo)
      (let-alist todo
        (format "- [%s] %s"
                (pcase .status
                  ((or "pending" "in_progress") " ")
                  ("completed" "x")
                  ("cancelled" "-")
                  (_ " "))
                .content)))
    todos
    "\n")))

(defun opencode--format-tool-call (tool input)
  "Format TOOL call with INPUT arguments for display."
  (let-alist input
    (when .filePath
      (setf .filePath (file-relative-name .filePath opencode-session-directory)))
    (pcase tool
      ("edit"
       (concat "edit " .filePath ":\n"
               (opencode--format-edit-diff .oldString .newString)
               "\n"))
      ("read"
       (if (and .offset .limit)
           (format "read %s [offset=%d, limit=%d]\n\n"
                   .filePath .offset .limit)
         (format "read %s\n\n" .filePath)))
      ("grep"
       (format "grep \"%s\" in %s\n\n" .pattern (or .include .path)))
      ("bash"
       (format "# %s\n$ %s\n\n"
               .description
               .command))
      ("websearch"
       (format "websearch \"%s\"\n\n" .query))
      ("todowrite"
       (concat (opencode--render-todos .todos) "\n\n"))
      (_ (if (= 1 (length input))
             (format "%s %s\n\n" tool (cdar input))
           ;; Multiple arguments: tool-name, then arg-name: value per line
           (concat tool "\n"
                   (mapconcat (lambda (pair)
                                (format "  %s: %s" (car pair) (cdr pair)))
                              input
                              "\n")
                   "\n\n"))))))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (let ((beginning (opencode--session-process-position)))
    (opencode--output text)
    (opencode--add-margin beginning (opencode--session-process-position) face)))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--refine-diff-hunks (start)
  "Refine all diff hunks between START and process marker."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (opencode--session-process-position))
      (condition-case nil
          (while (>= (point) start)
            (diff-refine-hunk)
            (diff-hunk-prev))
        (error nil)))))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    ;; For edit tools, apply diff hunk refinement after insertion
    (when (string= tool "edit")
      (opencode--refine-diff-hunks start))))

(defun opencode-open-session (session)
  "Open comint based shell for SESSION."
  (let-alist session
    (if (buffer-live-p (gethash .id opencode-session-buffers))
        (pop-to-buffer (gethash .id opencode-session-buffers))
      (let ((buffer (generate-new-buffer (format "*OpenCode: %s*" .title))))
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq opencode-session-id .id
                opencode-session-directory .directory
                opencode-session-agent (car opencode-agents))
          (puthash .id buffer opencode-session-buffers)
          (let ((proc (start-process "dummy" buffer nil)))
            (set-process-query-on-exit-flag proc nil)
            (opencode-insert-logo)
            (opencode-api-session-messages (.id)
                messages
              (dolist (message messages)
                (let-alist (alist-get 'info message)
                  (pcase .role
                    ("user" (opencode--replay-user-request message))
                    ("assistant" (dolist (part (alist-get 'parts message))
                                   (let-alist part
                                     (let ((text (opencode--render-markdown (concat .text "\n\n"))))
                                       (pcase .type
                                         ("text" (opencode--output text))
                                         ("tool" (opencode--insert-tool-block .tool .state.input))
                                         ("reasoning"
                                          (opencode--insert-reasoning-block
                                           text))))))))))
              (opencode--show-prompt)))
          (pop-to-buffer buffer))))))

(defun opencode-new-session (&optional title)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one."
  (interactive
   (list (when current-prefix-arg
           (read-string "Title: "))))
  (opencode-api-create-session (if title
                                   `((title . ,title))
                            (make-hash-table))
      session
    (opencode-open-session session)))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let ((title (read-string "Title: ")))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res
      (if session
          (opencode-sessions-redisplay)
        (rename-buffer
         (generate-new-buffer-name (format "*OpenCode: %s*" title)))))))

(defun opencode-sessions-redisplay ()
  "Refresh the session display table."
  (interactive)
  (opencode-api-sessions sessions
    (with-current-buffer opencode-sessions-buffer
      (let ((inhibit-read-only t)
            (point (point)))
        (erase-buffer)
        (if sessions
            (make-vtable :columns '("Title"
                                    (:name "Last Updated" :width 12
                                     :formatter seconds-to-string
                                     :primary ascend)
                                    (:name "Files changed" :width 13 :align right)
                                    "Created at")
                         :objects sessions
                         :actions '("x" opencode-kill-session
                                    "R" opencode-rename-session
                                    "RET" opencode-open-session
                                    "o" opencode-open-session)
                         :getter (lambda (object column vtable)
                                   (pcase (vtable-column vtable column)
                                     ("Title" (alist-get 'title object))
                                     ("Last Updated" (- (float-time)
                                                        (/ (alist-get 'updated
                                                                      (alist-get 'time object))
                                                           1000)))
                                     ("Files changed" (let-alist (alist-get 'summary object)
                                                        (if (or (null .files) (zerop .files))
                                                            "none"
                                                          (format "%d  +%d-%d" .files .additions .deletions))))
                                     ("Created at" (format-time-string
                                                    "%Y-%m-%d %H:%M:%S"
                                                    (seconds-to-time (/ (alist-get 'created
                                                                                   (alist-get 'time
                                                                                              object))
                                                                        1000))))))
                         :separator-width 3
                         :keymap opencode-session-control-mode-map)
          (insert "No sessions"))
        (goto-char point)))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
