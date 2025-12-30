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
(require 'markdown-mode)
(require 'opencode-api)
(require 'vtable)

(defvar opencode-sessions-buffer nil
  "Buffer of current opencode session.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              mode-line-process nil
              comint-input-sender #'ignore
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (visual-line-mode)
  (font-lock-mode -1)
  (add-hook 'comint-preoutput-filter-functions 'opencode--render-markdown nil t)
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

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode)

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  (let ((ov (make-overlay start (1- end)))
        (margin (propertize ">" 'display
                            `((margin left-margin)
                              ,(propertize "▎" 'face
                                           face)))))
    (overlay-put ov 'line-prefix margin)
    (overlay-put ov 'wrap-prefix margin)))

(defun opencode--render-markdown (string)
  "Render STRING in gfm-view-mode."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (gfm-view-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun opencode--render-input-markdown (input)
  "Rerender comint INPUT as markdown."
  (let ((inhibit-read-only t))
    (delete-region (process-mark (get-buffer-process (current-buffer)))
                   (point))
    (insert (opencode--render-markdown input))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (let ((beginning (point)))
    (dolist (part (alist-get 'parts message))
      (let-alist part
        (pcase .type
          ("text" (insert .text)))))
    (insert "\n")
    (comint-send-input)
    (opencode--add-margin beginning (point) 'opencode-request-margin-highlight)))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (let ((beginning (point)))
    (comint-output-filter (get-buffer-process (current-buffer)) text)
    (opencode--add-margin beginning (point) 'opencode-reasoning-margin-highlight)))

(defun opencode-open-session (session)
  "Open comint based shell for SESSION."
  (let-alist session
    (let ((buffer-name (format "*OpenCode: %s*" .title)))
      (if (get-buffer buffer-name)
          (pop-to-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (opencode-session-mode)
          (let ((proc (start-process buffer-name buffer-name nil)))
            (set-process-query-on-exit-flag proc nil)
            (opencode-api-session-messages (.id)
                messages
              (dolist (message messages)
                (let-alist (alist-get 'info message)
                  (pcase .role
                    ("user" (opencode--replay-user-request message))
                    ("assistant" (dolist (part (alist-get 'parts message))
                                   (let-alist part
                                     (let ((text (concat .text "\n\n")))
                                       (pcase .type
                                         ("text" (comint-output-filter proc text))
                                         ("reasoning" (opencode--insert-reasoning-block text))))))))))))
          (pop-to-buffer buffer-name))))))

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
                         :keymap (define-keymap
                                   "r" 'opencode-sessions-redisplay))
          (insert "No sessions"))
        (goto-char point)))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
