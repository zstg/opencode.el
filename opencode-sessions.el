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

(require 'opencode-api)
(require 'vtable)

(defvar opencode-sessions-buffer nil
  "Buffer of current opencode session.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defun opencode-kill-session (session)
  "Kill SESSION."
  (let-alist session
    (opencode-api-delete-session (.id)
        result
      (if result
          (opencode-sessions-redisplay)
        (error "Unable to delete session")))))

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
                         :actions '("x" opencode-kill-session)
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
