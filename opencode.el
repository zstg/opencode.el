;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; URL: https://codeberg.org/sczi/opencode.el/

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

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'notifications)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defvar opencode--plz-event-request nil
  "Request process streaming events from /event on opencode server.")

(defvar opencode--event-subscriptions nil
  "An alist mapping: SSE event process to it's directory.")

;;;###autoload
(defun opencode (&optional host port)
  "Connect to opencode server, or open buffer to existing connection.
If HOST and PORT are not given, use `opencode-host' and `opencode-port'.
With a prefix argument, prompt for HOST and PORT."
  (interactive
   (if current-prefix-arg
       (list (read-string "Host: " opencode-host)
             (read-number "Port: " opencode-port))
     (list opencode-host opencode-port)))
  (unless (process-live-p opencode--plz-event-request)
    (setq opencode-api-url (format "http://%s:%d" host port))
    (opencode--fetch-agents)
    (opencode-api-configured-providers result
      (setq opencode-providers (alist-get 'providers result))))
  (opencode-open-project
   (when-let (proj (project-current))
     (directory-file-name (project-root proj)))))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (unless (rassoc directory opencode--event-subscriptions)
      (opencode-process-events directory))
  (let ((buffer-name (opencode-session-control-buffer-name directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq opencode-directory directory)
        (opencode-sessions-redisplay)))
    (pop-to-buffer buffer-name)))


(defun opencode-select-project ()
  "Completing read to prompt which project to select."
  (interactive)
  (opencode-api-projects projects
    (opencode-open-project
     (cl-first
      (opencode--annotated-completion "Project: "
                                      (cl-loop for project in projects
                                               for worktree = (alist-get 'worktree project)
                                               collect (list (string-remove-prefix
                                                              (expand-file-name "~/")
                                                              worktree)
                                                             worktree
                                                             (seconds-to-string
                                                              (opencode--updated-time
                                                               project))))
                                      (lambda (candidate)
                                        (cl-second candidate)))))))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
     (save-excursion
       (goto-char (point-max))
       (insert (format "[%s] %s: %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S")
                       type
                       event))
       (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defun opencode--toast-show (properties)
  "Show toast notification with PROPERTIES from opencode."
  (let-alist properties
    (notifications-notify
     :title .title
     :body .message
     :urgency (pcase .variant
                ("error" 'critical)
                ("warning" 'critical)
                ((or "info" "success") 'normal))
     :timeout .duration
     :replaces-id 5647
     :app-icon 'none)))

(defun opencode--handle-message (event)
  "Handle a message EVENT from opencode server."
  (let ((data (json-read-from-string (plz-event-source-event-data event))))
    (opencode--log-event "MESSAGE" data)
    (let-alist (alist-get 'properties data)
      (cl-case (intern (alist-get 'type data))
        (tui.toast.show (opencode--toast-show (alist-get 'properties data)))
        (session.idle (opencode-api-session (.sessionID)
                          session
                        (let ((buffer (gethash .sessionID opencode-session-buffers)))
                          (unless (and (eq buffer (window-buffer (selected-window)))
                                       (frame-focus-state (window-frame (selected-window))))
                            (opencode--toast-show `((title . "OpenCode Finished")
                                                    (message . ,(alist-get 'title session))
                                                    (variant . "success")
                                                    (timeout . 1000)))))))
        (session.status (opencode-session--set-status .sessionID .status.type))
        ((session.created session.updated session.deleted)
         (if-let (buffer (get-buffer (opencode-session-control-buffer-name .info.directory)))
             (with-current-buffer buffer
                 (opencode-sessions-redisplay))))
        (session.error (opencode-session--display-error .sessionID .error.data.message))
        (message.part.updated (opencode-session--update-part .part .delta))
        (message.updated (opencode-session--message-updated .info))
        (otherwise (opencode--log-event "WARNING" "unhandled message type"))))))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (cl-loop for (process) in opencode--event-subscriptions
           do (kill-process process))
  (setq opencode--event-subscriptions nil))

(defun opencode--fetch-agents ()
  "Fetch available agents from server and filter out subagents or hidden agents."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-remove (lambda (agent)
                        (or (string= "subagent" (alist-get 'mode agent))
                            (alist-get 'hidden agent)))
                      agents))))

(defun opencode-process-events (directory)
  "Connect to the opencode event stream and process all events for DIRECTORY."
  (let ((process
         (plz-media-type-request
           'get (concat opencode-api-url "/event")
           :as `(media-types
                 ((text/event-stream
                   . ,(plz-event-source:text/event-stream
                       :events `((open . ,(lambda (event)
                                            (opencode--log-event "OPEN" event)))
                                 (message . ,(lambda (event)
                                               (let ((opencode-directory directory))
                                                 (opencode--handle-message event))))
                                 (close . opencode-disconnect))))))
           :headers `(("x-opencode-directory" . ,directory))
           :then 'opencode-disconnect
           :else 'opencode-disconnect)))
    (set-process-query-on-exit-flag process nil)
    (push (cons process directory) opencode--event-subscriptions)))

(provide 'opencode)
;;; opencode.el ends here
