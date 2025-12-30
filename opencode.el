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
(require 'opencode-sessions)
(require 'plz-media-type)
(require 'plz-event-source)

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defcustom opencode-mode-hook nil
  "Hook run after `opencode-mode' is entered."
  :type 'hook
  :group 'opencode)

(defvar opencode--plz-event-request nil
  "Request process streaming events from /event on opencode server.")

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
  (unless (and (get-buffer "*opencode-sessions*")
               (process-live-p opencode--plz-event-request))
    (setq opencode-sessions-buffer (generate-new-buffer "*OpenCode Sessions*"))
    (let ((host (or host opencode-host))
          (port (or port opencode-port)))
      (with-current-buffer opencode-sessions-buffer
        (opencode-session-control-mode)
        (setq opencode-api-url (format "http://%s:%d" host port))
        (start-process (format "opencode-pty-%s-%s" host port) opencode-sessions-buffer nil)
        (set-process-query-on-exit-flag (opencode--process) nil)
        (add-hook 'kill-buffer-hook #'opencode--close-process)
        (add-hook 'kill-buffer-hook #'opencode--disconnect)
        (opencode-process-events))))
  (opencode-sessions-redisplay)
  (pop-to-buffer opencode-sessions-buffer))

(defun opencode--process ()
  "Get the dummy PTY process associated with this opencode buffer."
  (get-buffer-process (current-buffer)))

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (with-current-buffer (get-buffer-create "*opencode-log*")
    (goto-char (point-max))
    (insert (format "[%s] %s: %s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S")
                    type
                    event))))

(defun opencode--close-process (&optional event)
  "Handle shutdown of opencode server, logging EVENT."
  (when (opencode--process)
    (opencode--log-event "CLOSE" event)
    (delete-process (opencode--process))))

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
        (session.idle (opencode--toast-show '((title . "Opencode")
                                              (message . "response finished")
                                              (variant . "success")
                                              (timeout . 1000))))
        (otherwise (opencode--log-event "WARNING" "unhandled message type"))))))

(defun opencode--disconnect ()
  "Disconnect from opencode server."
  (when (process-live-p opencode--plz-event-request)
    (opencode--log-event "DISCONNECT" "emacs disconnecting")
    (kill-process opencode--plz-event-request)))

(defun opencode-process-events ()
  "Connect to the opencode event stream and process all events."
  (setf opencode--plz-event-request
        (plz-media-type-request
          'get (concat opencode-api-url "/event")
          :as `(media-types
                ((text/event-stream
                  . ,(plz-event-source:text/event-stream
                      :events `((open . ,(lambda (event)
                                           (opencode--log-event "OPEN" event)))
                                (message . opencode--handle-message)
                                (close . opencode--close-process))))))
          :then 'opencode--close-process
          :else 'opencode--close-process))
  (set-process-query-on-exit-flag opencode--plz-event-request nil))

(provide 'opencode)
;;; opencode.el ends here
