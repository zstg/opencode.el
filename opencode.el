;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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

(require 'comint)
(require 'json)
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

(define-derived-mode opencode-mode comint-mode "OpenCode"
  "Major mode for interacting with opencode."
  (setq-local comint-use-prompt-regexp nil
              mode-line-process nil))

;;;###autoload
(defun opencode (&optional host port)
  "Start an opencode process.
If HOST and PORT are not given, use `opencode-host' and `opencode-port'.
With a prefix argument, prompt for HOST and PORT."
  (interactive
   (if current-prefix-arg
       (list (read-string "Host: " opencode-host)
             (read-number "Port: " opencode-port))
     (list opencode-host opencode-port)))
  (let* ((host (or host opencode-host))
         (port (or port opencode-port))
         (buffer (generate-new-buffer "*opencode*")))
    (with-current-buffer buffer
      (opencode-mode))
    (start-process (format "opencode-pty-%s-%s" host port) (current-buffer) nil)
    (set-process-query-on-exit-flag (opencode--process) nil)
    (add-hook 'kill-buffer-hook #'opencode--close-process)
    (opencode-process-events)
    (pop-to-buffer buffer)))

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

(defun opencode--handle-message (event)
  "Handle a message EVENT from opencode server."
  (opencode--log-event "MESSAGE" (json-read-from-string (plz-event-source-event-data event))))

(defun opencode-process-events ()
  "Connect to the opencode event stream and process all events."
  (let ((url (format "http://%s:%d/event" opencode-host opencode-port)))
    (plz-media-type-request
      'get url
      :as `(media-types
            ((text/event-stream
              . ,(plz-event-source:text/event-stream
                  :events `((open . ,(lambda (event)
                                       (opencode--log-event "OPEN" event)))
                            (message . opencode--handle-message)
                            (close . opencode--close-process))))))
      :then 'opencode--close-process)))

(provide 'opencode)
;;; opencode.el ends here
