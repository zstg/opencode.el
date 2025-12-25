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
    (pop-to-buffer buffer)))

(provide 'opencode)
;;; opencode.el ends here
