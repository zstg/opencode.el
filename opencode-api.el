;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

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

;; Wrapper for opencode api

;;; Code:

(require 'plz)
(require 'json)

(defvar opencode-api-url nil
  "URL of the opencode server we're connected to.")

(eval-and-compile
  (cl-defun opencode-api--call (method path return-var body &key data)
    "Generate the body of an opencode api wrapper macro.
Using METHOD, for endpoint PATH, saving result in RETURN-VAR,
and saving to CURRENT-BUFFER while running BODY."
    (cl-with-gensyms (current-buffer)
      `(let ((,current-buffer (current-buffer)))
         (plz ',method (concat opencode-api-url ,path)
           :as (lambda () (json-parse-buffer :array-type 'list
                                        :object-type 'alist))
           ,@(when data
               `(:headers '(("Content-Type" . "application/json"))
                 :body (json-encode ,data)))
           :then (lambda (alist)
                   (let ((,return-var alist))
                     (with-current-buffer ,current-buffer
                       ,@body)))
           :else (lambda (response)
                   (error (format "error requesting %s: %s" ,path response)))))))

  (cl-defun opencode-api--wrap (method path &key elisp-macro-name)
    "Define a macro to wrap api call with METHOD and PATH.
optionally provide ELISP-MACRO-NAME where opencode-api-elisp-macro-name will be
the name of the created macro."
    (let ((elisp-macro-name
           (intern
            (concat "opencode-api"
                    (if elisp-macro-name
                        (concat "-" (symbol-name elisp-macro-name))
                      (string-replace "/" "-" path))))))
      (eval
       (if (seq-contains-p path ?\%)
           (if (eq method 'postdata)
               `(defmacro ,elisp-macro-name
                    (args data return-var &rest body)
                  ,(format "Wrapper for opencode %s endpoint." path)
                  (declare (indent 3))
                  (opencode-api--call 'post `(format ,,path ,@args) return-var body
                                      :data data))
             `(defmacro ,elisp-macro-name
                  (args return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method `(format ,,path ,@args) return-var body)))
         (if (eq method 'postdata)
             `(defmacro ,elisp-macro-name
                  (data return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call 'post ,path return-var body
                                    :data data))
           `(defmacro ,elisp-macro-name
                (return-var &rest body)
              ,(format "Wrapper for opencode %s endpoint." path)
              (declare (indent defun))
              (opencode-api--call ',method ,path return-var body)))))))

  (defun opencode-api-define-wrapper (endpoint)
    "Create a macro wrapping ENDPOINT."
    (let ((method 'get))
      (when (and (listp endpoint)
                 (seq-contains-p '(get post postdata) (car endpoint)))
        (setf method (pop endpoint)))
      (pcase endpoint
        ((pred stringp) (opencode-api--wrap method endpoint))
        (`(,elisp-name ,path) (opencode-api--wrap method path :elisp-macro-name elisp-name)))))

  (defun opencode-api-define-wrappers (api-endpoints)
    "Define wrapping macros for a list of API-ENDPOINTS."
    (dolist (api-endpoint api-endpoints)
      (opencode-api-define-wrapper api-endpoint)))

  (opencode-api-define-wrappers
   '("/global/health"
     (projects "/project")
     (current-project "/project/current")
     "/path"
     "/vcs"
     "/config"
     (configured-providers "/config/providers")
     (providers "/provider")
     (provider-authentication-methods "/provider/auth")
     (session "/session/%s")
     (session-children "/session/%s/children")
     (session-todos "/session/%s/todo")
     (session-diff "/session/%s/diff")
     (session-messages "/session/%s/message")
     (message-details "/session/%s/message/%s")
     (sessions "/session")
     (sessions-status "/session/status")
     (commands "/command")
     "/file/status"
     (find-pattern "/find?pattern=%s")
     (find-files "/find/file?query=%s")
     (find-symbols "/find/symbol?query=%s")
     (list-files "/file?path=%s")
     (read-file "/file/content?path=%s")
     "/experimental/tool/ids"
     (tools-for-model "/experimental/tool?provider=%s&model=%s")
     "/lsp"
     "/formatter"
     "/mcp"
     (agents "/agent")
     (post dispose-instance "/instance/dispose")
     (postdata create-session "/session"))))

(provide 'opencode-api)
;;; opencode-api.el ends here
