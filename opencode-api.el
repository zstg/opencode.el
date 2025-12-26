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

(defmacro opencode-api-sessions (return-var &rest body)
  "Run BODY with RETURN-VAR bound to list of sessions."
  (declare (indent defun))
  (cl-with-gensyms (current-buffer)
    `(let ((,current-buffer (current-buffer)))
       (plz 'get (concat opencode-api-url "/session")
         :as (lambda () (json-parse-buffer :array-type 'list
                                      :object-type 'alist))
         :then (lambda (alist)
                 (let ((,return-var alist))
                   (with-current-buffer ,current-buffer
                     ,@body)))))))

(provide 'opencode-api)
;;; opencode-api.el ends here
