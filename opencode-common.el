;;; opencode-common.el --- Common code shared through the package  -*- lexical-binding: t; -*-

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

;; Common code shared through the package

;;; Code:

(require 'map)

(defvar opencode-agents nil
  "List of available primary agents (excluding sub-agents and hidden agents).")

(defvar opencode-slash-commands nil
  "List of available slash commands.")

(defvar opencode-providers nil
  "List of available providers and models.")

(defvar opencode--event-subscriptions nil
  "An alist mapping: SSE event process to it's directory.")

(defvar opencode--session-control-buffers nil
  "An alist mapping of projectIDs to session control buffers for that project.")

(defun opencode--time-ago (opencode-object type)
  "Return .time.TYPE value from OPENCODE-OBJECT, as seconds ago."
  (- (float-time)
     (/ (map-nested-elt opencode-object `(time ,type))
        1000)))

(defun opencode--annotated-completion (prompt candidates annotation-function)
  "Simplified and formatted completing read with PROMPT.
CANDIDATES is a list of lists, where the first element of each list is the
string to show, and the whole list will be passed to ANNOTATION-FUNCTION.
Returns the cdr of the list for the candidate selected (to return the info
excluding the display string)."
  (let* ((max-length (seq-max
                      (mapcar (lambda (candidate)
                                (length (car candidate)))
                              candidates)))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (concat (make-string (+ 5 (- max-length
                                            (length candidate)))
                                    ?\s)
                       (funcall annotation-function
                                (cdr (assoc-string candidate candidates))))))))
    (cdr (assoc-string
          (completing-read prompt candidates nil t)
          candidates))))

(provide 'opencode-common)
;;; opencode-common.el ends here
