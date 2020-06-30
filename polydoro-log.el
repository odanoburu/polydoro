;;; polydoro-log.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 bruno cuconato

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'cl-lib)

(defvar polydoro-log-file (expand-file-name "polydoro.log" user-emacs-directory))

(defun polydoro-log-- (what)
  "Write WHAT to ‘polydoro-log-file’."
  (write-region (format "[%s] %s\n" (current-time-string) what) nil polydoro-log-file t nil))

;;;###autoload
(defun polydoro-log (&optional event)
  "Log polydoro events to ‘polydoro-log-file’."
  (when event
    (polydoro-log-- (format "Pomodoro %s" event))))


(provide 'polydoro-log)
