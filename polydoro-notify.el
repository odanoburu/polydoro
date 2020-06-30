;;; polydoro-notify.el -*- lexical-binding: t; -*-
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

(defvar polydoro-notify-command "notify-send")

(defun polydoro-notify-- (program-args)
  (apply #'start-process "polydoro-notify" nil polydoro-notify-command program-args))

;;;###autoload
(defun polydoro-notify (&optional event)
  "Notify of polydoro EVENT using ‘polydoro-notify-command’."
  (cl-case event
    ((over session-over)
     (polydoro-notify-- (list "Time is up!" "Pomodoro is over")))))


(provide 'polydoro-notify)
