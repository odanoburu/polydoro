;;; polydoro.el --- very simple pomodoro timer -*- lexical-binding: t; -*-
;; Copyright (C) 2020 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/
;; Version: 0.0.1
;; Package-Requires: ((emacs "26")
;; Keywords: extensions productivity

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

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup polydoro nil
  "Customization options for polydoro's pomodoro timer."
  :prefix 'polydoro-)

(defcustom polydoro-pomodoro-length
  (* 60 25)
  "Length of a pomodoro.

See `polydoro-pomodoro-rests' for configuring pomodoro rests."
  ;; must be positive!
  :type 'integer)

(defcustom polydoro-pomodoro-rests
  (list
   :pomodoros-per-session 4
   :short-rest (* 60 5) :long-rest (* 60 30))
  "Configuration for polydoro rests.

If non-nil, `polydoro' will automatically manage pomodoros and
their rests. The value must be a plist specifying how many
pomodoros constitute a session, the length of a short rest
between two pomodoros, and the length of a long rest between two
pomodoro sessions. (Lengths are specified in seconds).

If nil, the user will manage his rests manually, resting as long
as they want and restarting the timer themselves."
  :type (restricted-sexp :match-alternatives ('nil polydoro-pomodoro-config-p)))

(defcustom polydoro-running-lighter
  "🍅"
  "Indicator in the mode line for running pomodoro timer"
  ;; FIXME: could be other stuff too, like a function
  :type 'string)

(defcustom polydoro-idle-lighter
  "⏲"
  "Indicator in the mode line for idle pomodoro timer."
  :type 'string)

(defcustom polydoro-run-prefix-key
  [F9]
  "Prefix key for running pomodoro timer.")

(defcustom polydoro-idle-prefix-key
  [F9]
  "Prefix key for idle pomodoro timer.")

(defcustom polydoro-hook
  nil
  "Functions to run when interacting with pomodoro timer.

")


(defun positive-p (integer)
  (and (integerp integer)
       (>= integer 0)))


(cl-defun polydoro--pomodoros-per-session (&optional (config polydoro-pomodoro-rests))
  (plist-get config :pomodoros-per-session))
(cl-defun polydoro--short-rest (&optional (config polydoro-pomodoro-rests))
  (plist-get config :short-rest))
(cl-defun polydoro--long-rest (&optional (config polydoro-pomodoro-rests))
  (plist-get config :long-rest))


(defun polydoro-pomodoro-config-p (config)
  (and (listp config)
       (positive-p (polydoro--pomodoros-per-session config))
       (positive-p (polydoro--short-rest config))
       (postivie-p (polydoro--long-rest config))))


(define-minor-mode polydoro-running-mode
  "TODO:"
  nil
  polydoro-running-lighter)

(define-minor-mode polydoro-idle-mode
  "TODO:"
  nil
  polydoro-idle-lighter
  :type 'string)

(provide 'polydoro)
