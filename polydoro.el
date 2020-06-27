;;; polydoro.el --- very simple pomodoro timer -*- lexical-binding: t; -*-
;; Copyright (C) 2020 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
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
(require 'timer)

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
  :type '(restricted-sexp :match-alternatives ('nil polydoro-pomodoro-config-p)))

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
  "Prefix key for running pomodoro timer."
  :type 'key-sequence)

(defcustom polydoro-running-multiple-choice
  ;; TODO:
  nil)

(defcustom polydoro-idle-prefix-key
  [F9]
  "Prefix key for idle pomodoro timer."
  :type 'key-sequence)

(defcustom polydoro-hook
  nil
  "Functions to run when interacting with pomodoro timer.

Each function will be called with a symbol argument describing
the event that is happening. The possible events are:
"
  :risky t
  :type 'hook)


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
       (positive-p (polydoro--long-rest config))))

;;; state
(defvar polydoro--current-pomodoro
  nil
  "Store the current pomodoro timer, if any.")

(defvar polydoro--session-number
  0
  "The number of pomodoros already run in this session.")

(defvar polydoro--paused-at
  nil
  "If non-nil, the time when the pomodoro was paused.

Used to calculate when the pomodoro is over after resumed.")


(defun polydoro--internal-hook (event)
  (cl-case event
    ((start session-start)
     nil)
    (pause
     (setf polydoro--paused-at (current-time)))
    (resume
     (setf polydoro--paused-at nil))
    (interrupt
     ;; interrupting is pausing + setting polydoro--paused-at to nil
     (setf polydoro--paused-at nil))
    ((over session-over)
     (setf polydoro--current-pomodoro nil))))

(defun polydoro--run-hook (event)
  "Call every function in `polydoro-hook' with argument EVENT."
  (mapc (lambda (f) (funcall f event))
	polydoro-hook)
  ;; run internal (book-keeping) hook
  (polydoro--internal-hook event))


(defun polydoro-run ()
  "Run pomodoro.

Runs ‘polydoro-hook’ with the symbol ‘start’ or the symbol
‘session-start’ as argument. When the pomodoro is over,
‘polydoro-hook’ is called with the symbol ‘over’ or the symbol
‘session-over’ as argument."
  (interactive)
  ;; run start hook
  (let* ((first-in-session (= polydoro--session-number 0))
	 (event (if first-in-session 'session-start 'start)))
    (polydoro--run-hook event))
  ;; set up timer
  (let* ((last-in-session (>= (1+ polydoro--session-number) (polydoro--pomodoros-per-session)))
	 (event (if last-in-session 'session-over 'over))
	 (timer (run-at-time polydoro-pomodoro-length nil #'polydoro--run-hook event)))
    (setf polydoro--current-pomodoro timer)))


(defun polydoro-pause ()
  "Pause the current pomodoro.

Runs ‘polydoro-hook’ with the symbol ‘pause’ as argument."
  (interactive)
  (unless polydoro--current-pomodoro
    (user-error "You are not in pomodoro"))
  ;; this will set ‘polydoro--paused-at’ too:
  (polydoro--run-hook 'pause)
  (cancel-timer polydoro--current-pomodoro))


(defun polydoro-resume ()
  "Resume the current pomodoro.

Runs ‘polydoro-hook’ with the symbol ‘resume’ as argument."
  (interactive)
  (unless (and polydoro--current-pomodoro
	       polydoro--paused-at)
    (user-error "Not in a paused pomodoro"))
  (let* ((pause-time (time-subtract (current-time) polydoro--paused-at))
	 (pause-duration (float-time pause-time)))
    (timer-inc-time polydoro--current-pomodoro pause-duration))
  ;; this will set ‘polydoro--paused-at’ to nil
  (polydoro--run-hook 'resume)
  (timer-activate polydoro--current-pomodoro))


(defun polydoro-interrupt ()
  "Interrupt the current pomodoro.

Runs ‘polydoro-hook’ with the symbol ‘interrupt’ as argument"
  (interactive)
  ;; interrupting the same as pausing and then setting ‘polydoro--paused-at’
  ;; to nil
  (polydoro-pause)
  ;; this sets ‘polydoro--paused-at’ to nil
  (polydoro-run-hook 'interrupt))


(defun polydoro-running-keymap ()
  (let ((prefix-map (make-sparse-keymap))
	(mode-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "p") _)
    ;; bind minor to minor mode keymap and return it
    (define-key mode-map polydoro-run-prefix-key prefix-map)
    mode-map))


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
