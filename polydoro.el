;;; polydoro.el --- simple and flexible pomodoro timer -*- lexical-binding: t; -*-
;; Copyright (C) 2020 bruno cuconato

;; Author: bruno cuconato <bcclaro+emacs@gmail.com>
;; Maintainer: bruno cuconato <bcclaro+emacs@gmail.com>
;; URL: https://github.com/odanoburu/polydoro
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
(require 'cl-lib)
(require 'subr-x)
(require 'timer)
(require 'rmc)

(defgroup polydoro nil
  "Customization options for polydoro's pomodoro timer."
  :prefix 'polydoro-
  :group 'Applications)

(defcustom polydoro-pomodoro-length
  (* 60 25)
  "Length of a pomodoro.

See `polydoro-pomodoro-rests' for configuring pomodoro rests."
  :type 'integer)

(defcustom polydoro-pomodoro-rests
  (list
   :pomodoros-per-session 4
   :short-rest (* 60 5) :long-rest (* 60 30))
  ;; TODO: implement code to honor this variable
  "NOT IMPLEMENTED YET. Configuration for polydoro rests.

Currently this variable has almost no effect — you must manage
your rests manually. It's nothing hard to implement, but I
haven't gotten to it yet (you can do so if you want!) Below is
the description of the variable as if it were useful right now:

If non-nil, `polydoro' will automatically manage pomodoros and
their rests. The value must be a plist specifying how many
pomodoros constitute a session, the length of a short rest
between two pomodoros, and the length of a long rest between two
pomodoro sessions. (Lengths are specified in seconds).

If nil, the user will manage his rests manually, resting as long
as they want and restarting the timer themselves."
  :type '(restricted-sexp :match-alternatives ('nil polydoro-pomodoro-config-p)))

(defcustom polydoro-running-lighter
  " ⏲"
  "Indicator in the mode line for running pomodoro timer"
  :type 'string)

(defcustom polydoro-idle-lighter
  " 🍅"
  "Indicator in the mode line for idle pomodoro timer."
  :type 'string)

(defcustom polydoro-command-prompt
  "Pomodoro: "
  "Prompt used by ‘polydoro’."
  :type 'string)

(defcustom polydoro-running-choices
  `((?c "cancel" "cancel the pomodoro" ,#'polydoro-cancel)
    (?p "pause" "pause the pomodoro" ,#'polydoro-pause)
    (?R "reset" "reset the pomodoro" ,#'polydoro-reset)
    (?q "quit" "do nothing" ,#'(lambda nil nil)))
  "Choices available when pomodoro is running."
  :type '(alist :key-type character :value-type (list string string function)))

(defcustom polydoro-paused-choices
  `((?c "cancel" "cancel the pomodoro" ,#'polydoro-cancel)
    (?p "resume" "resume the pomodoro" ,#'polydoro-resume)
    (?r "resume" "resume the pomodoro" ,#'polydoro-resume)
    (?R "reset" "reset the pomodoro" ,#'polydoro-reset)
    (?q "quit" "do nothing" ,#'(lambda nil nil)))
  "Choices available when pomodoro is paused.

Running ‘polydoro’ will present these choices"
  :type '(alist :key-type character :value-type (list string string function)))

(defcustom polydoro-command-key
  (kbd "C-c C-p")
  "Key for calling ‘polydoro’ command."
  :type 'key-sequence)


;;;###autoload
(defun polydoro ()
  (interactive)
  (unless polydoro-mode
    (polydoro-mode 1))
  (funcall
   (cond
    ((polydoro--running-p)
     (cl-fourth (read-multiple-choice polydoro-command-prompt polydoro-running-choices)))
    ((polydoro--paused-p)
     (cl-fourth (read-multiple-choice polydoro-command-prompt polydoro-paused-choices)))
    (t
     #'polydoro-start))))


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


(defun polydoro--toggle-lighter (active?)
  "Update the name for ‘polydoro-mode’ in `minor-mode-alist'."
  (if-let ((spec (assq 'polydoro-mode minor-mode-alist))
	   (new  (if active? polydoro-running-lighter polydoro-idle-lighter)))
      (setf (nth 1 spec) new)
    (error "polydoro: broken invariant (sorry!)"))
  (force-mode-line-update t))


(defun polydoro--internal-hook (event)
  "Perform internal book-keeping for EVENT.

The interactive functions provided by this package all call this
hook after some checks. All internal logic/state is handled
here."
  (cl-case event
    ((start session-start)
     ;; set up timer to handle pomodoro end
     (let ((timer (run-at-time polydoro-pomodoro-length nil #'polydoro--over)))
       (setf polydoro--current-pomodoro timer))
     (when polydoro-pomodoro-rests
       (cl-incf polydoro--session-number))
     (polydoro--toggle-lighter t))
    (pause
     ;; canceling merely removes the timer from ‘timer-list’
     (cancel-timer polydoro--current-pomodoro)
     ;; we remember the current time to be able to fix the timer if it
     ;; gets resumed
     (setf polydoro--paused-at (current-time))
     (polydoro--toggle-lighter nil))
    (resume
     (let* ((pause-time (time-subtract (current-time) polydoro--paused-at))
	    (pause-duration (float-time pause-time)))
       ;; fix triggering time of the timer
       (timer-inc-time polydoro--current-pomodoro pause-duration))
     ;; add timer back to ‘timer-list’
     (timer-activate polydoro--current-pomodoro)
     (setf polydoro--paused-at nil)
     (polydoro--toggle-lighter t))
    (cancel
     (when (polydoro--running-p)
       (cancel-timer polydoro--current-pomodoro))
     (when polydoro-pomodoro-rests
       (cl-decf polydoro--session-number))
     (setf polydoro--paused-at nil)
     (polydoro--toggle-lighter nil))
    ((over session-over)
     ;; TODO: set up next pomodoro if user requested it
     (setf polydoro--current-pomodoro nil)
     (when (eq event 'session-over)
       (setf polydoro--session-number 0))
     (polydoro--toggle-lighter nil))))


(defun polydoro--run-hook (event)
  "Call every function in `polydoro-mode-hook' with argument EVENT."
  (mapc (lambda (f) (funcall f event))
	polydoro-mode-hook)
  ;; run internal (book-keeping) hook
  (polydoro--internal-hook event))


(defun polydoro--running-p ()
  "Return t if pomodoro is running."
  (and polydoro--current-pomodoro
       (memq polydoro--current-pomodoro timer-list)
       (not polydoro--paused-at)))


(defun polydoro--paused-p ()
  "Return t if pomodoro is paused."
  (and polydoro--current-pomodoro
       (not (memq polydoro--current-pomodoro timer-list))
       polydoro--paused-at))


(defun polydoro-start ()
  "Start pomodoro.

Runs ‘polydoro-mode-hook’ with the symbol ‘start’ or the symbol
‘session-start’ as argument. When the pomodoro is over,
‘polydoro-mode-hook’ is called with the symbol ‘over’ or the symbol
‘session-over’ as argument."
  (interactive)
  (when (polydoro--running-p)
    (user-error "Pomodoro already running"))
  (when (polydoro--paused-p)
    (user-error "Pomodoro is paused. Run ‘polydoro-resume’ to resume it"))
  ;; run start hook
  (let* ((first-in-session (and polydoro-pomodoro-rests (= polydoro--session-number 0)))
	 (event (if first-in-session 'session-start 'start)))
    (polydoro--run-hook event))
  (message "Pomodoro started"))


(defun polydoro--over ()
  "Finish pomodoro.

Runs ‘polydoro-mode-hook’ with the symbol ‘over’ or the symbol
‘session-over’ as argument."
  (let* ((last-in-session (and polydoro-pomodoro-rests
			       (= polydoro--session-number (polydoro--pomodoros-per-session))))
	 (event (if last-in-session 'session-over 'over)))
    (polydoro--run-hook event)
    (message "Pomodoro is over")))


(defun polydoro-pause ()
  "Pause the current pomodoro.

Runs ‘polydoro-mode-hook’ with the symbol ‘pause’ as argument."
  (interactive)
  (unless (polydoro--running-p)
    (user-error "You are not in pomodoro"))
  (when (polydoro--paused-p)
    (user-error "Pomodoro already paused"))
  (polydoro--run-hook 'pause)
  (message "Pomodoro paused"))


(defun polydoro-resume ()
  "Resume the current pomodoro.

Runs ‘polydoro-mode-hook’ with the symbol ‘resume’ as argument."
  (interactive)
  (when (polydoro--running-p)
    (user-error "Pomodoro is already running"))
  (unless (polydoro--paused-p)
    (user-error "No pomodoro to resume"))
  ;; this will set ‘polydoro--paused-at’ to nil
  (polydoro--run-hook 'resume)
  (message "Pomodoro resumed"))


(defun polydoro-cancel ()
  "Cancel the current pomodoro.

Runs ‘polydoro-mode-hook’ with the symbol ‘cancel’ as argument"
  (interactive)
  (unless (or (polydoro--paused-p) (polydoro--running-p))
    (user-error "No pomodoro to cancel"))
  (polydoro--run-hook 'cancel)
  (message "Pomodoro canceled"))


(defun polydoro-reset ()
  "Reset the current pomodoro.

It's the same thing as canceling and then starting a new
pomodoro."
  (interactive)
  (polydoro-cancel)
  (polydoro-start))


(defvar polydoro-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map polydoro-command-key #'polydoro)
    map)
  "Produces polydoro's keymap.")


(define-minor-mode polydoro-mode
  "Run pomodoros simply and easily."
  :lighter polydoro-idle-lighter
  :keymap polydoro-keymap
  :global t)


(defun polydoro--status ()
  "Return and display remaining time in the current pomodoro."
  (interactive)
  (let ((running? (polydoro--running-p))
	(paused? (polydoro--paused-p)))
    (unless (or running? paused?)
      (message "No pomodoro"))
    (let* ((remaining-time (time-subtract (timer--time polydoro--current-pomodoro)
					  (current-time)))
	   (remaining-time (decode-time remaining-time))
	   (minutes (nth 1 remaining-time))
	   (seconds (nth 0 remaining-time)))
      (message "Pomodoro is %s, %s minutes and %s seconds left"
	       (if running? "running" "paused") minutes seconds))))


(provide 'polydoro)
