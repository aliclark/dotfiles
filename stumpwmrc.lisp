;-*-Lisp-*-

(in-package :stumpwm)

;;; 2.2 - Binding Keys

(set-prefix-key (kbd "C-i"))

;;; 3.1 - Message and Input Bar

(set-fg-color "green")
(set-bg-color "black")
(set-border-color "green")
(setf *message-window-padding* 5)

;;; 4.2 - Windows

(set-win-bg-color "green")
(set-focus-color "green")
(set-unfocus-color "black")

;;; 11 - Miscellaneous Commands

(setf *startup-message* nil)

;;; 11.2 - Debugging StumpwWM

(setf stumpwm::*debug-level* 10)

;; Not sure whether this is in the manual or not
(setf *mouse-focus-policy* :sloppy)

;;; Create some new commands

(defmacro defror (name &rest keys)
  (let ((sname (string-downcase (symbol-name name))))
    `(progn
       (defcommand ,name () ()
         ,(concatenate 'string "Run or raise " sname ".")
         (run-or-raise ,sname '(:class ,sname)))
       ,@(mapcar
          (lambda (key)
            `(define-key *root-map* (kbd ,key) ,sname))
          keys))))

(defror conkeror "C-f" "f")
(defror spotify "C-r")
(defror xterm "C-c" "c")

;;; Create some new keys

(define-key *root-map* (kbd "C-o") "fnext")

;; Create a second group for whatever
(gnewbg "second")

;(run-shell-command "xrandr --output LVDS --off --output VGA --mode 1440x900 --pos 0x0" t)

(run-shell-command "xsetroot -solid black" t)

