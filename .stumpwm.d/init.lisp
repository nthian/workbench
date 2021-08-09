(in-package :stumpwm)

(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")
(run-shell-command  "xsetroot -cursor_name left_ptr")
(setf *mouse-focus-policy* :click)

;;; Custom Functionality
(defcommand (push-window tile-group) (frame-number) ((:frame t))
  "Push a window to the selected frame."
  (let ((group (current-group))
	(window (current-window)))
    (pull-window window frame-number)
    (focus-frame group frame-number)))

;;; Key Bindings
(define-key *root-map*
  (kbd "c") "exec gnome-terminal")
(define-key *root-map*
  (kbd "m") "push-window")


;;; Customizations
;;;   - Windows
(setf *resize-increment* 50)
(setf *window-border-style* :thin)
(set-win-bg-color "#ffffff")
(set-focus-color "#ff0000")
(set-unfocus-color "#000000")

;;;   - Font
(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
;; ;; (xft:cache-fonts)
(set-font
 (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 8))

;; ;;; Modules
;; (load-module "swm-gaps")
;; (setf swm-gaps:*head-gaps-size* 0)
;; (setf swm-gaps:*inner-gaps-size* 5)

;;; Slime
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
		     :style swank:*communication-style*
		     :dont-close t)
