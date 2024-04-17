(defun glazer/disable-custom-themes ()
  (mapcar (lambda (x) (disable-theme x)) custom-enabled-themes))

(defun glazer/light-theme ()
  (set-face-attribute 'default nil
		      :foreground "#000000"
		      :background "#FFFFFF")
  (set-face-attribute 'region nil
		      :background "#EEDD82")
  (set-face-attribute 'highlight nil
		      :background "#F5DEb3")
  (set-face-attribute 'mode-line nil
		      :box nil
		      :background "grey75"
		      :foreground "black")
  (setq frame-background-mode 'light))

(defun glazer/dark-theme ()
  (set-face-attribute 'default nil
		      :background "#1F1421"
		      :foreground "#D0CFCC")
  (set-face-attribute 'region nil
		      :background "#6A5ACD")
  (set-face-attribute 'highlight nil
		      :background "#473c8b"
		      :distant-foreground "gtk_selection_fg_color")
  (set-face-attribute 'mode-line nil
		      :box nil
		      :foreground "#D0CFCC"
		      :background "#473c8b")
  (setq frame-background-mode 'dark))

(defvar glazer/theme-list
  '(glazer/light-theme glazer/dark-theme))

(defun glazer/apply-theme ()
  (apply (seq-first glazer/theme-list) nil))

(defun glazer/toggle-theme ()
  (interactive)
  (glazer/disable-custom-themes)
  (setq glazer/theme-list
	(append (cdr glazer/theme-list) (list (car glazer/theme-list))))
  (glazer/apply-theme)
  (mapcar #'frame-set-background-mode (visible-frame-list)))

(add-hook 'server-after-make-frame-hook #'glazer/apply-theme)

;;; Look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)

(set-default 'fill-column 80)
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '((font . "Liberation Mono-11")))
(global-font-lock-mode 1)


(provide 'glazer)
