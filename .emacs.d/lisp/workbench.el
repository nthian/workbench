;; packages
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(require 'color)

(use-package paredit
  :ensure t
  :hook ((lisp-interaction-mode emacs-lisp-mode) . paredit-mode))

(use-package haskell-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  (progn
    (add-hook 'go-mode-hook (lambda (&rest args)
			      (setq tab-width 4)))))

;;; Modes
(show-paren-mode 1)
(auto-fill-mode 1)
(column-number-mode 1)

(add-hook 'term-mode-hook
	  (lambda ()
	    (term-set-escape-char ?\C-x)))

;;; Look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-background-mode 'light)

(defun set-font (font)
  (set-default-font font)
  (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
  (add-to-list 'default-frame-alist (cons 'font font)))
(set-font "IBM Plex Mono-10")

(set-face-attribute 'mode-line nil
		    :box nil)
(global-font-lock-mode 1)

(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time)

;;; Reducing Friction
;; Ace Window is a nice little utility for popping around when
;; multiple buffers are open. 
(use-package ace-window
  :ensure t
  :bind
  (("M-'" . ace-window))
  :config
  (progn
    (set-face-attribute 'aw-leading-char-face nil
			:weight 'bold)))

;; backups
(add-to-list 'backup-directory-alist
	     '("." . "~/.saves/"))
(setq delete-old-versions t)

;; Custom Functions
(defun kill-and-close (&rest args)
  (interactive)
  (let ((buf (current-buffer))
	(win (selected-window)))
    (kill-buffer buf)
    (delete-window win)))

;;; Custom Keybindings
(global-set-key (kbd "C-x C-o") (lambda (&rest args)
				(interactive)
				(other-window -1)))
(global-set-key (kbd "C-x C-k") #'kill-and-close)
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "<f5>") (lambda (&rest args)
			       (interactive)
			       (revert-buffer t t t)))
(global-set-key (kbd "M-%") #'query-replace-regexp)

(provide 'workbench)
