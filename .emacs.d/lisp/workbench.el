;; packages
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . auto-fill-mode))

(use-package magit
  :ensure t)

(use-package org
  :ensure t
  :hook ((org-mode . auto-fill-mode))
  :config
  (progn
    (setq fill-column 80)))

;;; Modes
(show-paren-mode 1)
(column-number-mode 1)

(add-hook 'term-mode-hook
	  (lambda ()
	    (term-set-escape-char ?\C-x)))

;;; Look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-default 'fill-column 80)

(setq ring-bell-function 'ignore)

(setq frame-background-mode 'light)

(defun set-font (font)
  (set-frame-font font t t)
  (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
  (add-to-list 'default-frame-alist (cons 'font font)))
(set-font "IBM Plex Mono-11")

(set-face-attribute 'mode-line nil
		    :box nil)
(set-face-attribute 'secondary-selection nil
		    :background "lightgoldenrod")
(set-face-attribute 'match nil
		    :background "lightgoldenrod")
(global-font-lock-mode 1)

(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time)

;;; Reducing Friction
(use-package ace-window
  :ensure t
  :bind
  (("M-'" . ace-window)
   ("C-x M-'" . ace-window))
  :config
  (progn
    (setq aw-dispatch-always t)
    (set-face-attribute 'aw-leading-char-face nil
			:weight 'bold)))

;; Use the ssh-agent in /tmp/ssh-*/agent.* if it exists.
(defun set-ssh-agent ()
  (interactive)
  (dolist (file (file-expand-wildcards "/tmp/ssh-*/agent.*"))
    (let ((file-attrs (file-attributes file))
	  (pid (string-to-number (file-name-extension file))))
      (if (and (= (user-uid)
		  (file-attribute-user-id file-attrs))
	       (and pid (process-attributes pid)))
	  (setenv "SSH_AUTH_SOCK" file)))))
(set-ssh-agent)

;; backups
(add-to-list 'backup-directory-alist
	     '("." . "~/.saves/"))
(setq delete-old-versions t)
(setq inhibit-startup-message t)

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

(require 'plumber)
(provide 'workbench)
