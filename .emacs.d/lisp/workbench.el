;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package paredit
  :ensure t
  :hook ((lisp-interaction-mode emacs-lisp-mode lisp-mode) . paredit-mode))

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
  :bind
  (("C-c c" . org-capture))
  :config
  (progn
    (setq fill-column 80
	  org-directory "~/org"
	  org-agenda-files '("inbox.org"))
    (setq org-capture-templates
	  `(("i" "Inbox" entry (file "inbox.org")
	     ,(concat "* TODO %?\n"
		      "/Entered on/ %U"))))))

(use-package geiser-guile
  :ensure t)

;;; Encoding
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;;; Modes
(show-paren-mode 1)
(column-number-mode 1)

(add-hook 'term-mode-hook
	  (lambda ()
	    (term-set-escape-char ?\C-x)))

(xterm-mouse-mode 1)

;;; slime
(let ((quicklisp-file (expand-file-name "~/.quicklisp/slime-helper.el")))
  (if (file-exists-p quicklisp-file)
      (progn
	(load quicklisp-file)
	(setq inferior-lisp-program "sbcl"))))

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
    (setq aw-dispatch-always nil)
    (set-face-attribute 'aw-leading-char-face nil
			:weight 'bold)))

(setq server-window #'pop-to-buffer)

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

(defun snarf (p1 p2)
  (interactive "r")
  (save-excursion
    (if (not mark-active)
	(display-message-or-buffer "Snarfed whole buffer!"))
    (kill-ring-save p1 p2 mark-active)))

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
(global-set-key (kbd "M-%") #'replace-regexp)
(global-set-key (kbd "M-w") #'snarf)

(add-to-list 'custom-theme-load-path
	     (expand-file-name "~/.emacs.d/lisp"))

(require 'hal)
(require 'glazer)
(require 'plumber)
(require 'mail-config)
(require 'rss-config)
(provide 'workbench)
