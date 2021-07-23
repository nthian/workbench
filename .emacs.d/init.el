;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; most of personal settings are separated out so I can use them at
;; work without leaking work specific configurations onto the
;; interwebz.

(add-to-list 'load-path
	     "~/.emacs.d/lisp"
	     t)

(require 'workbench)

(let ((slime-file (expand-file-name "~/quicklisp/slime-helper.el")))
  (and (file-exists-p slime-file)
       (load (expand-file-name slime-file))))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   '("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default))
 '(package-selected-packages
   '(geiser gruvbox-theme base16-theme base16-themes acme-theme commentary-theme cyberpunk-theme doom-themes eink-theme subatomic-theme edit-indirect magit markdown-mode ace-window go-mode haskell-mode paredit use-package))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
