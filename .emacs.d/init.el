;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; most of personal settings are separated out so I can use them at
;; work without leaking work specific configurations onto the
;; interwebz.

(add-to-list 'load-path
	     "~/.emacs.d/lisp"
	     t)

(require 'workbench)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a0fa9c4582d3c50f6afdcbed2ea759d1e2e17caa3bccb62dbccc090be1129582" "b30ab3b30e70f4350dad6bfe27366d573ace2190cc405c619bd5e602110c6e0c" "217d2ec28f9ea0bf81f0d278557c84defc9e9c232002fa7ea0dd1426b35e286b" "4fe1b8318b32e2444eb8e3515041b1fc4249ae0718ebe098658430d29446d0a6" "0d02484ffe37515d22a0c0a3709eb8a153114d69dff0b594f574637558d4fb7e" "d19b94d54f496bfca31bb9e3db039ce89fa363f1bdd42e160e8482ed9232c280" "1d3e46a0c17b53065822932227d26acd88c47c831f922079527565f4d3c9cff7" default))
 '(package-selected-packages
   '(org-modern nordic-night-theme colorless-themes elfeed use-package slime paredit markdown-mode magit haskell-mode go-mode geiser ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
