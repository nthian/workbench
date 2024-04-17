(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :hook ((org-mode . auto-fill-mode))
  :config
  (progn
    (setq org-directory "~/org"
	  org-agenda-files
	  '("inbox.org" "projects.org")
	  org-capture-templates
	  `(("i"  "Inbox" entry (file "inbox.org")
	     ,(concat "* TODO %?\n"
		      "/Entered on/ %U"))
	    ("c" "org-protocol-capture" entry (file "inbox.org")
	     "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))
	  org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d@)"))
	  org-log-done 'note
	  org-log-into-drawer "LOGBOOK")))

(require 'org-protocol)

(provide 'hal)
