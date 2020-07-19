(defun plumber/expand-selection (&rest args)
  (interactive "d")
  (let ((pos (save-excursion  
	       (if (not mark-active)
		   (let ((cclass "^[:blank:]=\n\"'"))
		     (skip-chars-backward cclass)
		     (push-mark)
		     (skip-chars-forward cclass)))
	       (plumber/plumb-region (mark) (point)))))
    (when (number-or-marker-p pos)
      (deactivate-mark)
      (goto-char pos))))

(defun plumber/expand-selection-mouse (event &rest args)
  "When called with a mouse"
  (interactive "e\np")
  (mouse-set-point event)
  (let* ((pos (plumber/expand-selection (point)))
  	 (coords (window-absolute-pixel-position pos)))
    (x-set-mouse-absolute-pixel-position (car coords) (cdr coords)))
  )

(defun plumber/plumb-region (p1 p2)
  (interactive "r")
  (plumber/dispatch
   (current-buffer)
   (expand-file-name default-directory)
   (buffer-substring-no-properties p1 p2)))

(defun plumber/dispatch (src wdir data)
  (plumber/message "plumber/dispatch {\n\tsrc: %s,\n\twdir: %s,\n\tdata: %s\n}\n" src wdir data)
  (cond
   ((plumber/file-colon-number src wdir data))
   ((plumber/file-colon-regexp src wdir data))
   (t (plumber/search src wdir data))))

(defvar plumber/file-path-re "^\\(/?\\(?:[^/]+/\\)*[^/:]+\\)")
(defvar plumber/colon-line-re "\\(?::\\([[:digit:]]+\\)\\)?")
(defvar plumber/colon-regexp-re ":/\\(.+\\)\\(?:/\\|$\\)")

(defun plumber/file-colon-number (src wdir data)
  (let* ((re (concat plumber/file-path-re plumber/colon-line-re))
	 (m (string-match re data))
	 (file (expand-file-name (match-string 1 data)))
	 (line (match-string 2 data)))
    (if (file-exists-p file)
	(plumber/open-file-with-action file
				       (function (lambda ()
						   (plumber/goto-line line))))
      nil)))

(defun plumber/file-colon-regexp (src wdir data)
  (let* ((re (concat plumber/file-path-re plumber/colon-regexp-re))
	 (m (string-match re data))
	 (file (expand-file-name (match-string 1 data)))
	 (regexp (match-string 2 data)))
    (if (file-exists-p file)
	(plumber/open-file-with-action file
				       (function (lambda ()
						   (re-search-forward regexp))))
      nil)))

(defun plumber/search (src wdir data)
  (let* ((narrow-re "\\([a-zA-Z0-9_/\\$-]+\\)")
	 (matches (string-match narrow-re data))
	 (text (when matches (match-string 1 data)))
	 (start (min (+ (mark) matches 1) (point-max)))
	 (width (length text)))
    (plumber/message "narrow search: %s, start: %d" text start)
    (or
     (save-excursion
       (goto-char start)
       (when (search-forward text nil t)
	 (- (point) width)))
     (save-excursion
       (goto-char start)
       (search-backward text nil t))
     nil)))

(defun plumber/open-file-with-action (file action)
  (plumber/message "action: %s" action)
  (find-file-other-window file)
  (funcall action)
  (point))

(defun plumber/goto-line (line)
  (let ((n (when line (string-to-number line))))
    (when n
      (goto-char (point-min))
      (forward-line (1- n)))))

(defun plumber/message (fmt &rest args)
  (let ((inhibit-message t))
    (message (apply #'format fmt args))))

(global-set-key (kbd "M-.") #'plumber/expand-selection)
(global-set-key (kbd "<mouse-3>") #'plumber/expand-selection-mouse)
