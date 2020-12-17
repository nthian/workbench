;;; plumber.el --- a framework to take actions based on text patterns.

;; Copyright (C) 2020 Ian Thiele

;; Author: Ian Thiele <icthiele@gmail.com>
;; Keywords: automation, convenience
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This was loosely inspired by the plumbing mechanism in plan9port. It
;; is a fairly simple implementation, but is hopefully flexible enough
;; to easily extend.

(defun plumber/expand-selection (point)
  "Expand the selection out from `point` until a blank, newline,
quote, or brace character is reached. If one of these characters
should be included in the message to the dispatcher, it should be
manually selected as a region; a pre-selected region is passed
through without modification."
  (interactive "d")
  (let ((pos (save-excursion
	       (if (not mark-active)
		   (let ((cclass "^[:blank:]\n\"'([{}])"))
		     (skip-chars-backward cclass)
		     (push-mark)
		     (skip-chars-forward cclass)))
	       (plumber/plumb-region (mark) (point)))))
    (when (number-or-marker-p pos)
      (deactivate-mark)
      (goto-char pos))))

(defun plumber/expand-selection-mouse (event &rest args)
  "Sibling function to plumber/expand-selection to wrap mouse functionality.
Moves the mouse to the point chosen by whichever handler handled the message."
  (interactive "e\np")
  (mouse-set-point event)
  (let* ((pos (plumber/expand-selection (point)))
  	 (coords (window-absolute-pixel-position pos)))
    (x-set-mouse-absolute-pixel-position (car coords) (cdr coords))))

(defun plumber/plumb-region (p1 p2 &optional buffer)
  "Selects the substring from `buffer` and hands it off to the dispatcher.

If `buffer` is not passed or nil, then the current buffer is assumed."
  (interactive "r")
  (let ((buffer (or buffer (current-buffer))))
    (plumber/dispatch
     buffer
     (expand-file-name default-directory)
     (buffer-substring-no-properties p1 p2))))

(defvar plumber/dispatchers (vector)
  "A list of dispatchers that should attempt to handle each message.")

(defun plumber/install-dispatcher (fun &optional pos)
  (let* ((n (seq-length plumber/dispatchers))
	 (v (vector fun)))
    (if pos
	(setq plumber/dispatchers (vconcat (seq-subseq plumber/dispatchers 0 pos)
					   v
					   (seq-subseq plumber/dispatchers pos)))
      (setq plumber/dispatchers (vconcat plumber/dispatchers v)))))

(defun plumber/dispatch (src wdir data)
  (let ((pos nil))
    (seq-take-while (lambda (fun)
		      (let ((rval (funcall fun src wdir data)))
			(when (number-or-marker-p rval)
			  (setq pos rval))
			(null (funcall fun src wdir data))))
		    plumber/dispatchers)
    pos))

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

(defun plumber/xdg-open (src widr data)
  (let* ((re "\\.\\(docx?\\|pdf\\|png\\)$")
	 (m (string-match re data)))
    (when m
      (call-process "xdg-open"
		    nil
		    nil
		    nil
		    data))))

(defun plumber/xref-find-definitions (src wdir data)
  (condition-case err
      (progn
	(xref-find-definitions data)
	(message "xref: point: %d" (point))
	(point))
    (error nil)))

(defun plumber/web-browser (src wdir data)
  (let* ((re "^\\(https?://[^[:blank:]]+\\)")
	 (m (string-match re data)))
    (when m
      (let ((url (match-string 1 data)))
	(start-process "plumber/browser"
		       nil
		       (or (getenv "BROWSER")
			   "firefox")
		       data)))))

(defun plumber/search (src wdir data)
  (let* ((narrow-re "\\([a-zA-Z0-9_/\\$-]+\\)")
	 (matches (string-match narrow-re data))
	 (text (when matches (match-string 1 data)))
	 (start (min (+ (mark) matches 1) (point-max)))
	 (width (length text)))
    (or
     (save-excursion
       (goto-char start)
       (when (search-forward text nil t)
	 (- (point) width)))
     (save-excursion
       (goto-char (point-min))
       (when (search-forward text nil t)
	 (- (point) width)))
     nil)))

(defun plumber/open-file-with-action (file action)
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

(plumber/install-dispatcher #'plumber/xdg-open)
(plumber/install-dispatcher #'plumber/file-colon-number)
(plumber/install-dispatcher #'plumber/file-colon-regexp)
(plumber/install-dispatcher #'plumber/xref-find-definitions)
(plumber/install-dispatcher #'plumber/web-browser)
(plumber/install-dispatcher #'plumber/search)

(global-set-key (kbd "M-.") #'plumber/expand-selection)
(global-set-key (kbd "<mouse-3>") #'plumber/expand-selection-mouse)

(provide 'plumber)
