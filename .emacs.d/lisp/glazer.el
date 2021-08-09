(defvar glazer/default-column-width -85)

(defun glazer/new-column (&optional window)
  (interactive)
  (let* ((frame (selected-frame))
	 (window (or window (selected-window)))
	 (parent (window-parent window))
	 (split-base
	  (if (frame-root-window-p parent)
	      window
	    parent)))
    (select-window
     (split-window split-base
		   glazer/default-column-width
		   'right))
    (balance-windows frame)))

(setq display-buffer-base-action
      '(display-buffer-below-selected . ((window-min-height . 30))))

(global-set-key
 (kbd "C-x 3") #'glazer/new-column)

(provide 'glazer)
