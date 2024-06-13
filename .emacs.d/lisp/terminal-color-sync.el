(defun resolve-color (color palette)
  (if (symbolp color)
      (resolve-color (car (alist-get color palette)) palette)
    color))

(defun palette-color (color palette)
  (resolve-color
   (car (alist-get color palette))
   palette))

(defun gnome-terminal-dconf-slots (uuid)
  (let* ((result)
	 (dconfdir (format "%s/:%s" gnome-terminal-dconf-dir uuid))
	 (output (process-lines "dconf" "list" (format "%s/" dconfdir))))
    (princ output)
    (dolist (slot output)
      (setq result (append result `(,(format ":%s" slot)
				    ,(string-trim (shell-command-to-string (format "dconf read %s/%s" dconfdir slot)))))))
    result))

(defun gnome-terminal-profile--populate (uuid)
  (let ((dconf-slots (dolist (e  ))))
    (gnome-terminal-profile-create
     :uuid)))

(defconst gnome-terminal-dconf-dir "/org/gnome/terminal/legacy/profiles:")

(cl-defstruct (gnome-terminal-profile (:constructor gnome-terminal-profile-create)
				      (:copier nil))
  uuid
  dconf-path
  background-color
  foreground-color
  bold-is-bright
  custom-command
  font
  login-shell
  palette
  scroll-on-keystroke
  scrollback-unlimited
  scrollbar-policy
  use-custom-command
  use-system-font
  use-theme-colors
  visible-name)

(defun gnome-terminal-profile-uuids ()
  (mapcar
   (lambda (x)
     (string-trim x
		  "[: \t\r\n]+"
		  "[/ \t\r\n]+"))
   (seq-filter
    (lambda (x)
      (string-match-p "^:[0-9a-f\-]+/" x))
    (process-lines "dconf" "list" (format "%s/" gnome-terminal-dconf-dir)))))

(defun dconf-type (val type)
  (cond ((string= type "bool")
	   (dconf-bool val))
	  ((string= type "number")
	   (dconf-number val))
	  ((string= type "string")
	   (dconf-string val))
	  ((string= type "list-of-strings")
	   (dconf-list-of-strings val))
	  (t
	   (dconf-string val))))

(defun dconf-read (dir key &optional type)
  (let ((val (string-trim (shell-command-to-string (format "dconf read %s/%s" dir key)))))
    (dconf-type val type)))

(defun dconf-write (dir key val type)
  (let ((dconf-val (dconf-type val type)))
    (shell-command-to-string
     (format "dconf write %s/%s \"%s\"" dir key dconf-val))))

(defun dconf-bool (val)
  (if (booleanp val)
      (if val "true" "false")
    (if (string= val "true") t nil)))

(defun dconf-number (val)
  (if (stringp val)
      (string-to-number val)
    (number-to-string val)))

(defun dconf-string (val)
  (cond ((string-match-p "^'.*'$" val)
	 (string-trim val "[']" "[']"))
	((= (length val) 0)
	 nil)
	(t
	 (format "'%s'" val))))

(defun dconf-list-of-strings (val)
  (if (stringp val)
      (split-string val ", " nil "\\[?'\\]?")
    (format "[%s]"
	    (string-join
	     (mapcar
	      (lambda (x)
		(format "'%s'" x))
	      val) ", "))))

(defun gnome-terminal-profile-from-dconf (uuid)
  (let ((dconf-path (format "%s/:%s" gnome-terminal-dconf-dir uuid)))
    (gnome-terminal-profile-create
     :uuid uuid
     :dconf-path dconf-path
     :background-color (dconf-read dconf-path "background-color")
     :foreground-color (dconf-read dconf-path "foreground-color")
     :palette (dconf-read dconf-path "palette" "list-of-strings")
     :visible-name (dconf-read dconf-path "visible-name"))))

(defun get-gnome-terminal-profiles ()
  (let ((uuids 
	 (dconf-read
	  gnome-terminal-dconf-dir
	  "list" "list-of-strings"))
	(result '()))
    (dolist (uuid uuids result)
      (let ((p (gnome-terminal-profile-from-dconf uuid)))
	(add-to-list 'result (cons (gnome-terminal-profile-visible-name p) p))))))

(defun modus-theme-export (name &optional uuid)
  (let* ((theme-palette (symbol-value
		   (intern
		    (concat
		     "modus-"
		     name
		     "-tinted-palette"))))
	 (uuid (or uuid (string-trim (shell-command-to-string "uuid"))))
	 (uuids (seq-uniq (append (dconf-read gnome-terminal-dconf-dir "list" "list-of-strings") (list uuid))))
	 (profile
	  (gnome-terminal-profile-create
	   :uuid uuid
	   :background-color (palette-color 'bg-main theme-palette)
	   :foreground-color (palette-color 'fg-main theme-palette)
	   :use-theme-colors nil
	   :palette (list
		     (palette-color 'fg-term-black theme-palette)
		     (palette-color 'fg-term-red theme-palette)
		     (palette-color 'fg-term-green theme-palette)
		     (palette-color 'fg-term-yellow theme-palette)
		     (palette-color 'fg-term-blue theme-palette)
		     (palette-color 'fg-term-magenta theme-palette)
		     (palette-color 'fg-term-cyan theme-palette)
		     (palette-color 'fg-term-white theme-palette)
		     (palette-color 'fg-term-black theme-palette)
		     (palette-color 'fg-term-red-bright theme-palette)
		     (palette-color 'fg-term-green-bright theme-palette)
		     (palette-color 'fg-term-yellow-bright theme-palette)
		     (palette-color 'fg-term-blue-bright theme-palette)
		     (palette-color 'fg-term-magenta-bright theme-palette)
		     (palette-color 'fg-term-cyan-bright theme-palette)
		     (palette-color 'fg-term-white-bright theme-palette))
	   :visible-name (format "modus-%s" name)))
	 (dconf-path (format "%s/:%s"
			     gnome-terminal-dconf-dir uuid)))
    (dconf-write dconf-path "background-color" (gnome-terminal-profile-background-color profile) "string")
    (dconf-write dconf-path "foreground-color" (gnome-terminal-profile-foreground-color profile) "string")
    (dconf-write dconf-path "palette" (gnome-terminal-profile-palette profile) "list-of-strings")
    (dconf-write dconf-path "visible-name" (gnome-terminal-profile-visible-name profile) "string")
    (dconf-write dconf-path "use-theme-colors" (gnome-terminal-profile-use-theme-colors profile) "bool")
    (dconf-write gnome-terminal-dconf-dir "list" uuids "list-of-strings")))
