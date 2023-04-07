;; Save as my-settings.el and customize

;; Themes
(defvar my/themes
      '("themes"
        :light tango
        :dark deeper-blue))

(defvar my/current-theme :dark)

(defun my/get-theme (theme)
  (plist-get (cdr my/themes) theme))

(defun my/toggle-light-dark-theme ()
  "Toggle light or dark theme defined in my/themes"
  (interactive)
  (cond ((eq my/current-theme :light)
         (setq my/current-theme :dark))
        ((eq my/current-theme :dark)
         (setq my/current-theme :light)))
  (load-theme (my/get-theme my/current-theme) t))

;; Fonts
(defvar my/font
      '("font"
        :face default
        :family "Hack"
        :weight normal
        :width normal
        :height 100))

(defvar my/variable-pitch
      '("variable pitch face"
        :face variable-pitch
        :family "Noto Sans"))

;; org-mode
(defvar my/org-config
      '("org-mode"
        :org-agenda-files nil
        :org-default-notes-file org-default-notes-file
        :org-work-tasks-file nil
        :org-personal-tasks-file nil
        :org-journal-file nil
        :org-roam-directory nil))

;; Mail
(defvar
  my/user-mail-address  (concat user-login-name "@" system-name))
(defvar
  my/user-full-name     (user-full-name))
(defvar
  my/gmail-smtp         nil)


(defun my/set-docplist-attribute (plist attribute value)
  "Set the VALUE of the ATTRIBUTE of the doc-plist PLIST."
  (setq plist (plist-put (cdr plist) attribute value)))
