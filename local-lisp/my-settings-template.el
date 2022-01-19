;; Save as my-settings.el and customize

;; Themes
(setq my/themes
      '("themes"
        :light spacemacs-light
        :dark spacemacs-dark))

(setq my/current-theme :dark)

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
(setq my/font
      '("font"
        :family "Hack"
        :weight normal
        :width normal
        :height 100))

;; org-mode
(setq my/org-config
      '("org-mode"
        :org-agenda-files nil
        :org-default-notes-file org-default-notes-file))

;; Mail
(setq
 my/user-mail-address  (concat user-login-name "@" system-name)
 my/user-full-name     (user-full-name)
 my/gmail-smtp         nil)
