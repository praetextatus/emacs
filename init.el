;; my emacs customizing

;; Better looks
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode 'nil)
(global-linum-mode 0)
(global-hl-line-mode 1)
(show-paren-mode 1)

(setq default-input-method "russian-computer")
(setq inhibit-startup-screen t)

;; never put tabs
(setq-default indent-tabs-mode nil)

;; more lenient garbage collection
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 64))  ; set GC threshold to 64Mb -- should be fine

;;;;;;;; PACKAGES ;;;;;;;;

;; elpa config
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))
(package-initialize)

;; use use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c p")
  :ensure t
  :hook ((python-mode . lsp))
  :commands lsp)
(use-package lsp-ui
  :ensure t)

;; various packages
(use-package company
  :ensure t)
(use-package org-bullets
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package flymake
  :ensure t)
(use-package dockerfile-mode
  :ensure t)


;;;;;;;; PERSONAL SETTINGS ;;;;;;;;
(let* ((my/settings-file
        (if (file-exists-p "~/.emacs.d/local-lisp/my-settings.el")
            "~/.emacs.d/local-lisp/my-settings.el"
          "~/.emacs.d/local-lisp/my-settings-template.el")))
  (load my/settings-file))

;; Theme
(load-theme (my/get-theme my/current-theme) t)

;; Fonts
(let ((font-attributes '(:family :weight :height :width)))
  (dolist (attribute font-attributes)
    (set-face-attribute 'default nil
                        attribute (plist-get (cdr my/font) attribute))))

;; org-mode
(use-package org
  :init
  (setq org-log-done t)
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE")
          (sequence "|" "CANCELLED")))
  (setq org-hide-emphasis-markers t)

  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode)
              (org-bullets-mode)))

  (setq org-agenda-files (plist-get (cdr my/org-config) :org-agenda-files))
  (setq org-default-notes-file (plist-get (cdr my/org-config) :org-default-note-files))

  :bind
  ("C-c a" . org-agenda))

;; Selectrum
(use-package selectrum
  :if (eq system-type 'gnu/linux)
  :ensure t
  :init
  (add-hook 'after-init-hook (lambda () (selectrum-mode +1))))
(use-package selectrum-prescient
  :if (eq system-type 'gnu/linux)
  :ensure t
  :init
  (add-hook 'after-init-hook (lambda () (selectrum-prescient-mode +1))))

;;;;;;;; MAIL ;;;;;;;;

;; mu4e
(use-package mu4e
  :bind ("C-c m" . mu4e)
  :config
  ;; general mu4e config
  (setq
   mail-user-agent                  'mu4e-user-agent
   mu4e-get-mail-command            "mbsync -a"
   mu4e-update-interval             600
   user-mail-address                my/user-mail-address
   user-full-name                   my/user-full-name
   mu4e-view-show-images            t
   mu4e-sent-messages-behavior      'delete)
  ;; headers fields
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:maildir . 12)
                              (:flags . 6)
                              (:from . 22)
                              (:subject)))
  ;; Gmail send (smtp) config
  (when my/gmail-smtp
      (setq
       message-send-mail-function    'smtpmail-send-it
       smtpmail-default-smtp-server  "smtp.gmail.com"
       smtpmail-smtp-server          "smtp.gmail.com"
       smtpmail-local-domain         "gmail.com"
       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
       smtpmail-smtp-service         587
       starttls-extra-arguments      nil
       starttls-gnutls-program       "gnutls-cli"
       starttls-extra-arguments      nil
       starttls-use-gnutls           t)))


;;;;;;;; CODING ;;;;;;;;
(use-package python
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)
              (setq python-indent-offset 4))))


(use-package cc-mode
  :init
  (add-hook 'c++-mode-hook
            (lambda ()
              (define-key c++-mode-map [?\C-c ?\C-c] 'compile)
              (define-key c++-mode-map [?\C-c d]   'gdb)
              (c-set-offset 'access-label '0)
              (c-set-offset 'inclass '+)
              (auto-complete-mode)))
  :mode ("\\.h\\'" . c++-mode))

;; treat .m files as Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;;;;;; DIRED ;;;;;;;;
(setq dired-listing-switches "-alh")


;;;;;;;; SPECIAL KEYS ;;;;;;;;
(global-set-key (kbd "C-c l") 'goto-line)


;;;;;;;; WINDOWS ;;;;;;;;

;; some Windows-specific options that are not local
(when (memq system-type '(windows-nt ms-dos))
  ;; tramp for windows
  (setq tramp-default-method "plink")
  ;; git ask password in gui (for windows)
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  ;; ido mode -- selectrum doesn't work in windows
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  ;; encoding
  (set-coding-system-priority 'utf-8 'utf-16 'windows-1251 'cp1251-dos)
  ;; Prevent issues with the Windows null device (NUL)
  ;; when using cygwin find with rgrep.
  (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
    "Use cygwin's /dev/null as the null-device."
    (let ((null-device "/dev/null"))
      ad-do-it))
  (ad-activate 'grep-compute-defaults))  

;; set custom file for Customize but never load it
(setq custom-file "~/.emacs.d/local-lisp/custom.el")
