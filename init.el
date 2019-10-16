;; my emacs customizing

(menu-bar-mode 1)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)

;; Better looks
(global-linum-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

;; coding style
(setq-default c-default-style "ellemtel"
      c-basic-offset 4
      tab-width 4
      tab-indent-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent-offset 4)))

;; hook for c++ mode
(defun my-c++-mode-hook ()
  (define-key c++-mode-map [?\C-c ?\C-c] 'compile)
  (define-key c++-mode-map [?\C-c d]   'gdb)
  (c-set-offset 'access-label '0)
  (c-set-offset 'inclass '+)
  (auto-complete-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; treat .h files as c++ 
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; treat .m files as Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; special keys
(global-set-key "\C-cl" 'goto-line)
(global-set-key (kbd "C-x g") 'magit-status)

;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq custom-package-list '(github-modern-theme elpy company spacemacs-theme))
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package custom-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; loading local settings
(add-to-list 'load-path "~/.emacs.d/local-lisp/")
(load "local-settings.el")

;; color theme -- spacemacs, light or dark based on current time of day
(when (>= emacs-major-version 24)
  (let ((current-hour (string-to-number (substring (current-time-string) 11 13))))
	(if (and (>= current-hour 9) (< current-hour 18))
		(load-theme 'spacemacs-light t)
	  (load-theme 'spacemacs-dark t))))

;; elpy settings
(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-input-method "russian-computer")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
	(spacemacs-theme magit markdown-mode melancholy-theme elpy company github-modern-theme spacemacs-theme)))
 '(safe-local-variable-values (quote ((tab-indent-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

