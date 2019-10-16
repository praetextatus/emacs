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

;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq custom-package-list '(github-modern-theme elpy company))
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package custom-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; color theme -- github-modern
(when (>= emacs-major-version 24)
  (load-theme 'github-modern t))

(elpy-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (melancholy-theme elpy company github-modern-theme)))
 '(safe-local-variable-values (quote ((tab-indent-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

