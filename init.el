;; my emacs customizing

(menu-bar-mode 1)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)

;; Better looks
(global-linum-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

;; fonts
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-12"))


;; printing settings
(setq printer-name "Deskjet-F2200-series")
(global-set-key [?\C-c ?\C-p] 'print-buffer)

;; coding style
(setq-default c-default-style "ellemtel"
      c-basic-offset 4
      tab-width 4
      tab-indent-mode t)
(defvaralias 'c-basic-offset 'tab-width)


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


;; special keys
(global-set-key "\C-cl" 'goto-line)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))


;; color theme -- tango
(when (>= emacs-major-version 24)
  (load-theme 'tango t))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((tab-indent-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
