;; my emacs customizing

(menu-bar-mode 1)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)

;;linum mode and highliting of current line
(global-linum-mode 1)
(global-hl-line-mode 1)

;;printing settings
(setq printer-name "Deskjet-F2200-series")
(global-set-key [?\C-c ?\C-p] 'print-buffer)

;;coding style
(setq-default c-default-style "ellemtel"
      c-basic-offset 4
      tab-width 4
      tab-indent-mode t)
(defvaralias 'c-basic-offset 'tab-width)


;;hook for c++ mode
(defun my-c++-mode-hook ()
  (define-key c++-mode-map [?\C-c ?\C-c] 'compile)
  (define-key c++-mode-map [?\C-c d]   'gdb)
  (c-set-offset 'access-label '0)
  (c-set-offset 'inclass '+)
  (linum-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;;treat .h files as c++ 
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;;special keys
(global-set-key "\C-cl" 'goto-line)

;;auto-complete-mode
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)








