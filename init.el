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

;; more lenient garbage collection
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 64))  ; set GC threshold to 64Mb -- should be fine

;; loading local settings
(add-to-list 'load-path "~/.emacs.d/local-lisp/")
(load "local-settings.el")

;; packages
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

;; selected packages
(setq common-package-selected-packages
	  '(flymake spacemacs-theme magit markdown-mode elpy company org-bullets))
(setq package-selected-packages (append common-package-selected-packages local-selected-packages))
(package-install-selected-packages)

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

;; custom packages
(add-to-list 'load-path "~/.emacs.d/custom-pkgs")
(load "better-pyvenv-activate.el")

;; color theme -- spacemacs, light or dark based on current time of day
(when (>= emacs-major-version 24)
  (let ((current-hour (string-to-number (substring (current-time-string) 11 13))))
	(if (and (>= current-hour 9) (< current-hour 18))
		(load-theme 'spacemacs-light t)
	  (load-theme 'spacemacs-dark t))))

;; elpy settings
(elpy-enable)
(let ((python-interpreter "ipython")
	  (python-args "-i --simple-prompt"))
  (setq python-shell-interpreter python-interpreter
		python-shell-interpreter-args python-args
		python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
			   python-interpreter))

;; org-mode settings
(setq org-log-done t)
(setq org-todo-keywords
	  '((sequence "TODO" "|" "DONE")
		(sequence "|" "CANCELLED")))
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook
		  (lambda ()
			(visual-line-mode)
			(org-bullets-mode)))

;; dired settings
(setq dired-listing-switches "-alh")

;; special keys
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c e") 'better-pyvenv-activate)

;; some Windows-specific options that are not local
(when (memq system-type '(windows-nt ms-dos))
  ;; tramp for windows
  (setq tramp-default-method "plink")
  ;; fonts
  (set-face-attribute 'default nil
					  :font "Source Code Pro")
  ;; git ask password in gui (for windows)
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  ;; ido mode -- selectrum doesn't work in windows
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  ;; coding
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
