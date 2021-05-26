(defvar better-pyvenv-env-directory "~/mininconda3/envs"
  "A root directory for conda/pyvenv environments")

(defun better-pyvenv-filter-subdirs ()
  "Read root directory and filter only subdirectories"
  (mapcar 'file-name-nondirectory
		  (seq-filter
		   (lambda (p) (and (file-directory-p p)
							(not (seq-contains '("." "..")
											   (file-name-nondirectory p)))))
		   (directory-files better-pyvenv-env-directory t))))

(defun better-pyvenv-activate ()
  "A better pyvenv-activate that requires to enter only environment name
   (as opposed to environment directory)"
  (interactive)
  (let* ((available-envs (better-pyvenv-filter-subdirs))
		 (env-name (completing-read "Env name: " available-envs))
		 (env-path (mapconcat 'identity `(,better-pyvenv-env-directory ,env-name) "/")))
	(pyvenv-activate (expand-file-name env-path))))
