(defun make ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "Makefile"))
    (compile "make -k"))))

(defun make-run ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "Makefile"))
    (compile "make -k run"))))

(defun new ()
  (interactive)
  (let ((buffer (generate-new-buffer "<Unsaved File>")))
    (switch-to-buffer buffer)
    (setq-local is-new-file-buffer t)))

