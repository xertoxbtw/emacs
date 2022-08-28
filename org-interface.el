;;; org-interface.el

(if (fboundp 'read-key)
    (defalias 'selection-menu--read-key 'read-key)
  (defalias 'selection-menu--read-key
    (lambda (msg) (aref (read-key-sequence-vector msg) 0))))
(defun selection-menu--select (ident &optional unread)
  (let ((helpmsg "Type ESC to abort, Space or Enter to select.")
        (buffer-read-only t)
        first last overlay pevent select)
    (forward-line -1)
    (setq last (point))
    (goto-char (point-min))
    (setq first (1+ (point)))
    (save-window-excursion
      (pop-to-buffer (current-buffer))
      (setq mode-name "Selection Menu"
            mode-line-buffer-identification (concat "*" ident "*"))
      (setq overlay (make-overlay (point) (line-end-position)))
      (overlay-put overlay 'face 'highlight)
      (while
          (let ((event (selection-menu--read-key helpmsg)))
            (cond ((or (eq event 'up) (eq event 16))
                   (when (> (point) first)
                     (forward-line -1)
                     (move-overlay overlay (point) (line-end-position)))
                   t)
                  ((or (eq event 'down) (eq event 14))
                   (when (< (point) last)
                     (forward-line)
                     (move-overlay overlay (point) (line-end-position)))
                   t)
                  ((or (eq event 32) (eq event 13) (eq event 'return))
                   (setq select
                         (buffer-substring (1+ (point))
                                           (line-end-position)))
                   nil)
                  ((eq event 'escape)
                   nil)
                  (t (setq pevent event)
                     nil)
                  ))))
    (if (and unread pevent)
        (push pevent unread-command-events))
    (message nil)
    select))

(defun selection-menu (ident items &optional unread)
  (if (eq (length items) 0) nil
    (save-excursion
      (with-temp-buffer
        (dolist (item items) (insert " " item "\n"))
        (selection-menu--select ident unread)))))

(defun org-interface (&optional path)
  (interactive)
  (let* ((path (if (null path) (car org-agenda-files) path))
		 (files (directory-files path nil "^[^.]\\w+"))
		 (selected (selection-menu "" files)))
	(if (null selected) nil
	  (let ((fullpath (concat path "/" selected)))
		(if (file-directory-p fullpath)
			(org-interface fullpath)
		  (find-file fullpath))))))

(provide 'org-interface)
