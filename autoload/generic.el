;; Visual
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq warning-minimum-level :emergency)
(setq visible-bell 1)
(setq use-dialog-box nil)


;; Aliases
(defalias 'open 'find-file)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Modes
(electric-pair-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default c-basic-offset 4)

;; Misc
(setq make-backup-files nil)
(setq-default tab-width 4)
(setq initial-scratch-message "")

(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
(setq inhibit-startup-buffer-menu t)
(add-hook 'window-setup-hook 'delete-other-windows)

(global-set-key (kbd "<Hangul_Hanja>") 'ignore)
(global-set-key (kbd "C-<Hangul_Hanja>") 'ignore)
(global-set-key (kbd "M-<Hangul_Hanja>") 'ignore)

(prefer-coding-system 'utf-8-unix)
