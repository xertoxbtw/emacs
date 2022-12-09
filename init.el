;; Startup
(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
						 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))
(eval-when-compile 
  (require 'use-package))

;; Auto Update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
		auto-package-update-interval 4)
  (auto-package-update-maybe))

;; Evil mode
(use-package undo-fu
  :ensure t)
(use-package evil 
  :ensure t 
  :demand t 
  :bind (("<escape>" . keyboard-escape-quit))
  :init (setq evil-want-keybinding nil) 
  (setq evil-undo-system 'undo-fu) 
  :config (evil-mode 1))

;; Which Key
(use-package which-key 
  :ensure t 
  :config (which-key-mode 1))

;; All the icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard 
  :ensure t 
  :config (dashboard-setup-startup-hook) 
  (setq dashboard-banner-logo-title "GNU Emacs")
  ;;(setq dashboard-startup-banner "~/.config/emacs/banner.png")
  (setq dashboard-center-content t) 
  (setq dashboard-show-shortcuts nil) 
  (setq dashboard-items '()))

;; Slime
(use-package sly 
  :ensure t 
  :config (setq inferior-lisp-program "sbcl"))

;; Org Mode
(setq org-hide-emphasis-markers t)
(use-package 
  org-bullets 
  :ensure t 
  :config (add-hook 'org-mode-hook 
					(lambda () 
					  (org-bullets-mode 1))))
(use-package orgtbl-aggregate
  :ensure t)


;; Switch Window
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-input-style 'minibuffer))

;; Magit
(use-package magit
  :ensure t)

;; Web-Mode
(use-package web-mode
  :ensure t
  :hook ((html-mode . web-mode)))

;; Auctex
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; Eglot
(use-package eglot
  :ensure t
  :bind (("M-f" . eglot-format-buffer)))

;; Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 2)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preview-current 'insert)   ; Do not preview current candidate
  (corfu-preselect-first nil)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
			  ("M-SPC" . corfu-insert-separator)
			  ("TAB"     . corfu-next)
			  ([tab]     . corfu-next)
			  ("S-TAB"   . corfu-previous)
			  ([backtab] . corfu-previous)
			  ("S-<return>" . corfu-insert)
			  ("RET"     . nil) ;; leave my enter alone!
			  )

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
  (add-hook 'eshell-mode-hook
			(lambda () (setq-local corfu-quit-at-boundary t
							  corfu-quit-no-match t
							  corfu-auto nil)
			  (corfu-mode))))


;; Org Interface
(load-file "~/.config/emacs/org-tools.el")

;; Base Configuration
(add-to-list 'auto-mode-alist
			 '("\\.php\\'" . (lambda ()
							   (web-mode))))

(load-theme 'dichromacy t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq scroll-conservatively 100)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method nil)
(setq use-dialog-box nil)
(define-key evil-normal-state-map (kbd "u") nil)
(setq tab-always-indent 'complete)
(setq electric-pair-pairs '((?\{ . ?\}) 
							(?\( . ?\)) 
							(?\[ . ?\]) 
							(?\" . ?\")))
(electric-pair-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(defalias 'open 'find-file)
(defalias 'clean 'eshell/clear-scrollback)
(global-prettify-symbols-mode +1)
(add-hook 'c-mode-hook 'eglot-ensure)
(setq org-agenda-files '("~/David/org"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes '("org-plain-latex" "\\documentclass{article}
		   [NO-DEFAULT-PACKAGES]
		   [PACKAGES]
		   [EXTRA]" ("\\section{%s}" . "\\section*{%s}") 
		   ("\\subsection{%s}" . "\\subsection*{%s}") 
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}") 
		   ("\\paragraph{%s}" . "\\paragraph*{%s}") 
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-listings 't)
(setq org-latex-toc-command "\\tableofcontents \\clearpage")
(org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))

(add-hook 'org-mode-hook 'org-language)

;; Custom Functions
(defun new () 
  (interactive) 
  (let ((buffer (generate-new-buffer "<Unsaved File>"))) 
	(switch-to-buffer buffer) 
	(setq-local is-new-file-buffer t)))

