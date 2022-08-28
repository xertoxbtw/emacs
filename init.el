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

;; Evil mode
(use-package 
  undo-fu)
(use-package 
  evil 
  :ensure t 
  :demand t 
  :bind (("<escape>" . keyboard-escape-quit)) 
  :init (setq evil-want-keybinding nil) 
  (setq evil-undo-system 'undo-fu) 
  :config (evil-mode 1))

;; Which Key
(use-package 
  which-key 
  :ensure t 
  :config (which-key-mode 1))

;; All the icons
(use-package 
  all-the-icons 
  :if (display-graphic-p))

;; Geiser
(use-package 
  geiser 
  :ensure t)
(use-package 
  geiser-guile 
  :ensure t)

;; Dashboard
(use-package 
  dashboard 
  :ensure t 
  :config (dashboard-setup-startup-hook) 
  (setq dashboard-banner-logo-title "GNU Emacs") 
  (setq dashboard-startup-banner "~/.config/emacs/banner.png") 
  (setq dashboard-center-content t) 
  (setq dashboard-show-shortcuts nil) 
  (setq dashboard-items '()))

;; Slime
(use-package 
  slime 
  :ensure t 
  :config (setq inferior-lisp-program "sbcl"))

;; Clang Format
(use-package 
  clang-format 
  :ensure t 
  :bind (("M-f" . clang-format-buffer)))

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

;; Eglot
(use-package 
  eglot 
  :ensure t 
  :config (add-hook 'c-mode-hook 'eglot-ensure) 
  (add-hook 'c++-mode-hook 'eglot-ensure))

;; Switch Window
(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-input-style 'minibuffer))

;; Org Interface
(load-file "org-interface.el")

;; Base Configuration
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
(setq backward-delete-char-untabify-method 'nil)
(setq use-dialog-box nil)
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

(org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)))



;; Custom Functions
(defun new () 
  (interactive) 
  (let ((buffer (generate-new-buffer "<Unsaved File>"))) 
	(switch-to-buffer buffer) 
	(setq-local is-new-file-buffer t)))
