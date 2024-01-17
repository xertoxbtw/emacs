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
  :bind (("<escape>" . keyboard-escape-quit)
		 ("<f5>" . make-run)
		 ("<f6>" . make))
  :init
  (setq evil-want-keybinding nil) 
  (setq evil-undo-system 'undo-fu)
  (define-key evil-normal-state-map (kbd ".") 'nil)
  :config (evil-mode 1))

;; Which Key
(use-package which-key 
  :ensure t 
  :config (which-key-mode 1))

;; All the icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :bind (:map corfu-map
			  ("M-SPC" . corfu-insert-separator)
			  ("TAB"     . corfu-next)
			  ([tab]     . corfu-next)
			  ("S-TAB"   . corfu-previous)
			  ([backtab] . corfu-previous)
			  ("C-<return>" . corfu-insert)
			  ("RET"     . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
  (add-hook 'eshell-mode-hook
			(lambda () (setq-local corfu-quit-at-boundary t
							  corfu-quit-no-match t
							  corfu-auto nil)
			  (corfu-mode))))

;; Eglot
(use-package eglot
  :ensure t
  ;; :hook (c-mode . eglot-ensure)
  :hook (c++-mode . eglot-ensure)
  :hook (python-mode . eglot-ensure)
  :hook (javascript-mode . eglot-ensure))
;; :bind ("M-f" . eglot-format-buffer))

;; Clang-Format
(use-package clang-format+
  :ensure t
  :bind ("M-f" . clang-format-buffer))

;; Sly
(use-package sly
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :bind (:map evil-normal-state-map
			  ("C-<return>" . common-lisp-hyperspec)))
 
;; Magit
(use-package magit
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

;; adwaita-dark-theme
(use-package adwaita-dark-theme
  :ensure t
  :config (load-theme 'adwaita-dark t))

;; mood-line
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode))

;; swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; switch-window
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
		'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

;; glsl-mode
(use-package glsl-mode
  :ensure t
  :config
  (progn
	(add-to-list 'auto-mode-alist '("\\.vertex\\'" . glsl-mode))
	(add-to-list 'auto-mode-alist '("\\.fragment\\'" . glsl-mode))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
	(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

(use-package elcord
  :ensure t
  :config
  (elcord-mode))
