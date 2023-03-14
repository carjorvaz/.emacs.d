(require 'package)
(setq package-enable-at-startup nil)

;; Setup package sources
(setq package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it isn't already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure packages are installed
(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-prompt-before-updatex t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

;; Disable default init screen
(setq inhibit-startup-message t)

;; Enable visible bell
(setq visible-bell t)

(scroll-bar-mode -1) ; Disable scrollbar
(menu-bar-mode -1)   ; Disable menu bar
(tool-bar-mode -1)   ; Disable tool bar
(tooltip-mode -1)    ; Disable tooltips

;; Enable column numbers on modeline
(column-number-mode)

(defvar cjv/default-font-size 160)

(set-face-attribute 'default nil :font "monospace" :height cjv/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "monospace" :height cjv/default-font-size)
(set-face-attribute 'variable-pitch nil :font "monospace" :height cjv/default-font-size :weight 'regular);

(use-package ef-themes)

;; Sets up solar for use with circadian
(use-package solar
  :ensure nil ; built-in package
  :config
  (setq calendar-latitude 38.7 ; Lisbon, Portugal coordinates
	calendar-longitude -9.14))

;; Sets a light theme during the day and a dark theme during the night
(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . ef-day)
			   (:sunset  . ef-night)))
  (circadian-setup))

(use-package vertico
  :custom
  (vertico-cycle t) ;; Wrap around after reaching the last completion candidate
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil ;; Built-in package
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package no-littering)

(save-place-mode 1)

(use-package swiper
  :bind
  ("C-s" . 'swiper))

(use-package avy
  :bind
  ("M-s" . avy-goto-char))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Code")
    (setq projectile-project-search-path '("~/Documents/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :custom
  (magit-define-global-key-bindings t)
  :bind
  ("C-c g" . magit-file-dispatch))

(use-package vterm)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package format-all)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package org
  :config
  (require 'org-tempo))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Enable org-babel for the following languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; Enable support for typical unix config files in code blocks
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Disable asking for confirmation when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

;; Automatically tangle Emacs.org when saved
(defun cjv/org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
			  (expand-file-name "~/.emacs.d/Emacs.org"))
	    (string-equal (buffer-file-name)
			  (expand-file-name "~/.config/emacs/Emacs.org")))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate-nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cjv/org-babel-tangle-config)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n t" . org-roam-dailies-find-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)) ;; TODO investigate further
