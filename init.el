(require 'package)
(setq package-enable-at-startup nil)

;; Setup package sources
(setq package-archives '(("elpa"   . "http://elpa.gnu.org/packages/")
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

;; Makes escape behave like C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; (use-package general
;;   :config
;;   (general-create-definer cjv/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (cjv/leader-keys
;;    "t"  '(:ignore t :which-key "toggles")
;;    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

;; (cjv/leader-keys
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Disable default init screen
(setq inhibit-startup-message t)

;; Enable visible bell
(setq visible-bell t)

(scroll-bar-mode -1) ; Disable scrollbar
(menu-bar-mode -1)   ; Disable menu bar
(tool-bar-mode -1)   ; Disable tool bar
(tooltip-mode -1)    ; Disable tooltips

(set-fringe-mode 10) ; Set some spacing between windows

;; Enable column numbers on modeline
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                pdf-view-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar cjv/default-font-size 160)

(set-face-attribute 'default nil :font "Fira Code" :height cjv/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height cjv/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height cjv/default-font-size :weight 'regular);

;; doom-themes has most popular colorschemes
(use-package doom-themes)

;; Sets up solar for use with circadian
(use-package solar
  :ensure nil ; built-in package
  :config
  (setq calendar-latitude 38.72 ; Lisbon coordinates
        calendar-longitude -9.14))

;; Sets a light theme during the day and a dark theme during the night
(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset  . doom-one)))
  (circadian-setup))

;; Automatically applies emacs's theme to the rest of the system
;; Requires pywal
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; The first time the configuration is loaded, run:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

(use-package vterm)

(use-package format-all
  :config
  (format-all-mode))

(defun cjv/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (visual-line-mode 1))

(defun cjv/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; Use describe-face to find more
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun cjv/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . cjv/org-mode-visual-fill))

(use-package org
  :hook (org-mode . cjv/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; org-agenda settings
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/org/Tasks.org"
          "~/org/Habits.org"
          "~/org/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refilling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Setup tags
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  ;; Setup org-capture
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/Tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
       (file+olp+datetree "~/org/Journal.org")
       "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
       ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
       :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
       (file+olp+datetree "~/org/Journal.org")
       "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
       :clock-in :clock-resume
       :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/Journal.org")
       "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/org/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  ;; Example on how to define a global keybinding for a particular capture
  ;; However, it's probably better to just setup a binding for org-capture
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  ;; org-hide-emphasis-markers t ; Disables the display of markup symbols like * and _ for formatting.
  (cjv/org-font-setup))

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
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate-nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cjv/org-babel-tangle-config)))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/org-roam/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package pdf-tools
  :config
  (pdf-loader-install))

(defun cjv/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)

  (add-hook 'exwm-update-class-hook #'cjv/exwm-update-class)
  ;; (add-hook 'exwm-init-hook #'cjv/exwm-init-hook)

  ;(start-process-shell-command "unclutter" nil "unclutter")

  ;; Launch apps that will run in the background
  ;;(cjv/run-in-background "nm-applet"))

  ;; Setup muti-head setup
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output DVI-1 --primary --mode 1920x1080 --rate 144 --output DVI-0 --mode 1920x1080 --right-of DVI-1")

  (setq exwm-randr-workspace-monitor-plist '(2 "DVI-0" 3 "DVI-0"))
  (setq exwm-workspace-warp-cursor t)

  (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable)

  (display-time)
  ;; (display-battery-mode)
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)

          ([s-left] . windmove-left)
          ([?\s-h] . windmove-left)

          ([s-right] . windmove-right)
          ([?\s-l] . windmove-right)

          ([s-up] . windmove-up)
          ([?\s-k] . windmove-up)

          ([s-down] . windmove-down)
          ([?\s-j] . windmove-down)

          ([?\s-.] . counsel-find-file)
          ([?\s-,] . counsel-switch-buffer)

          ;; ([?\s->] . ) depois separar entre dentro do workspace e fora do workspace
          ;; ([?\s-<] . )

          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; ([?\s-p] . dmenu)

          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive)
                       (exwm-workspace-switch-create 0)))


          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))
;; EXWM improvements
;; (defun cjv/run-in-background (command)
;;   (let ((command-parts (split-string command "[ ]+")))
;;     (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun cjv/exwm-init-hook ()
  ;; Make workspace 1 be the one we land at startup
  (exwm-workspace-switch-create 1))
