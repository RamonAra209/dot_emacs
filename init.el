;; git clone --depth 1 git@github.com:RamonAra209/dot_emacs.git ~/.emacs.d

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)


;; Custom File
(setq custom-file "~/emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))


;; Mac-Specific Settings
(when (eq system-type 'darwin) 
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))

(use-package default-text-scale
  :straight t
  :config
  (global-set-key (kbd "M-=") 'default-text-scale-increase)
  (global-set-key (kbd "M--") 'default-text-scale-decrease))

(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; General 
(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)

  (general-unbind 'normal dired-mode-map "SPC")

  (general-create-definer leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))


;; Package Setup
(setq package-archives ;; if packages fail to download M-x package-refresh-contents
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


;; Visual
(setq display-line-numbers-type 'visual)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package beacon
  :straight t
  :init
  (beacon-mode))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Go change the world, one line at a time")
  )

;;; Key Bindings 
(global-set-key (kbd "M-/") 'comment-line)
(electric-pair-mode)

(use-package undo-fu)

(use-package undo-fu-session :straight t :init (global-undo-fu-session-mode))

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-set-leader '(normal visual) (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (setq evil-respect-visual-line-mode t)
  :init
  (evil-collection-init))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit which-key helpful orderless marginalia consult winum vertico evil-collection evil undo-fu use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package evil-multiedit)
(evil-multiedit-default-keybinds)

(use-package evil-goggles
  :straight t
  :custom
  (setq evil-goggles-duration 0.150)
  (setq evil-goggles-enable-change t)
  (setq evil-goggles-enable-delete t)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-snipe
  :straight t
  :init
  (evil-snipe-mode))


;; Minibuffer
(use-package vertico
  :config
  (vertico-mode)
  :bind (:map vertico-map
	      ("DEL" . #'vertico-directory-delete-char)
	      ;; ("DEL" . #'vertico-directory-delete-word)
  ))

(use-package consult)
(leader-key-def "h t" 'consult-theme)

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist 
  :init
  (savehist-mode))

(use-package helpful)
(leader-key-def "h f" 'helpful-callable)
(leader-key-def "h v" 'helpful-variable)
(leader-key-def "h k" 'helpful-key)

(use-package which-key)
(which-key-mode)


;; Dired + Buffer
;; (add-hook 'dired-mode-hook (lambda () (local-unset-key (kbd "SPC"))))
(leader-key-def "f f" 'find-file)
(leader-key-def "." 'find-file)
(leader-key-def "," 'switch-to-buffer)
(leader-key-def "b k" 'image-kill-buffer)
(leader-key-def "RET" 'consult-bookmark)
(use-package all-the-icons :straight t)

;; Git
(use-package magit
  :ensure t
  :general
  (leader-key-def "g g" 'magit-status)
  (setq magit-status-buffer-switch-function 'switch-to-buffer))

;; (use-package diff-hl
;;   :config
;;   (global-diff-hl-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


;; Completion
;; (setq python-shell-exec-path "/opt/homebrew/bin/python3.11")

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preview-current 'insert)   ; Do not preview current candidate
  :init
  (global-corfu-mode))

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright ;; remember to: 'brew install pyright'
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package pyvenv)

(use-package rustic ;; remember to: 'brew install rust-analyzer'
  :straight t
  :config
  (setq rustic-cargo-bin "~/.cargo/bin/cargo")
  )

;; (use-package lsp-ui
;;   :straight t
;;   :config
;;   (setq lsp-ui-sideline-enable t))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))


;; Theme
(use-package ef-themes
  :straight t)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t) 
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom") 
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(load-theme 'doom-old-hope t)
(set-face-foreground 'line-number "#708090")
(set-face-foreground 'line-number-current-line "#ef7c2b")


;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq auto-revert-check-vc-info t)
  (setq doom-modeline-buffer-encoding nil
	doom-modeline-enable-word-count nil
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t))


;; Window Management
(leader-key-def "w c" 'evil-window-delete)
(leader-key-def "w v" 'evil-window-vsplit)
(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
(use-package winum :straight t :init (winum-mode))

;; Projectile
(use-package projectile
  :straight t
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode)
  (leader-key-def "SPC" 'projectile-find-file))


;; Terminal
(use-package vterm
  :straight t)

(use-package vterm-toggle
  :straight t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
	       '((lambda (buffer-or-name _)
		   (let ((buffer (get-buffer buffer-or-name)))
		     (with-current-buffer buffer
		       (or (equal major-mode 'vterm-mode)
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 ;;(display-buffer-reuse-window display-buffer-in-direction)
		 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
		 ;;(direction . bottom)
		 ;;(dedicated . t) ;dedicated is supported in emacs27
		 (reusable-frames . visible)
		 (window-height . 0.3)))

  (leader-key-def "o t" 'vterm-toggle)
)


;; Org Mode
(setq org-agenda-files (apply 'append ;; Fix this, ethan said setqs go under custom
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd")
			       )))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)"))))

(leader-key-def "X" 'org-capture)
(setq org-capture-templates
      '(
        ;; ("t" "General Todo" entry (file+headline "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org/gtd.org" "Tasks")
        ;;  "* TODO %?\n  %i\n  %a")
        ("t" "General Todo")
            ("te" "No Time" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org" :maxlevel . 2)))

            ("ts" "Scheduled" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org" :maxlevel . 2)))

            ("td" "Deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org" :maxlevel . 2)))

            ("tw" "Scheduled & deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/gtd.org" :maxlevel . 2)))
        ("j" "Journal" entry (file+datetree "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Work Todo Entries")
            ("we" "No Time" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org" :maxlevel . 2)))

            ("ws" "Scheduled" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org" :maxlevel . 2)))

            ("wd" "Deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org" :maxlevel . 2)))

            ("ww" "Scheduled & deadline" entry (file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org")
             "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org" :maxlevel . 2)))
            )
)

(use-package org
  :defer t
  :config
  (leader-key-def "o a" 'org-agenda)
  (setq org-agenda-skip-scheduled-if-done t ;; for setting todo priority colors
	org-priority-faces '((65 :foreground "#FF0000")
			     (66 :foreground "#0098dd")
			     (67 :foreground "#da8548")))

  (setq org-ellipsis "  ⬎ ")
  (setq org-startup-folded 'show2levels)
  (setq org-hide-emphasis-markers t)
  (setq org-list-demote-modify-bullet
	'(("+" . "*") ("*" . "-") ("-" . "+")))

  (use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package org-download
    :straight t
    :init
    (add-hook 'dired-mode-hook 'org-download-enable))

  (use-package toc-org
    :straight t
    :config
    (if (require 'toc-org nil t)
	(progn
	  (add-hook 'org-mode-hook 'toc-org-mode)

	  ;; enable in markdown, too
	  (add-hook 'markdown-mode-hook 'toc-org-mode)
	  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))
      (warn "toc-org not found")))

  (use-package org-bullets
    :straight t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (use-package org-fancy-priorities
    :straight t
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("HIGH" "MEDIUM" "LOW"))
    org-todo-keywords '((sequence "HW")))

  :hook
  (org-mode . org-indent-mode)
  )


;; Misc
(eldoc-mode -1)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1) 


;; Key Maps
(leader-key-def
 :keymaps 'rustic-mode-map
 (leader-key-def "m b r" 'rustic-cargo-run)
 (leader-key-def "m b b" 'rustic-cargo-build)
 (leader-key-def "m b c" 'rustic-cargo-check)
 (leader-key-def "m b C" 'rustic-cargo-clippy))

(leader-key-def
 :keymaps 'python-mode-map
 (leader-key-def "m v c" 'pyvenv-create)
 (leader-key-def "m v a" 'pyvenv-activate)
 (leader-key-def "m v d" 'pyvenv-deactivate)
 (leader-key-def "m v m" 'pyvenv-menu))

(general-imap
  :keymaps 'vterm-mode-map
  "C-c" 'vterm-send-C-c)
