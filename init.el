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

  (use-package exec-path-from-shell
    :straight t
    :init
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

(setq package-archives ;; if packages fail to download M-x package-refresh-contents
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq frame-resize-pixelwise t)
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

(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)

  (general-unbind 'normal dired-mode-map "SPC")

  (general-create-definer leader-key-def
    ;; :keymaps '(normal insert visual emacs)
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(add-hook 'dired-mode-hook (lambda () dired-hide-details-mode))

(use-package all-the-icons :straight t)

(use-package dirvish
   :custom
(dirvish-quick-access-entries ; It's a custom option, `setq' won't work
 '(("h" "~/"                          "Home")
   ("d" "~/Downloads/"                "Downloads")
   ("D" "~/Developer/"                "Developer")
   ))
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq insert-directory-program "gls" dired-use-ls-dired t)     ;; needs coreutils: 'brew install coreutils'
  (setq dired-listing-switches "-al --group-directories-first")  ;; needs coreutils: 'brew install coreutils' 
  :init
  (dirvish-override-dired-mode))

(use-package magit
  :ensure t
  :general
  (setq magit-status-buffer-switch-function 'switch-to-buffer))
(add-hook 'git-commit-mode-hook 'evil-insert-state)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode) (org-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(defun ramon/template-insert-gitignore()
  (interactive)
  (let* ((dir (concat "~/.emacs.d/" "templates/gitignore/"))
	 (files (directory-files dir nil ".*\\.gitignore"))
	 (pick (yas-choose-value (mapcar #'file-name-sans-extension files))))
    (insert-file-contents (concat dir (concat pick ".gitignore")))))

(global-set-key (kbd "M-/") 'comment-line)
(electric-pair-mode)

(use-package undo-fu)

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

(use-package evil-snipe
  :straight t
  :init
  (evil-snipe-mode))

(use-package vertico
  :config
  (vertico-mode)
  :bind (:map vertico-map
	      ("DEL" . #'vertico-directory-delete-char)))

(use-package consult)

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

(use-package which-key
  :config
  (setq which-key-idle-delay 1))
(which-key-mode)

(use-package org 
  :defer t
  :config
  (setq
   org-image-actual-width 400
   org-agenda-skip-scheduled-if-done t ;; for setting todo priority colors
   org-priority-faces '((65 :foreground "#FF0000")
                        (66 :foreground "#0098dd")
                        (67 :foreground "#da8548")))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . toggle-truncate-lines)
  (org-mode . flyspell-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-org-mode
  :straight (evil-org-mode :type git :host github :repo "hlissner/evil-org-mode")
  :hook ((org-mode . evil-org-mode)
         (org-mode . (lambda () 
                       (require 'evil-org)
                       (evil-normalize-keymaps)
                       (evil-org-set-key-theme '(textobjects))
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))
  :bind
  ([remap evil-org-org-insert-heading-respect-content-below] . +org/insert-item-below) ;; "<C-return>" 
  ([remap evil-org-org-insert-todo-heading-respect-content-below] . +org/insert-item-above) ;; "<C-S-return>" 
  :general
  (general-nmap
    :keymaps 'org-mode-map
    :states 'normal
    "RET"   #'org-open-at-point))

;; (setq org-agenda-files (apply 'append ;; Fix this, ethan said setqs go under custom
;;                              (mapcar
;;                               (lambda (directory)
;;                                 (directory-files-recursively
;;                                  directory org-agenda-file-regexp))
;;                               '("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd")
;;                               )))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)"))))

(setq org-capture-templates
      '(
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
             :refile-targets (("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/gtd/work.org" :maxlevel . 2)))))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output"))

(use-package ox-pandoc
  :straight t)

(use-package ox-reveal)

(use-package org-download
  :straight t
  :init
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(defun org-babel-edit-prep:python (babel-info) 
  ;; to add more language support, see:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2842#issuecomment-870807018
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(defun org-babel-edit-prep:rust (babel-info) 
  ;; to add more language support, see:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2842#issuecomment-870807018
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(use-package toc-org
  :straight t
  :config
  (if (require 'toc-org nil t)
      (progn
        (add-hook 'org-mode-hook 'toc-org-mode)
        ;; (add-hook 'markdown-mode-hook 'toc-org-mode)
        ;; (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
        )
    (warn "toc-org not found")))

(use-package org-fancy-priorities
  :straight t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MEDIUM" "LOW"))
  org-todo-keywords '((sequence "HW")))

(use-package org-modern
  :straight t
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-ellipsis "  ⬎ "
   org-modern-table nil
   org-hide-emphasis-markers t
   org-startup-folded 'show2levels
   org-list-demote-modify-bullet
   '(("+" . "*") ("*" . "-") ("-" . "+"))

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))

(use-package haskell-mode
  :straight t)

(use-package python)
(use-package pyvenv
  :config
  (pyvenv-mode 1)) ;; TODO Have it so that it automatically restarts the lsp session on venv activation
(use-package numpydoc :straight t)

(use-package rustic ;; remember to: 'brew install rust-analyzer'
  :straight t
  :config
  (setq rustic-cargo-bin "~/.cargo/bin/cargo"))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package company ;; TODO add tab completion
  :straight t
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.01)
  :init
  (global-company-mode)
  (global-set-key (kbd "TAB") 'company-indent-or-complete-common))

;; (use-package corfu
;;   :straight t
;;   :defer t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-prefix 3)
;;   (corfu-auto-delay 0.0)           ; Enable auto completion
;;   (corfu-quit-at-boundary 'separator)
;;   (corfu-echo-documentation 0.25)   ; Enable auto completion
;;   (corfu-preview-current 'insert)   ; Do not preview current candidate
;;   ;; :init
;;   ;; (global-corfu-mode)
;;   :hook
;;   (prog-mode . corfu-mode))

;; (use-package eglot
;;   :ensure t
;;   :defer t
;;   :hook
;;   (python-mode . eglot-ensure)
;;   (rust-mode . eglot-ensure)
;;   )

(use-package lsp-mode
  :straight t
  :config
  (setq lsp-headerline-breadcrumb-mode nil)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package origami
  :defer t
  :hook (prog-mode . origami-mode))

(use-package projectile ;; remmeber, `brew install ripgrep`
  :straight t
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :init (projectile-mode))

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
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package hl-todo ;; This package caused all my headaches >.<
;;   :straight t
;;   :config
;;   (setq hl-todo-keyword-faces
;; 	'(("TODO"   . "#FF69B4") 
;; 	  ("FIXME"  . "#ea3d54") 
;; 	  ("NOTE"  . "#93C572") 
;; 	  ("REVIEW" . "#A7C7E7")
;; 	  ))
;;   :hook (prog-mode . (hl-todo-mode)))

(use-package beacon
  :straight t
  :init
  (beacon-mode))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Don't be a weenie")
  (setq dashboard-items '((agenda . 15))))

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq auto-revert-check-vc-info t)
  (setq doom-modeline-buffer-encoding nil
	doom-modeline-enable-word-count nil
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t))

(use-package winum :straight t :init (winum-mode))
(winner-mode 1)

(general-imap 
  :keymaps 'vterm-mode-map
  "C-c" 'vterm-send-C-c)

(general-unbind 'normal dired-mode-map
  :with 'ignore
  [dired-next-line])

(general-define-key
 :states 'normal
 "RET" 'push-button)

(leader-key-def ;; NOTE/REVIEW Link to gist to show leader-key-cleanup: https://gist.github.com/thriveth/1cfb03fac55c3076a34f9627bc3c6a63
  "." 'find-file
  "," 'switch-to-buffer

  "RET" '(consult-bookmark :which-key "bookmarks")
  "TAB" 'dirvish-subtree-toggle

  "b" '(:ignore t :which-key "Buffer")
  "b b " 'switch-to-buffer
  "b k" 'image-kill-buffer
  "b r" '(revert-buffer :which-key "refresh-buffer")

  "e" '(:ignore t :which-key "Evil")
  "e f" 'evil-toggle-fold

  "f" '(:ignore t :which-key "Find")
  "f f" 'find-file

  "g" '(:ignore t :which-key "Git")
  "g g" 'magit-status
  "g i" '(ramon/template-insert-gitignore :which-key "insert-gitignore-template")

  "h" '(:ignore t :which-key "Help")
  "h f" 'helpful-callable
  "h v" 'helpful-variable
  "h k" 'helpful-key
  "h t" 'consult-theme
  "h m" 'describe-mode
  "h r r" 'eval-defun

  "l" '(:ignore t :which-key "LSP")
  "l a" '(lsp :which-key "activate lsp")
  "l r" '(lsp-rename :which-key "rename variable")

  "m" '(:ignore t :which-key "Prog Mode")
  "m p" '(:ignore t :which-key "Python")
  "m p c" 'pyvenv-create
  "m p a" 'pyvenv-activate
  "m p k" 'pyvenv-deactivate
  "m p m" 'pyvenv-menu
  "m p d" 'numpydoc-generate
  "m r" '(:ignore t :which-key "Rust")
  "m r r" 'rustic-cargo-run
  "m r b" 'rustic-cargo-build
  "m r c" 'rustic-cargo-check
  "m r C" 'rustic-cargo-clippy

  "o" '(:ignore t :which-key "Org")
  "o a" 'org-agenda
  "o c" 'org-capture
  "o w" '(flyspell-correct-wrapper :which-key "correct-word")

  "o i" '(:ignore t :which-key "Insert")
  "o i l" 'org-insert-link
  "o i t" 'org-table-create
  "o i s" 'org-insert-structure-template

  "o t" '(:ignore t :which-key "Toggle")
  "o t c" 'org-toggle-checkbox

  "p" '(:ignore t :which-key "Projectile")
  "p p" 'projectile-find-file
  "p g" '(projectile-ripgrep :which-key "grep-project")

  "t" '(:ignore t :which-key "Toggle")
  "t t" 'vterm-toggle

  "w" '(:ignore t :which-key "Window")
  "w c" 'evil-window-delete
  "w v" 'evil-window-vsplit
  "w u" 'winner-undo
  "w n" 'evil-window-new
)

;; FIXME: Figure out how to embed this into the keymapping
(setq winum-keymap 
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (leader-key-def "w 0" 'winum-select-window-0-or-10)
      (leader-key-def "w 1" 'winum-select-window-1)
      (leader-key-def "w 2" 'winum-select-window-2)
      (leader-key-def "w 3" 'winum-select-window-3)
      (leader-key-def "w 4" 'winum-select-window-4)
      (leader-key-def "w 5" 'winum-select-window-5)
      (leader-key-def "w 6" 'winum-select-window-6)
      (leader-key-def "w 7" 'winum-select-window-7)
      (leader-key-def "w 8" 'winum-select-window-8)
      map))

(setq max-lisp-eval-depth 10000)  ;; Debugging 
(setq debug-on-error t)           ;; Debugging 

(eldoc-mode -1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1) 
(setq scroll-conservatively 101)
(setq use-dialog-box nil)
(setq make-backup-files nil)

(defun ramon/call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
  Interactively, prompt for a command to execute.

  Return a list of the hooks run, in the order they were run.
  Interactively, or with optional argument VERBOSE, also print a
  message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (let* ((log     nil)
         (logger (lambda (&rest hooks) 
                   (setq log (append log hooks nil)))))
    (ramon/with-advice
        ((#'run-hooks :before logger))
      (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))

(defmacro ramon/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))
