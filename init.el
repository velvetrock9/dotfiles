;;; init.el --- Universal Emacs configuration for macOS and Linux

;;; Bootstrap Straight.el and Use-Package
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
(setq straight-use-package-by-default t)

;;; Universal OS Configuration
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

(global-set-key (kbd "C-c t t") 'tab-new)
(global-set-key (kbd "C-c t w") 'tab-close)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(cond
 ((eq system-type 'darwin)

  ;; Font
  (set-face-attribute 'default nil :family "Linux Libertine Mono O" :height 140)

  ;; Theme
  (use-package spacemacs-theme               ; pulls from MELPA
    :ensure t
    :init
    ;; Load the light variant immediately after init
    (load-theme 'spacemacs-light t))

  ;; GOPATH
  (setenv "PATH" (concat (getenv "PATH") ":/Users/pavel/go/bin"))
  (add-to-list 'exec-path "/Users/pavel/go/bin")

  ;; PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; List every ENV VAR you need exactly as exported in your shell:
  (setq exec-path-from-shell-variables
        '("GOPATH"                                     ; Go workspace 
          "GOROOT"                                     ; Go root 
          "GOBIN"                                      ; Go binaries 
          "PATH"                                       ; standard executable search path 
          "OPENAI_API_KEY"                             ; your personal API token 
          ))
  ;; Now pull them all in with one shell spawn:
  (exec-path-from-shell-initialize))    

  ;; Disable zoom gestures
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<magnify-up>") 'ignore)
  (global-set-key (kbd "<magnify-down>") 'ignore)
  (global-set-key (kbd "s-<wheel-up>") 'ignore)
  (global-set-key (kbd "s-<wheel-down>") 'ignore)
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>"))
  (global-unset-key (kbd "C-M-<wheel-up>"))
  (global-unset-key (kbd "C-M-<wheel-down>")))
 ((eq system-type 'gnu/linux)

  ;; Disable zoom gestures
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<magnify-up>") 'ignore)
  (global-set-key (kbd "<magnify-down>") 'ignore)
  (global-set-key (kbd "s-<wheel-up>") 'ignore)
  (global-set-key (kbd "s-<wheel-down>") 'ignore)
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>"))
  (global-unset-key (kbd "C-M-<wheel-up>"))
  (global-unset-key (kbd "C-M-<wheel-down>"))
  
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 140)
  (setenv "PATH" (concat (getenv "PATH") ":~/.asdf/shims:/home/archuser/go/bin"))
  (add-to-list 'exec-path "~/.asdf/shims")
  (add-to-list 'exec-path "/home/archuser/go/bin")
  (add-to-list 'exec-path (expand-file-name "~/.opam/cs3110-2025s/bin"))))


;;; Fonts
(when (display-graphic-p)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'unicode "Noto Sans Symbols" nil 'prepend))

;;; Recent Files & UI Tweaks
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 100
      recentf-auto-cleanup 'never)

;; Disable clutter (same as original macOS config)
(setq make-backup-files nil      ;; no backup~ files
      auto-save-default nil      ;; no #autosave# files
      create-lockfiles nil)      ;; no .#lock files

(delete-selection-mode 1)       ;; overwrite selection when typing

;;; UI Enhancements
(use-package which-key
  :config (which-key-mode))

(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  ;; Try fuzzy first, then basic prefix matching
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Per-category overrides:
  ;; • consult-location → substring (for consult-line, consult-buffer, etc.)
  ;; • file             → substring (for consult-recent-file, find-file, etc.)
  (completion-category-overrides
   '((consult-location (styles . (substring)))
     (file             (styles . (substring)))))
  (orderless-matching-styles '(orderless-flex)))

(setq completion-category-overrides
      '((consult-location (styles . (substring)))
        (file             (styles . (substring)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :init
  ;; preview on any selection change, with a small debounce
  (setq consult-preview-key '(:debounce 0.15 any))
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("C-c F" . consult-recent-file)
         ("M-y"   . consult-yank-pop)
         ("C-c p s" . consult-ripgrep)))

(setq use-short-answers t)

;; -----------------------------
;; 2. Git / Magit prefix
;; -----------------------------
(defvar my-magit-map (make-sparse-keymap) "C-c m …")
(define-key global-map (kbd "C-c m") my-magit-map)

(use-package magit
  :bind (:map my-magit-map
              ("s" . magit-status)       ; C-c m s
              ("l" . magit-log-current)  ; C-c m l
              ("b" . magit-blame)        ; C-c m b
              ("c" . magit-commit-create)))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  :config
  (define-key my-magit-map (kbd "n") #'diff-hl-next-hunk) ; C-c m n
  (define-key my-magit-map (kbd "p") #'diff-hl-previous-hunk)
  (define-key my-magit-map (kbd "r") #'diff-hl-revert-hunk))


(use-package ace-window :bind ("C-c o" . ace-window))

;; Allow commands to be called from inside an active minibuffer
(setq enable-recursive-minibuffers t)

;; While the minibuffer is active, let “C-c C-p” do the whole dance:
(defun my/avy-paste-from-other-window ()
  "Use avy to pick text in any visible window, copy it, and insert it here."
  (interactive)
  ;; 1. Pick a region or line with avy…
  (call-interactively #'avy-copy-region) ; or #'avy-copy-line
  ;; 2. …the selection is now at the head of the kill ring → yank it.
  (yank))
(define-key minibuffer-local-map (kbd "C-c C-p") #'my/avy-paste-from-other-window)

(use-package avy
  :ensure t
  :bind
  (("C-c g w" . avy-goto-word-1)    ;; go to word starting with char
   ("C-c g c" . avy-goto-char)      ;; go to char
   ("C-c g 2" . avy-goto-char-2)    ;; go to 2-char sequence
   ("C-c g l" . avy-goto-line)      ;; go to line
   ("C-c g s" . avy-goto-subword-1) ;; go to subword (camelCase etc)
   ("C-c g SPC" . avy-goto-whitespace) ;; go to whitespace
   ("C-c c l" . avy-copy-line) ;; copy selected line
   ("C-c c r" . avy-copy-region))) ;; copy selected region

(when (fboundp 'global-ts-mode) ; Emacs 29+
  (global-ts-mode))

(use-package smart-mode-line
  :ensure t                                  ; grabs smart-mode-line from MELPA 
  :init
  ;; Prevent stupid “Load theme?” prompts
  (setq sml/no-confirm-load-theme t)         ; silence theme load confirmation 
  :config
  ;; Pick a built-in theme: dark, light, or respectful
  (setq sml/theme 'light)                    ; for a light‐themed modeline 
  (require 'smart-mode-line)                 ; load the core package
  (sml/setup))    

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Language: Go
(use-package go-mode
  :hook ((go-mode . lsp-deferred))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom (lsp-completion-provider :none)
  :hook ((go-mode . lsp-deferred)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-peek-fontify 'on-demand)
  ;; Optional keybindings for peek interface
  (define-key lsp-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map (kbd "M-?") 'lsp-ui-peek-find-references))
(global-set-key (kbd "C-c s d") #'lsp-ui-doc-show)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package ansible
  :hook ((yaml-mode . ansible)
         (ansible . lsp-deferred)))   ;; activates lsp only in Ansible files

;; Optional, if you edit Jinja2 templates (`*.j2`):
(use-package jinja2-mode
  :mode "\\.j2\\'")

;; Tell lsp which server to use (it auto-detects by file name):
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(ansible "ansible"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ansible-language-server" "--stdio"))
    :major-modes '(ansible yaml-mode)
    :server-id 'ansible-ls)))

;; (use-package company
;;   :hook (go-mode . company-mode))

;;; --- Corfu + Cape minimal setup -----------------------------

;; 0.  Disable the built-in text-mode ispell completion CAPF.
(setq text-mode-ispell-word-completion nil)

;; 1.  Popup UI (Corfu)
;; TAB-only configuration
(use-package corfu
  :custom
  (corfu-auto t)               ;; Enable auto completion
  (corfu-preselect 'directory) ;; Select the first candidate, except for directories

  :init
  (global-corfu-mode)

  :config
  ;; Free the RET key for less intrusive behavior.
  ;; Option 1: Unbind RET completely
  ;; (keymap-unset corfu-map "RET")
  ;; Option 2: Use RET only in shell modes
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send)))))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;;; ------------------------------------------------------------

(use-package benchmark-init)
(use-package org-download
  :after org
  :config (setq org-download-image-dir "~/org/img"
                org-download-heading-lvl nil))

(use-package gcmh    :init (gcmh-mode 1))
(use-package beacon  :init (beacon-mode 1))
(use-package vundo
  :bind ("C-c u" . vundo)                 ; mnemonic “u”ndo
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(use-package yasnippet
  :config
  ;; Remove ansible’s snippet dir from the search path
  (setq yas-snippet-dirs
        (cl-remove-if
         (lambda (dir)
           (string-match-p "/ansible/snippets" dir))
         yas-snippet-dirs))
  (yas-global-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))


(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

;;; Org Mode
(use-package org
  :straight (:type built-in :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                   :local-repo "org" :depth full
                   :files (:defaults "lisp/*.el" "contrib/lisp/*.el"))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-directory "~/org"
        org-agenda-files '("~/org")))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-bullets :hook (org-mode . org-bullets-mode))
(use-package org-modern  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :custom (org-roam-directory "~/org/roam")
  :config (org-roam-db-autosync-enable))


;;; Terminal
(use-package vterm :commands vterm)
(use-package multi-vterm
  :after vterm
  :bind (("C-c v v" . multi-vterm)
         ("C-c v n" . multi-vterm-next)
         ("C-c v p" . multi-vterm-prev)
	 ("C-c v r" . multi-vterm-rename-buffer)))

(global-hl-line-mode 1)

;;; Project Management
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file     "Find file" ?f)
          (consult-ripgrep       "Ripgrep"   ?r)
          (magit-project-status  "Magit"     ?m)
          (project-eshell        "Eshell"    ?e))))

;; 2. Bind “project find file” to consult-project-file replacing the default project.el binding
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "f") #'consult-project-buffer))

;; 1) Install from MELPA or NonGNU ELPA
(use-package smartparens
  :ensure t                          
  :hook ((prog-mode . smartparens-mode)
	 (yaml-mode . smartparens-mode)
	 (go-mode . smartparens-mode))
  :init
  ;; load the bundled configuration
  (require 'smartparens-config)
  (smartparens-global-mode 1) 
  :config
  ;; turn on strict pairing if you like
  ;; (smartparens-strict-mode 1)
  ;; enable nice delimiter highlighting
  (show-smartparens-global-mode +1)) ;
(define-key smartparens-mode-map (kbd "C-c r a") #'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-c r i") #'sp-change-inner)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Optional: tweak the colors to your taste
  (setq rainbow-delimiters-max-face-count 6))

;;; Visual Tools
(use-package tab-bar :config (tab-bar-mode 1))
(global-set-key (kbd "<C-tab>") 'tab-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tab-previous)
(set-face-attribute 'tab-bar nil :height 180)


;; Save tabs and window layout via desktop-save-mode
(desktop-save-mode 1)

;; 1) Ensure the desktop-save dir exists
(let ((desktop-dir (expand-file-name "~/.emacs.d/desktop/")))
  (unless (file-directory-p desktop-dir)
    (make-directory desktop-dir t)))           ; auto-create directory

;; 2) Configure desktop-path and enable saving
(setq desktop-path '("~/.emacs.d/desktop/" "."))  ; search new subdir first
(setq desktop-restore-frames t)                   ; restore window layouts
(desktop-save-mode 1)    

(defun window-toggle-split-direction ()
  "Toggle window split between vertical and horizontal layout for two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)))
    (message "Only works with exactly two windows.")))

;;; Markdown + Grip
(use-package markdown-mode :mode ("\\.md\\'" . markdown-mode))
(use-package grip-mode
  :ensure t
  :config (setq grip-command 'auto
                grip-binary-path (if (eq system-type 'darwin)
                                     "/opt/homebrew/bin/grip"
                                   "~/.asdf/shims/grip"))
  :bind (:map markdown-mode-command-map ("g" . grip-mode)))

;;; Telega
(use-package telega
  :straight (:host github :repo "zevlg/telega.el")
  :commands (telega)
  :custom (telega-server-libs-prefix (if (eq system-type 'darwin)
                                         "/opt/homebrew/opt/tdlib"
                                       "/usr")))
(use-package gptel
 :config
 (setq gptel-api-key (getenv "OPENAI_API_KEY")))

(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq blink-matching-paren-distance nil)   ; show-paren works on big files too

(use-package golden-ratio
  :init
  (golden-ratio-mode 1))


;;
;;(use-package kubernetes
;;  :ensure t
;;  :commands (kubernetes-overview)
;;  :config
;;  (setq kubernetes-poll-frequency 3600
;;        kubernetes-redraw-frequency 3600))
