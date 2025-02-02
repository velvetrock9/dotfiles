;; Initialize package sources
(require 'package)

;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remap Meta to Option key
(setq mac-option-modifier 'meta)       ; Make the Option key act as Meta
;; (setq mac-command-modifier 'super)     ; Make the Command key act as Super

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms!!
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Move ~filename and #filename# files to a separate director
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; Store backups in ~/.emacs.d/backups/
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t))) ; Store auto-saves in a separate directory

;;Don't create .#filename files
(setq create-lockfiles nil)

;; helps to initialize environment variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOBIN"))

;; Theme-folder
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;; Theme

(load-theme 'vt220-orange t)

;; Paste new line below the current line
(defun paste-new-line-below ()
  "Insert a new line below the current line and move the cursor to it."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline))
  (forward-line 1))
(global-set-key (kbd "C-c O") 'paste-new-line-below)

;; startup message
(setq inhibit-startup-message t
      visible-bell t)

;; toolbar
(tool-bar-mode -1)

;; scrollbar
(scroll-bar-mode -1)

;; line numbers un every buffer
(global-display-line-numbers-mode 1)

;; blinking cursor
(blink-cursor-mode t)


;; Highlight active line
(hl-line-mode t)

;; Recent files opening
(recentf-mode 1)
;; Optional: Set the maximum number of recent files to track
(setq recentf-max-menu-items 25)
;; Optional: Save recent files list periodically
(run-at-time nil (* 5 60) 'recentf-save-list)
;; Optional: Keybinding to open recent files quickly
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Remember cursor position after file is closed
(save-place-mode 1)

;; Move customization variables to a separate file and load it on start
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Update buffers when they were edited outside of emacs
(global-auto-revert-mode 1)

;; Also update non-file buffers
(setq global-auto-revert-non-file-buffers t)

;; set font size
;; Height values in 1/10pt, so 100 will give you 10pt, etc.
(set-face-attribute 'default nil
                    :font "IBM Plex Mono"
                    :height 170)

(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (when evil-want-C-u-scroll
      (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; nerd icons
(use-package nerd-icons)
;; Enable vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; disable line numbers in some modes
(dolist (mode '(org-mode-hook
		shell-mode-hook
		term-mode-hook
		dashboard-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to Quake Emacs")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; rebind keys to switch between current window frames
(global-set-key (kbd "M-o") 'other-window)

;; save all backup files in a particular directory
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Implement vscode-like line vertical movement
(defun move-line-up ()
  "Move the current line up by one row."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
  ;; keybinding
  (global-set-key (kbd "M-p") 'move-line-up)
  (global-set-key (kbd "M-<up>") 'move-line-up)
(defun move-line-down ()
  "Move the current line down by one row."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
  ;; keybinding
  (global-set-key (kbd "M-n") 'move-line-down)
  (global-set-key (kbd "M-<down>") 'move-line-down)

;; Implement vscode-like line vertical copying
(defun duplicate-line-up ()
  "Duplicate the current line above."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (beginning-of-line)
    (insert current-line)
    ;; - 1 means "Move UP by ONE line"
    (forward-line -1)))
  ;; keybinding
  (global-set-key (kbd "M-P") 'duplicate-line-up)
  (global-set-key (kbd "M-S-<up>") 'duplicate-line-up)
(defun duplicate-line-down ()
  "Duplicate the current line below."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    ;; 1 means "Move DOWN by ONE line"
    (beginning-of-line)
    (forward-line 1)
    (insert current-line)
    (forward-line -1)))
  ;; keybinding
  (global-set-key (kbd "M-N") 'duplicate-line-down)
  (global-set-key (kbd "M-S-<down>") 'duplicate-line-down)

(use-package company)
(use-package yasnippet)
;; code completions menu reveal timing
(setq company-idle-delay 0)
 

;; delete selected text
(delete-selection-mode 1)

;; Yes/No to y/n
(setq use-short-answers t)

;; Brackets, Parenthesis and Curly Braces
(electric-pair-mode 1)

(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

;; Paredit for parentheses management
(use-package paredit
  :ensure t
  :hook (scheme-mode . paredit-mode))

;; Rainbow Delimiters for better visualization
(use-package rainbow-delimiters
  :ensure t
  :hook (scheme-mode . rainbow-delimiters-mode))
  
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

    ;; Customize dashboard components order
  (setq dashboard-startupify-list '(
				    dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-items           ;; Recent Files, Bookmarks, etc.
                                    dashboard-insert-newline
                                    dashboard-insert-init-info       ;; Package info at the bottom
                                    dashboard-insert-newline))       ;; Newline at the bottom
  
  ;; Customize dashboard
  (setq dashboard-banner-logo-title "vt220")
  (setq dashboard-startup-banner "~/.emacs.d/goffman.png")  ;; Remove the banner
  (setq dashboard-center-content t)    ;; Center the content
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)))  ;; Only display Recent Files
  (setq dashboard-set-footer nil)      ;; Remove the random quote
  (setq dashboard-set-init-info t)     ;; Show packages loaded info at the bottom
  (setq dashboard-projects-backend 'projectile)
)




(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)) ; Enable yasnippet globally



;; Add extensions
(use-package cape
  :bind (("C-c c p" . completion-at-point)
         ("C-c c d" . cape-dabbrev)
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword))
  :init
  (setq completion-at-point-functions
        (list #'lsp-completion-at-point
              #'cape-dabbrev
              #'cape-file
              #'cape-keyword))) 

(use-package marginalia)
(marginalia-mode t)

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)          ;; Minimum prefix length for auto-popup
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  :bind (:map corfu-map
              ("TAB" . corfu-next) ;; Next item
              ("<backtab>" . corfu-previous) ;; Previous item
              ("<down>" . corfu-next) ;; Arrow key support
              ("<up>" . corfu-previous) ;; Arrow key support
              ("<mouse-4>" . corfu-scroll-up)    ;; Scroll up
	      ("<mouse-5>" . corfu-scroll-down)) ;; Scroll down
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(setq scroll-step 1)  ; Scroll by 1 line at a time
(setq scroll-conservatively 10000)  ; Prevent recenters while scrolling

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; LSP support
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((go-mode		.	lsp-deferred))
  :bind (:map lsp-mode-map
	      ("C-c d"		.	lsp-describe-thing-at-point)
	      ("C-c a"		.	lsp-execute-code-action)
	      ("C-c f"		.	flycheck-list-errors))
  :custom
    ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; general
  (define-key lsp-mode-map (kbd "C-c l") 'lsp-command-map)
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-links nil
	lsp-keep-workspace-alive nil
	lsp-signature-doc-lines 2
	lsp-enable-snippet nil)
(setq lsp-completion-provider :none))
	 
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))	 

;; Flycheck setup to rely on LSP
(use-package flycheck
  :init (global-flycheck-mode))

;; Geiser with Racket support
(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(racket))
  :config
  (add-hook 'geiser-repl-mode-hook #'paredit-mode))

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Bookmarks
(use-package bm
         :ensure t
         :demand t

         :init
         ;; restore on load (even before you require bm)
         (setq bm-restore-repository-on-load t)


         :config
         ;; Allow cross-buffer 'next'
         (setq bm-cycle-all-buffers t)

         ;; where to store persistant files
         (setq bm-repository-file "~/.emacs.d/bm-repository")

         ;; save bookmarks
         (setq-default bm-buffer-persistence t)
         ;; Loading the repository from file when on start up.
         (add-hook 'after-init-hook 'bm-repository-load)

         ;; Saving bookmarks
         (add-hook 'kill-buffer-hook #'bm-buffer-save)

         ;; Saving the repository to file when on exit.
         ;; kill-buffer-hook is not called when Emacs is killed, so we
         ;; must save all bookmarks first.
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)

         ;; Restoring bookmarks
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


         :bind (("C-c m n" . bm-next)
                ("C-c m p" . bm-previous)
                ("C-c m a" . bm-toggle))
)
(use-package projectile)
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Sandbox/"))


(use-package multi-vterm
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(global-set-key (kbd "C-c o v") 'multi-vterm)
(global-set-key (kbd "C-c e b") 'eval-buffer)

(use-package org
  :demand t
  :config 
  (global-set-key (kbd "C-c t s") 'org-timer-set-timer)
  (global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)
  (setq org-clock-sound "~/Music/orgtimer.wav"))
