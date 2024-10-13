;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms!!

(require 'use-package)
(setq use-package-always-ensure t)

;; helps to initialize environment variables

;; Theme-folder
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(load-theme 'modus-operandi t)

;; startup frame size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Paste new line below the current line
(defun paste-new-line-below ()
  "Insert a new line below the current line and move the cursor to it."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline))
  (next-line 1))
(global-set-key (kbd "C-c O") 'paste-new-line-below)

 ;; Jump to a line with 'C-c l'
(global-set-key (kbd "C-c l") 'goto-line) 

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
(hl-line-mode -1)

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
                    :height 180)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; nerd icons
(use-package nerd-icons)

;; use doom-emacs mod-line
(use-package doom-modeline
	     :ensure t
	     :init (doom-modeline-mode 1)
	     :custom ((doom-modeline-height 15)))


;; disable line numbers in some modes
(dolist (mode '(org-mode-hook
		shell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

		
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

(use-package marginalia)
(marginalia-mode t)

(use-package orderless)
(setq completion-styles '(orderless))
