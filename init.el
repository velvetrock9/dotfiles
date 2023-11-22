;; Initialize package sources
(require 'package)

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

;; helps to initialize environment variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOROOT"))

;; Theme-folder
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;; Theme
(load-theme 'basic t)

;; startup frame size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
                    :font "Fira Code"
                    :height 180)



;; nerd icons
(use-package nerd-icons)

;; use doom-emacs mod-line
(use-package doom-modeline
	     :ensure t
	     :init (doom-modeline-mode 1)
	     :custom ((doom-modeline-height 15)))


;; disable line numbers in some modes
(dolist (mode '(org-mode-hook
		eshell-mode-hook
		shell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; open new shell buffer in a split window instead of a new frame
(defun split-window-and-start-shell ()
  (interactive)
  (split-window-below(round (* 0.7 (window-total-height))))
  (other-window 1)
  (shell (generate-new-buffer-name "*shell*")))

(global-set-key (kbd "C-c s") 'split-window-and-start-shell)

;; kill current buffer and close ALL windows in current frame, associated with it
(defun kill-buffer-and-all-windows ()
  "Kill the current buffer and all windows that display it."
  (interactive)
  (let ((buffer (current-buffer)))
    (dolist (window (window-list))
      (when (eq (window-buffer window) buffer)
        (delete-window window)))
    (kill-buffer buffer)))

(global-set-key (kbd "C-c k") 'kill-buffer-and-all-windows)
		
;; rebind keys to switch between current window frames
(global-set-key (kbd "M-o") 'other-window)

;; save all backup files in a particular directory
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(require 'company)
(require 'yasnippet)

;; code completions menu reveal timing
(setq company-idle-delay 0)


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


 

;; delete selected text
(delete-selection-mode 1)


(setq use-short-answers t)


(electric-pair-mode 1)


