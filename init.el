;;; init.el --- Emacs configuration entry point

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

;;; OS/Environment Specific Setup
(cond
 ((eq system-type 'darwin) ;; macOS
  (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin:/opt/homebrew/sbin"))
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/sbin")

  ;; Disable touchpad gestures and scroll zooming
  (fboundp 'mac-trackpad-pinching-p)
  (setq mac-mouse-wheel-smooth-scroll nil)
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<magnify-up>") 'ignore)
  (global-set-key (kbd "<magnify-down>") 'ignore)
  (global-set-key (kbd "s-<wheel-up>") 'ignore)
  (global-set-key (kbd "s-<wheel-down>") 'ignore))

 ((eq system-type 'gnu/linux) ;; Linux
  (setenv "PATH" (concat (getenv "PATH") ":~/.asdf/shims"))
  (add-to-list 'exec-path "~/.asdf/shims")
  (add-to-list 'exec-path (expand-file-name "~/.opam/cs3110-2025s/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":~/.opam/default/bin"))))

;;; Keybindings and UI Enhancements
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-x |") #'window-toggle-split-direction)

(fido-mode 1)
(fido-vertical-mode 1)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package tron-theme
  :straight (tron-theme
             :type git
             :host github
             :repo "ivanmarcin/emacs-tron-theme"
             :files ("tron-theme.el"))
  :config
  (add-to-list 'custom-theme-load-path
               (straight--repos-dir "emacs-tron-theme"))
  (load-theme 'tron t))




;;; Recent Files, Backups, and Autosaves
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 100
      recentf-auto-cleanup 'never)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq make-backup-files t)

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))
(make-directory "~/.emacs.d/auto-saves/" t)
(setq create-lockfiles nil)

;;; Tree-sitter Manual Load Path
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))

;;; Language-Specific: Go
(use-package go-mode
  :hook ((go-mode . lsp-deferred))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp-deferred)))

(use-package company
  :hook (go-mode . company-mode))

(use-package yasnippet
  :hook (go-mode . yas-minor-mode))

;;; Language-Specific: OCaml
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :hook ((tuareg-mode . merlin-mode)))

(use-package merlin
  :hook (tuareg-mode . merlin-mode))

(add-to-list 'auto-mode-alist '("\\.ml\\'" . ocaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . ocaml-ts-mode))

;;; Project Management and Checking
(use-package projectile
  :init
  (setq projectile-project-search-path '(("~/Sandbox" . 2))
        projectile-auto-discover nil)
  :config
  (projectile-mode +1)
  (projectile-discover-projects-in-search-path)
  (global-unset-key (kbd "C-x p"))
  (define-key global-map (kbd "C-x p") projectile-command-map))

(use-package flycheck
  :init (global-flycheck-mode))

;;; Org and Documentation
(use-package org
  :straight (:type built-in :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                   :local-repo "org"
                   :depth full
                   :files (:defaults "lisp/*.el" "contrib/lisp/*.el"))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-directory "~/org"
        org-agenda-files '("~/org")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :custom (org-roam-directory "~/org/roam")
  :config (org-roam-db-autosync-enable))

;;; Terminal and External Services
(use-package multi-vterm :ensure t)

(use-package telega
  :straight (:host github :repo "zevlg/telega.el")
  :commands (telega)
  :custom (telega-server-libs-prefix "/usr"))

;;; Markdown and Preview
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package grip-mode
  :ensure t
  :config (setq grip-command 'auto)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(setq grip-binary-path
      (if (eq system-type 'darwin)
          "/opt/homebrew/bin/grip"
        "~/.asdf/shims/grip"))

;;; Window Management
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

;;; Tabs
(use-package tab-bar
  :config (tab-bar-mode 1))

(provide 'init)
;;; init.el ends here
