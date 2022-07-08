; Garbage collector
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; Data emacs reads from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

; Quality of life
(setq use-short-answers t)
(setq ring-bell-function 'ignore)

; Cache directory
(setq user-emacs-directory "~/.cache/emacs/")

(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory t))

; Backup directory
(setq backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      vc-make-backup-files t
      kept-old-versions 10
      kept-new-versions 10)

; Configuration directory

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
    (load custom-file t))

; Native compilation
(setq package-native-compile t
      native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil)

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

; Visual
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)


(blink-cursor-mode 0)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      server-client-instructions nil)

(load-theme 'mplex t)
; Line number mode
(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode)

; Scroll
(setq scroll-conservatively 1000)
(setq scroll-margin 2)

(defun my-set-font-faces ()
  (let ((my-font "Ttyp0 Otb"))
    (set-face-attribute 'default nil :font my-font)
    (set-face-attribute 'fixed-pitch nil :font my-font)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my-set-font-faces))))
  (my-set-font-faces))

; Straight bootstrap
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Straight config
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun my-set-evil-keybinds ()
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ss") 'sp-forward-slurp-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>sb") 'sp-forword-barf-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>lb") 'switch-to-buffer))

; Evil mode
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (use-package undo-fu)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  :custom
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (my-set-evil-keybinds))

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :init (evil-commentary-mode))

; Vertico
(use-package vertico
  :init
  (use-package savehist
    :init
    (savehist-mode))

  (vertico-mode)
  (setq vertico-scroll-margin 2))

(use-package which-key
  :config
  (which-key-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode))

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  (dashboard-setup-startup-hook))

;; Languages modes
(use-package fish-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package elixir-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package magit)

(use-package cider
  :init
  (setq cider-show-error-buffer nil))

;; Auto completion an lsp stuff
(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0 
	company-selection-wrap-around t)
  :init
  (add-hook 'after-init-hook 'global-company-mode))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (use-package yasnippet)
  :hook ((clojure-mode . lsp)
         (go-mode . lsp)
	 (c-mode . lsp)
	 (lsp-mode . yas-global-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;; Smartparens
(use-package smartparens
  :init
  (smartparens-global-mode t))
