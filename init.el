;; -*- lexical-binding: t; -*-
;;; 
;;; Cassio's Emacs Configuration
;;;

;; Copyright (C) Cássio Ávila
;; Author: Cássio Ávila <cassioavila@protonmail.com>
;; URL: https://github.com/toniz4/.emacs.d
;; This file is not part of GNU Emacs.
;; This file is free software.

;; The following code was auto-tangled from init.org. ;;

(setq use-short-answers t)
(setq ring-bell-function 'ignore)

;; scroll
(setq scroll-conservatively 1000)
(setq scroll-margin 2)

;; Revert window changes
(winner-mode)

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

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

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

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But don't resize pixelwise
(setq window-resize-pixelwise nil)

(defun my/set-font-faces ()
  (let* ((main-font "GoMono Nerd Font Mono")
         (fallback "monospace")
         (font (if (x-list-fonts main-font) main-font fallback)))
    (set-face-attribute 'default nil :font font :height 90)
    (set-face-attribute 'fixed-pitch nil :font font :height 90)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my/set-font-faces))))
  (my/set-font-faces))

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-local-mode t)))

(add-hook 'org-present-mode-hook
          (lambda ()
            (visual-fill-column-mode 1)
            (setq mode-line-format nil)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (visual-fill-column-mode 0)
            (doom-modeline-mode)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq-local tab-width 4)))

;; Switch to the scratch buffer
(defun my/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/switch-to-dashboard-buffer ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(setq bookmark-save-flag 1
      bookmark-set-fringe-mark nil)

(defun my/bookmark-make-record ()
  `((filename . ,(buffer-file-name))))

(setq bookmark-make-record-function #'my/bookmark-make-record)

(save-place-mode)

; Straight bootstrap
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(require 'org-tempo)

(defun my/set-evil-keybinds ()
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ss") 'sp-forward-slurp-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>sb") 'sp-forword-barf-sexp)
  (evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>lb") 'switch-to-buffer))

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-operator-state-tag "OPR"
        evil-normal-state-tag "NOR"
        evil-insert-state-tag "INS"
        evil-visual-state-tag "VIS"
        evil-replace-state-tag "REP"
        evil-emacs-state-tag "EMC"
        evil-motion-state-tag "MOT")

  (use-package undo-fu)

  (setq evil-echo-state nil
        evil-undo-system 'undo-fu
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-search-module 'evil-search)

  :custom
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :init (evil-commentary-mode))

(use-package general
  :init
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"
   ;; Applications
   "a" '(nil :which-key "applications")
   "ag" '(magit-status :which-key "magit")
   "ad" '(my/switch-to-dashboard-buffer :which-key "dashboard")

   "SPC" '(execute-extended-command :which-key "M-x")
   "q" '(save-buffers-kill-emacs :which-key "quit emacs")
   ;; Buffes 
   "b" '(nil :which-key "buffer")
   "ba" '(bookmark-set :which-key "set bookmark")
   "bb" '(switch-to-buffer :which-key "switch buffers")
   "bd" '(evil-delete-buffer :which-key "delete buffer")
   "bk" '(kill-buffer :which-key "kill other buffers")
   "bs" '(my/switch-to-scratch-buffer :which-key "scratch buffer")
   "bi" '(clone-indirect-buffer  :which-key "indirect buffer")
   "br" '(revert-buffer :which-key "revert buffer")

   ;; Files
   "f" '(nil :which-key "files")
   "fb" '(counsel-bookmark :which-key "bookmarks")
   "ff" '(find-file :which-key "find file")
   ;; "fn" '(new-file :which-key "new file")
   ;; "fr" '(counsel-recentf :which-key "recent files")
   "fR" '(rename-file :which-key "rename file")
   "fs" '(save-buffer :which-key "save buffer")
   "fS" '(evil-write-all :which-key "save all buffers")))

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

(use-package pulsar
  :init
  (pulsar-global-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package magit
  :commands (magit-status))

(use-package rainbow-mode)

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  (dashboard-setup-startup-hook))

(use-package visual-fill-column
  :commands
  (visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width 110))

(use-package fish-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package elixir-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package clojure-mode)

(use-package cider
  :init
  (setq cider-show-error-buffer nil))

(use-package python-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-dispatch '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-at-boundary nil)      ;; Never quit at completion boundary
  (corfu-quit-no-match t)           ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)       ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (completion-styles '(orderless-fast basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

(use-package cape
  ;; Bind dedicated completion commands
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-snippets)

(use-package yasnippet)

(use-package flycheck)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless-fast)))

  (defun my/update-completions-list ()
    (progn
      (fset 'non-greedy-lsp
            (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq completion-at-point-functions
            '(non-greedy-lsp cape-file))))

  :hook ((clojure-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-completion-mode . my/update-completions-list)
         (lsp-mode . yas-minor-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package doom-modeline
  :init
  (setq doom-modeline-height 0)
  :hook (after-init . doom-modeline-mode))

(defun my/org-mode-setup ()
  (display-line-numbers-mode 0)

  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)

  ;; Org tempo
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org
  :hook
  (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-present
  :commands (org-present))
