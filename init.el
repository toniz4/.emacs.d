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

;; (load-theme 'mplex t)

; Line number mode
(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But don't resize pixelwise
(setq window-resize-pixelwise nil)

(setq window-divider-default-right-width 3
        window-divider-default-left-width 3)

  (window-divider-mode)

(defun my-set-font-faces ()
  (let* ((main-font "GoMono Nerd Font Mono")
         (fallback "monospace")
         (font (if (x-list-fonts main-font) main-font fallback)))
    (set-face-attribute 'default nil :font font :height 90)
    (set-face-attribute 'fixed-pitch nil :font font :height 90)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my-set-font-faces))))
  (my-set-font-faces))

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

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local tab-width 4)))

(add-hook 'doc-view-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

;; Switch to the scratch buffer
(defun my-switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my-switch-to-dashboard-buffer ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun upload-buffer-file-to-0x0 ()
  (interactive)
  (if-let ((filename (buffer-file-name))
           (curl (executable-find "curl")))
      (make-process
       :name "cu"
       :command `("curl" "-F" ,(concat "file=@" filename) "https://0x0.st")
       :filter (lambda (x y) (kill-new y)))))

(defun my-open-eshell ()
  (interactive)
  (dlet ((eshell-buffer-name "*eshell session*"))
    (cond ((equal (get-buffer eshell-buffer-name) (window-buffer (selected-window))) 
           (select-window (get-mru-window t t t))) ;; Focused on eshell buffer

          ((get-buffer-window eshell-buffer-name)
           (switch-to-buffer-other-window eshell-buffer-name)) ;; Visible in frame

          (t
           (let ((buf (eshell))) ;; Buffer does not exist
             (display-buffer buf '(display-buffer-below-selected . ((window-height . 10))))
             (switch-to-buffer (other-buffer buf))
             (switch-to-buffer-other-window buf))))))

(setq bookmark-save-flag 1
      bookmark-set-fringe-mark nil)

(defun my-bookmark-make-record ()
  `((filename . ,(buffer-file-name))))

(setq bookmark-make-record-function #'my-bookmark-make-record)

(save-place-mode)

(add-to-list 'exec-path
             (concat (getenv "HOME") "/.local/bin"))

(setq eshell-banner-message "")

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

(use-package evil-cleverparens
  :hook
  (clojure-mode . evil-cleverparens-mode)
  (emacs-lisp-mode . evil-cleverparens-mode)
  (scheme-mode . evil-cleverparens-mode))

(use-package general
  :init
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   "SPC" '(execute-extended-command :which-key "M-x")
   "q" '(save-buffers-kill-emacs :which-key "quit emacs")

   ;; Applications
   "a" '(nil :which-key "applications")
   "ag" '(magit-status :which-key "magit")
   "ad" '(my-switch-to-dashboard-buffer :which-key "dashboard")
   "as" '(my-open-eshell :which-key "eshell")

   ;; Buffes 
   "b" '(nil :which-key "buffer")
   "ba" '(bookmark-set :which-key "set bookmark")
   "bb" '(consult-buffer :which-key "switch buffers")
   "bd" '(evil-delete-buffer :which-key "delete buffer")
   "bk" '(kill-buffer :which-key "kill other buffers")
   "bs" '(my-switch-to-scratch-buffer :which-key "scratch buffer")
   "bi" '(clone-indirect-buffer  :which-key "indirect buffer")
   "br" '(revert-buffer :which-key "revert buffer")

   ;; Files
   "f" '(nil :which-key "files")
   "fb" '(consult-bookmark :which-key "bookmarks")
   "ff" '(find-file :which-key "find file")
   "fr" '(consult-recent-file :which-key "recent files")
   "fR" '(rename-file :which-key "rename file")
   "fs" '(save-buffer :which-key "save buffer")
   "fS" '(evil-write-all :which-key "save all buffers")
   "fg" '(consult-ripgrep :which-key "ripgrep")
   "fG" '(consult-grep :which-key "grep")

   ;; Window
   "w" '(nil :which-key "window")
   "ww" '(evil-window-next :which-key "next")
   "wv" '(evil-window-vsplit :which-key "vsplit")
   "wn" '(evil-window-split :which-key "split")
   "wq" '(evil-quit :which-key "close window")
   "w1" '(delete-other-windows :which-key "close other windows")

   ;; Help
   "h" '(nil :which-key "help")
   "hc" '(describe-char :which-key "describe char")
   "hC" '(describe-command :which-key "describe command")
   "hf" '(describe-function :which-key "describe function")
   "hF" '(describe-face :which-key "describe face")
   "hv" '(describe-variable :which-key "describe variable")))

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
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :init (evil-commentary-mode))

(use-package vertico
  :custom
  (vertico-scroll-margin 2)
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult)

(use-package consult-lsp)

(use-package consult-flycheck)

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
  :init
  (defun transient-bind-esc-to-quit ()
    (define-key transient-base-map   (kbd "<escape>") #'transient-quit-one)
    (define-key transient-sticky-map (kbd "<escape>") #'transient-quit-seq)
    (setq transient-substitute-key-function
          #'transient-rebind-quit-commands))
  :commands (magit-status))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 2)
  (eldoc-echo-area-display-truncation-message nil))

(use-package yaml-mode)

(use-package fish-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package elixir-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package clojure-mode)

(use-package cider
  :init

  (add-to-list 'completion-category-defaults '(cider (styles basic)))

  (setq cider-show-error-buffer nil))

(use-package python-mode
  :defer t
  :custom
  (python-shell-interpreter (executable-find "python")))

(use-package scad-mode)

(defun my-org-mode-setup ()
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
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  :hook
  (org-mode . my-org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))


(setq org-bullets-face-name "BlexMono Nerd Font")

(use-package org-present
  :commands (org-present))

(use-package orderless
  :config
  (defmacro dispatch: (regexp style)
    (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
      `(defun ,(symcat "dispatch:" style) (pattern _index _total)
         (when (string-match ,regexp pattern)
           (cons ',(symcat "orderless-" style) (match-string 1 pattern))))))

  (cl-flet ((pre/post (str) (format "^%s\\(.*\\)$\\|^\\(?1:.*\\)%s$" str str)))
    (dispatch: (pre/post "=") literal)
    (dispatch: (pre/post "`") regexp)
    (dispatch: (pre/post (if (or minibuffer-completing-file-name
                                 (derived-mode-p 'eshell-mode))
                             "%" "[%.]"))
               initialism))

  (dispatch: "^{\\(.*\\)}$" flex)
  (dispatch: "^\\([^][^\\+*]*[./-][^][\\+*$]*\\)$" prefixes)
  (dispatch: "^!\\(.+\\)$" without-literal)
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles 'orderless-regexp)
  (orderless-style-dispatchers
   '(dispatch:literal dispatch:regexp dispatch:without-literal
     dispatch:initialism dispatch:flex dispatch:prefixes))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)      ;; Never quit at completion boundary
  (corfu-quit-no-match nil)           ;; Never quit, even if there is no match
  (corfu-preselect-first nil)       ;; Disable candidate preselection
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  :bind
  (:map corfu-map
        ("C-s" . corfu-quit)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (minibuffer-setup . corfu-enable-in-minibuffer)
         (eshell-mode . corfu-mode)))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :commands
  (yas-minor-mode)
  :hook
  (prog-mode . yas-minor-mode))

(use-package flycheck
  :commands flycheck-mode)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-action-fallback-icon "?")
  (lsp-modeline-code-actions-segments '(icon count))
  :init
  (defun my-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (defun my-update-completions-list ()
    (progn
      (fset 'non-greedy-lsp
            (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
      (setq completion-at-point-functions
            '(non-greedy-lsp cape-file))))

  (defun my-lsp-python-setup ()
    (add-hook 'lsp-configure-hook
              (lambda ()
                    (when lsp-auto-configure
                      (flycheck-add-next-checker 'lsp 'python-pyright)))))

  (use-package lsp-ui :commands lsp-ui-mode)

  (setq lsp-enabled-clients '(pyright jedi clojure-lsp gopls clang))

  :hook ((clojure-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-mode . (lambda ()
                          (advice-add #'my-lsp-python-setup
                                      :after #'lsp-configure-buffer)))
         (lsp-completion-mode . my-lsp-mode-setup-completion)
         (lsp-completion-mode . my-update-completions-list)
         (lsp-mode . yas-minor-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-jedi
  :defer t)

(use-package lsp-pyright
  :defer t
  :custom
  (lsp-pyright-auto-import-completions nil)
  :init
  (defun my-python-setup ()

    (require 'lsp-pyright)
    (lsp-deferred))
  :hook
  (python-mode . my-python-setup))

(use-package acme-theme
  :straight
  (:local-repo "/home/cassio/src/acme-emacs-theme")
  :custom
  (acme-theme-black-fg t)
  :init
  (load-theme 'acme t))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 0)
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t))
