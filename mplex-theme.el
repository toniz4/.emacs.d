(deftheme mplex "Simple minimal theme")

(defun mplex--face (name &rest args)
  (list name `((t ,args))))

(let ((class '((class color) (min-colors #xFFFFFF)))
      (mplex-black "#050505")
      (mplex-grey "#363636")
      (mplex-white "#F0F0F0")
      (mplex-red "#c37474")
      (mplex-lred "#C49EA0")
      (mplex-green "#8AAC8B")
      (mplex-lgreen "#9EC49F")
      (mplex-yellow "#D28846")
      (mplex-lyellow "#C4C19E")
      (mplex-blue "#8F8AAC")
      (mplex-lblue "#2F4243")
      (mplex-magenta "#AC8AAC")
      (mplex-lmagenta "#C49EC4")
      (mplex-cyan "#8AABAC")
      (mplex-lcyan "#8AABAC")
      (mplex-fg "#F0F0F0")
      (mplex-bg "#050505"))

  (custom-theme-set-faces
   'mplex
   ;; Base
   (mplex--face 'default :background mplex-bg :foreground mplex-fg)
   (mplex--face 'vertical-border :inherit 'default)
   (mplex--face 'fringe :inherit 'default)
   (mplex--face 'cursor :inherit 'default)
   (mplex--face 'bold :bold t)
   (mplex--face 'italic :italic t)
   (mplex--face 'bold-italic :bold-italic t)
   (mplex--face 'underline :inherit 'default)
   (mplex--face 'region :background mplex-lblue)
   (mplex--face 'custom-face-tag :inherit 'default)
   (mplex--face 'custom-state :inherit 'default)
   (mplex--face 'line-number :foreground mplex-grey)
   (mplex--face 'show-paren-match :background mplex-bg :foreground mplex-yellow :bold t)

   (mplex--face 'isearch :foreground mplex-bg :background mplex-yellow)

   (mplex--face 'link :foreground mplex-cyan :underline t)

   (mplex--face 'error :foreground mplex-red)
   (mplex--face 'warning :foreground mplex-yellow)
   (mplex--face 'success :foreground mplex-green)

   (mplex--face 'shadow :foreground mplex-grey)

   ;; Mode-line
   (mplex--face 'mode-line :box mplex-grey :foreground mplex-yellow)
   (mplex--face 'mode-line-inactive :foreground mplex-grey)
   (mplex--face 'mode-line-emphasis :foreground mplex-red)
   (mplex--face 'mode-line-hightlight :foreground mplex-green)
   (mplex--face 'doom-modeline-evil-insert-state :bold t :foreground mplex-magenta)

   (mplex--face 'font-lock-builtin-face :foreground mplex-lyellow)
   (mplex--face 'font-lock-comment-face :foreground mplex-grey :italic t)
   (mplex--face 'font-lock-comment-delimiter-face :inherit 'font-lock-comment-face)
   (mplex--face 'font-lock-constant-face :inherit 'default)
   (mplex--face 'font-lock-doc-face :inherit 'default)
   (mplex--face 'font-lock-doc-string-face :inherit 'default)
   (mplex--face 'font-lock-function-name-face :inherit 'default)
   (mplex--face 'font-lock-keyword-face :inherit 'default)
   (mplex--face 'font-lock-negation-char-face :inherit 'default)
   (mplex--face 'font-lock-preprocessor-face :inherit 'default)
   (mplex--face 'font-lock-string-face :foreground mplex-green)
   (mplex--face 'font-lock-type-face :inherit 'default :bold t)
   (mplex--face 'font-lock-variable-name-face :inherit 'default)
   ;; (mplex--face 'font-lock-warning-face :inherit 'default)
   (mplex--face 'minibuffer-prompt :inherit 'default :bold t)

   ;; Evil mode
   (mplex--face 'evil-ex-lazy-highlight
		:foreground mplex-bg :background mplex-yellow :bold t)

   ;; Pulsar
   (mplex--face 'pulse-highlight-start-face :background mplex-grey)

   ;; Vertico
   (mplex--face 'vertico-current :background mplex-grey)

   ;; Completions
   (mplex--face 'completions-common-part :foreground mplex-yellow)

   ;; Company
   (mplex--face 'company-tooltip :inherit 'default)
   (mplex--face 'company-tooltip-selection :background mplex-grey)
   (mplex--face 'company-tooltip-common :foreground mplex-yellow)
   (mplex--face 'company-scrollbar-fg :background mplex-grey)
   (mplex--face 'company-scrollbar-bg :inherit 'company-tooltip)
   (mplex--face 'company-tooltip-annotation :inherit 'font-lock-comment-face)
   (mplex--face 'company-tooltip-annotation-selection
		:inherit 'font-lock-comment-face
		:foreground mplex-bg)

   ;; flycheck
   (mplex--face 'flycheck-error :underline (list :style 'wave :color mplex-red))
   (mplex--face 'flycheck-warning :underline (list :style 'wave :color mplex-yellow))
   (mplex--face 'flycheck-info :underline (list :style 'wave :color mplex-blue))

   ;; Lsp ui
   (mplex--face 'lsp-face-highlight-textual :background mplex-grey)
   ;; Org mode
   (mplex--face 'org-block :inherit 'default)
   ))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; (custom-set-faces
;;  '(company-tooltip
;;    ((t (:background "#FF0000" :foreground "#FF0000"))))
;;  '(company-tooltip-selection
;;    ((t (:background "#FF0000" :foreground "#FF0000"))))
;;  '(company-tooltip-common ((t (:weight bold :foreground "#FF0000"))))
;;  '(company-scrollbar-fg ((t (:background "#FE0000"))))
;;  '(company-scrollbar-bg ((t (:background "#FF0000"))))
;;  '(company-tooltip-annotation ((t (:foreground "#00FF00")))))

(provide-theme 'mplex)
; (load-theme 'mplex t)
