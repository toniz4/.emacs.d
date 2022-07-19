;; -*- lexical-binding: t; -*-

;; Garbage Collections
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; Data emacs reads from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Compile warnings
;;  (setq warning-minimum-level :emergency)
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq package-native-compile t
      native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil)

;; MISC OPTIMIZATIONS ----
  ;;; optimizations (froom Doom's core.el). See that file for descriptions.
(setq idle-update-delay 1.0)

;; Disabling bidi (bidirectional editing stuff)
(setq-default bidi-display-reordering 'left-to-right 
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; emacs 27 only - disables bidirectional parenthesis

(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

;; Disable default package.el
(setq package-enable-at-startup nil)
