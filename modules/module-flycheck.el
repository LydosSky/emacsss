;;; module-flycheck.el --- Flycheck configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize Flycheck to minimize UI distractions by adjusting error displays.

;;; Code:

(use-package flycheck
  :defer t
  :init
  (global-flycheck-mode)
  :config
  ;; Additional configurations
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Other options: 'idle-change, 'new-line, 'idle-buffer-switch
  ;; Disable Flycheck's error notifications and symbols
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-indication-mode 'right-fringe) ;; Or 'left-fringe or nil

  ;; Customize fringe indicators to be less intrusive
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] nil nil 'center))

  ;; Optionally disable underlining of errors
  (setq flycheck-highlighting-mode 'nil) ;; Options: 'symbols, 'lines, 'columns, or nil
  (setq flycheck-highlighting-style 'level-face)

  ;; Disable tooltip pop-ups
  (setq flycheck-pos-tip-mode nil)
  (setq flycheck-display-errors-delay 0.3)
  )

(provide 'module-flycheck)
;;; module-flycheck.el ends here
