;;; module-emmet.el --- Emmet mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up Emmet mode for rapid HTML and CSS development.
;; Emmet allows you to expand abbreviations into complete code structures,
;; streamlining your web development workflow.

;;; Code:
(use-package emmet-mode
  :ensure t
  :defer t
  :hook
  ((html-mode css-mode web-mode sgml-mode pug-mode
              rjsx-mode js-jsx-mode typescript-tsx-mode) . emmet-mode)
  :config
  ;; Set indentation preferences
  (setq emmet-indentation 2) ;; Indent by 2 spaces
  ;; Expand keys
  (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line)
  ;; Enable Emmet preview
  (setq emmet-preview-default nil)
  ;; Customize Emmet variables as needed
  )

(provide 'module-emmet)
;;; module-emmet.el ends here
