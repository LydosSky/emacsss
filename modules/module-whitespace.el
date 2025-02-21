;;; module-whitespace.el --- Whitespace management configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up automatic removal of trailing whitespace upon saving files.
;; It also provides visual indicators for trailing whitespace and long lines.

;;; Code:

;; Automatically clean up whitespace on save
(defun my/cleanup-whitespace ()
  "Clean up whitespace before saving."
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

;; Enable whitespace cleanup in programming and text modes
(add-hook 'prog-mode-hook 'my/cleanup-whitespace)
(add-hook 'text-mode-hook 'my/cleanup-whitespace)

;; Optional: Visualize trailing whitespace
(defun my/show-trailing-whitespace ()
  "Enable visualization of trailing whitespace."
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'my/show-trailing-whitespace)
(add-hook 'text-mode-hook 'my/show-trailing-whitespace)

;; Optional: Customize whitespace mode for more control
(use-package whitespace
  :ensure t
  :defer t
  :config
  ;; Define which types of whitespace to visualize
  (setq whitespace-style '(face trailing tabs spaces lines-tail newline indentation::space))
  ;; Set the limit for long lines
  (setq whitespace-line-column 80)
  ;; Enable whitespace mode globally or in specific modes
  (global-whitespace-mode t)
  )

(provide 'module-whitespace)
;;; module-whitespace.el ends here
