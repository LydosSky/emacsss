;;; module-tree-sitter.el --- Tree-sitter configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up Tree-sitter for advanced syntax highlighting and code parsing.

;;; Code:

;; Tree-sitter core
(use-package tree-sitter
  :ensure t
  :defer t
  :config)

;; ;; Tree-sitter language bundles
(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after tree-sitter)



(provide 'module-tree-sitter)
;;; module-tree-sitter.el ends here
