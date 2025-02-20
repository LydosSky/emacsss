;;; module-prisma.el --- Tree-sitter configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up prisma mode
;;; Code:


(use-package prisma-mode
  :straight (:host github :repo "pimeys/emacs-prisma-mode" :branch "main")
  )


(provide 'module-prisma)
;;; module-prisma.el ends here
