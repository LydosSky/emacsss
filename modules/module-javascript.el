;;; module-javascript.el --- JavaScript development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up an enhanced JavaScript development environment,
;; including syntax highlighting, LSP support, autocompletion, and more.

;;; Code:

;; Use js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js-mode)
  :hook
  ;;(js-mode . js2-minor-mode)
  (js-mode . tree-sitter-hl-mode)
  (js-mode . lsp)
  :interpreter ("node" . js-mode)
  :config
  ;; Use js2-mode as the default JS mode
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 0)
  )


;; Use rjsx-mode for React JSX files
(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook
  (rjsx-mode . tree-sitter-hl-mode)
  (rjsx-mode . lsp)
  :config
  ;; Indentation settings
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil)
  (setq js2-highlight-level 0))


(use-package typescript-mode
  :ensure t
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))



(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-mode-alist '(js2-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier)))

(defun my/enable-js2-minor-mode ()
  "Enable `js2-minor-mode` only if `prisma-mode` is not active."
  (unless (derived-mode-p 'prisma-mode)
    (js2-minor-mode 1)))

;;(add-hook 'js-mode-hook 'my/enable-js2-minor-mode)

(setq js-indent-level 2)
(provide 'module-javascript)
;;; module-javascript.el ends here
