;;; module-javascript.el --- JavaScript development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up an enhanced JavaScript development environment,
;; including syntax highlighting, LSP support, autocompletion, and more.

;;; Code:

;; Use js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . tree-sitter-hl-mode)
  :interpreter ("node" . js2-mode)
  :config
  ;; Use js2-mode as the default JS mode
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 0))

;; Use rjsx-mode for React JSX files
(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx\\'" . rjsx-mode)
  :hook
  (rjsx-mode . tree-sitter-hl-mode)
  :config
  ;; Indentation settings
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil)
  (setq js2-highlight-level 0))


(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  :config
  (setq typescript-indent-level 2))

;; Configure lsp-mode for JavaScript and TypeScript
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(js2-mode . "javascript"))
  (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascriptreact"))
  (add-to-list 'lsp-language-id-configuration '(typescript-mode . "typescript"))
  ;; Enable lsp-mode in js2-mode and rjsx-mode
  (add-hook 'js2-mode-hook #'lsp-deferred)
  (add-hook 'rjsx-mode-hook #'lsp-deferred))


(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-mode-alist '(js2-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier)))


(provide 'module-javascript)
;;; module-javascript.el ends here
