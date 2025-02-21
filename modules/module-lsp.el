;;; module-lsp.el --- LSP mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up lsp-mode for language server support, integrating
;; with lsp-ui while disabling any distracting UI features.

;;; Code:


;;; LSP Mode
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook
  ((prog-mode . (lambda ()
                  (when (derived-mode-p 'python-mode 'c-mode 'c++-mode 'javascript-mode
                                        'ruby-mode 'go-mode 'rust-mode 'java-mode)
                    (lsp-deferred))))
   ;; Add other modes you want to enable lsp-mode for
   )
  :init
  (setq lsp-keymap-prefix "C-x c")  ;; Or any other preferred prefix
  :config
  ;; General LSP settings
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil)  ;; Use flycheck instead of flymake
  ;; Performance optimizations
  (setq gc-cons-threshold 100000000)   ;; Increase garbage collection threshold
  (setq read-process-output-max (* 1024 1024)) ;; Increase amount of data read from the process
  )

;;; LSP UI
(use-package lsp-ui
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  ;; Disable lsp-ui's sideline and documentation popups
  ;; These can be distracting, so we disable them
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-imenu-enable t        ;; Enable lsp-ui-imenu for code navigation
        lsp-ui-peek-enable t         ;; Enable lsp-ui-peek for references and definitions
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil)
  :config
  ;; Additional configurations if needed
  ;; Disable formatting
  ;; Disable LSP's formatting if using Apheleia
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-formatting nil)
  )

                                        ;6u
(provide 'module-lsp)
;;; module-lsp.el ends here
