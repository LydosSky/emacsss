;;; module-c.el --- C/C++ development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up an enhanced C/C++ development environment,
;; including syntax highlighting, LSP support, autocompletion, and formatting.

;;; Code:

;; Use cc-mode for C and C++ editing
(use-package cc-mode
  :ensure t    ; Ensure the package is installed
  :defer t     ; Defer loading until a C/C++ file is opened
  :mode (("\\.c\\'" . c-ts-mode)    ; Associate .c files with c-mode
         ("\\.h\\'" . c-ts-mode)    ; Associate .h files with c-mode
         ("\\.cpp\\'" . c++-ts-mode) ; Associate .cpp files with c++-mode
         ("\\.hpp\\'" . c++-ts-mode) ; Associate .hpp files with c++-mode
         ("\\.cc\\'" . c++-ts-mode)  ; Associate .cc files with c++-mode
         ("\\.hh\\'" . c++-ts-mode)) ; Associate .hh files with c++-mode
  :hook
  ;; Hooks run when entering c-mode or c++-mode
  ;; (c-mode . tree-sitter-hl-mode)  ; Enable tree-sitter for highlighting in C files
  ;; (c++-mode . tree-sitter-hl-mode) ; Enable tree-sitter for highlighting in C++ files
  (c-tsmode . lsp)                  ; Enable LSP for C files
  (c++-ts-mode . lsp)                ; Enable LSP for C++ files
  :config
  ;; Global indentation settings for cc-mode (applies to both C and C++)
  (setq c-basic-offset 2)        ; Set default indentation to 2 spaces
  (setq tab-width 2)             ; Set tab width to 2
  (setq indent-tabs-mode nil)    ; Use spaces for indentation, not tabs

  ;; Set a default coding style for C/C++ (e.g., "stroustrup", "k&r", "gnu", "bsd")
  ;; You can customize this to your preference or project's coding style.
  (setq c-default-style "k&r")

  ;; Hook to apply indentation settings specifically for c-mode
  (add-hook 'c-tsmode-hook
            (lambda ()
              (message "C mode activated. Setting C specific options.")
              ;; You can add more C-specific settings here if needed
              ))

  ;; Hook to apply indentation settings specifically for c++-mode
  (add-hook 'c++-ts-mode-hook
            (lambda ()
              (message "C++ mode activated. Setting C++ specific options.")
              ;; You can add more C++n-specific settings here if needed
              ))
  )

(provide 'module-c)

;;; module-c.el ends here
