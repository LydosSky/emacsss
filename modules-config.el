;;; modules-config.el --- Module enable/disable configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file controls which modules are enabled in the Emacs configuration.
;; Add module names to the `my-enabled-modules` list to enable them.

;;; Code:

;; This is the list of modules that will be load
(defvar list-modules
  '(
    ui                                  ; doom-themes modeline
    c                                   ;c/c++
    magit                               ; magit
    company                             ; company mode
    completion                          ; completion vertico and all crew
    lsp                                 ; lsp mode and ui
    env                                 ; env for getting PATH
    tree-sitter                         ; tree-sitter better syntax
    flycheck                            ; check this out
    formatter                           ; better format it bro!!!
    emmet                               ; tags everywhere
    pug                                 ; pug templates
    prisma                              ; prisma orm
    javascript                          ; javascript
    git-gutter                          ; git-gutter
    whitespace                          ; remove whitespaces
    ;; performance
    vterm                               ; terminal
    )
  "List of enabled modules.")

(provide 'modules-config)
;;; modules-config.el ends here
