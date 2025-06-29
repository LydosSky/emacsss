;;; module-tree-sitter.el --- Tree-sitter configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up Tree-sitter for advanced syntax highlighting and code parsing.

;;; Code:

;; Tree-sitter core
;; (use-package tree-sitter
;;   :ensure t
;;   :defer t
;;   :config
;;   )

;; ;; Tree-sitter language bundles
(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after tree-sitter)

;; (with-eval-after-load 'tree-sitter
;;   (custom-set-faces
;;    `(tree-sitter-hl-face:number ((t (:foreground  ,(doom-color 'orange) :weight bold))))
;;    `(tree-sitter-hl-face:operator ((t (:foreground ,(doom-color 'fg)))))
;;    `(tree-sitter-hl-face:method ((t (:foreground  ,(doom-color 'blue)))))
;;    `(tree-sitter-hl-face:function ((t (:foreground ,(doom-color 'blue)))))
;;    `(tree-sitter-hl-face:constant ((t (:foreground ,(doom-color 'red)))))
;;    `(tree-sitter-hl-face:keyword ((t (:foreground ,(doom-color 'magenta)))))
;;    `(tree-sitter-hl-face:string.special ((t (:foreground ,(doom-color 'blue)))))
;;    `(tree-sitter-hl-face:variable.parameter ((t (:foreground ,(doom-color 'red))))))
;;   )

;; (with-eval-after-load 'tree-sitter
;;   (custom-set-faces
;;    '(tree-sitter-hl-face:function.call ((t (:inherit tree-sitter-hl-face:function :weight bold))))))





(custom-set-faces
 '(font-lock-escape-face ((t :inherit font-lock-warning-face)))
 '(font-lock-function-call-face ((t (:inherit font-lock-preprocessor-face)))))

(setq treesit-font-lock-level 6)

(provide 'module-tree-sitter)
;;; module-tree-sitter.el ends here
