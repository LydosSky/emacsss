;;; module-performance.el --- performance tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive configuration to speed up Emacs startup time.

;;; Code:

;; Install and configure benchmark-init
(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; Display the benchmark report after initialization
(add-hook 'after-init-hook 'benchmark-init/show-durations-tabulated)

(provide 'module-performance)
;;; module-performance.el ends here
