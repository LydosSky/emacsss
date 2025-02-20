;;; modules-config.el --- Module enable/disable configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file controls which modules are enabled in the Emacs configuration.
;; Add module names to the `my-enabled-modules` list to enable them.

;;; Code:

(defvar list-modules
  '(
    magit
    company
    ui
    completion
    diff-hl
   )
    env                                 ; env for getting PATH
    flycheck                            ; check this out
  "List of enabled modules.")

(provide 'modules-config)
;;; modules-config.el ends here
