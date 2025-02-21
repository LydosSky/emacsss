;;; module-company.el --- Company mode configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; Company is a modular text completion framework.
;; This configuration sets up Company with various backends and enhancements.

;;; Code:

(use-package company
  :defer t
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0.2)  ;; Delay before suggestions pop up
  (setq company-minimum-prefix-length 2)  ;; Minimum number of characters before suggestions
  (setq company-show-numbers t)  ;; Show numbers for suggestions
  (setq company-tooltip-align-annotations t)  ;; Align annotations to the right tooltip border
  (setq company-selection-wrap-around t)  ;; Wrap around suggestions
  (setq company-require-match 'never)
  (setq company-global-modes '(not vterm-mode help-mode message-mode))
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "M-/") 'company-complete)  ;; Keybinding for manual completion
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)  ;; Use tab for cycling completions
              ("<backtab>" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))


(use-package company-statistics
  :defer t
  :after company
  :config
  (company-statistics-mode))  ;; Enable company-statistics mode
(setq-local completion-at-point-functions
            (mapcar #'cape-company-to-capf
                    (list #'company-files #'company-keywords #'company-dabbrev)))
(setq company-tooltip-align-annotations t)
(setq company-tooltip-offset-display 'lines)  ;; Can be 'lines', 'scrollbar', or nil

(provide 'module-company)
;;; module-company.el ends here
