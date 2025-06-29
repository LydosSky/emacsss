;;; module-company.el --- Company mode configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; Company is a modular text completion framework.
;; This configuration sets up Company with various backends and enhancements.

;;; Code:

(use-package company
  :defer t
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0.1)  ;; Delay before suggestions pop up
  (setq company-minimum-prefix-length 2)  ;; Minimum number of characters before suggestions
  (setq company-show-numbers nil)  ;; Show numbers for suggestions
  (setq company-tooltip-align-annotations t)  ;; Align annotations to the right tooltip border
  (setq company-selection-wrap-around t)  ;; Wrap around suggestions
  (setq company-require-match 'never)
  (setq company-global-modes '(not vterm-mode help-mode message-mode))
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-offset-display 'scroll)  ;; Can be 'lines', 'scrollbar', or nil
  (global-set-key (kbd "M-/") 'company-complete)  ;; Keybinding for manual completion
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)  ;; Use tab for cycling completions
              ("<backtab>" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-backends
        '(company-capf
          company-dabbrev
          company-dabbrev-code
          company-keywords
          company-files
          company-yasnippet))
  )



(use-package company-statistics
  :defer t
  :after company
  :config
  (company-statistics-mode))  ;; Enable company-statistics mode


(use-package company-box
  :defer t
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-enable-icon t)
  (company-box-scrollbar nil)
  :init
  ;; Define a list of letter icons
  (setq company-box-icons-alist 'company-box-icons-letters)
  (setq company-box-icons-letters
        `((Unknown       . ,(propertize "U" 'face 'font-lock-comment-face))
          (Text          . ,(propertize "T" 'face 'font-lock-string-face))
          (Method        . ,(propertize "M" 'face 'font-lock-function-name-face))
          (Function      . ,(propertize "ƒ" 'face 'font-lock-function-name-face))
          (Constructor   . ,(propertize "C" 'face 'font-lock-type-face))
          (Field         . ,(propertize "F" 'face 'font-lock-variable-name-face))
          (Variable      . ,(propertize "V" 'face 'font-lock-variable-name-face))
          (Class         . ,(propertize "C" 'face 'font-lock-type-face))
          (Interface     . ,(propertize "I" 'face 'font-lock-type-face))
          (Module        . ,(propertize "M" 'face 'font-lock-constant-face))
          (Property      . ,(propertize "P" 'face 'font-lock-variable-name-face))
          (Unit          . ,(propertize "U" 'face 'font-lock-constant-face))
          (Value         . ,(propertize "V" 'face 'font-lock-builtin-face))
          (Enum          . ,(propertize "E" 'face 'font-lock-builtin-face))
          (Keyword       . ,(propertize "K" 'face 'font-lock-keyword-face))
          (Snippet       . ,(propertize "S" 'face 'font-lock-string-face))
          (Color         . ,(propertize "C" 'face 'font-lock-type-face))
          (File          . ,(propertize "F" 'face 'font-lock-string-face))
          (Reference     . ,(propertize "R" 'face 'font-lock-variable-name-face))
          (Folder        . ,(propertize "D" 'face 'font-lock-doc-face))
          (EnumMember    . ,(propertize "E" 'face 'font-lock-builtin-face))
          (Constant      . ,(propertize "C" 'face 'font-lock-constant-face))
          (Struct        . ,(propertize "S" 'face 'font-lock-type-face))
          (Event         . ,(propertize "E" 'face 'font-lock-warning-face))
          (Operator      . ,(propertize "O" 'face 'font-lock-comment-delimiter-face))
          (TypeParameter . ,(propertize "T" 'face 'font-lock-type-face)))))


;; (setq-local completion-at-point-functions
;;             (mapcar #'cape-company-to-capf
;;                     (list #'company-files #'company-keywords #'company-dabbrev #'company-yasnippet)))


(provide 'module-company)
;;; module-company.el ends here
