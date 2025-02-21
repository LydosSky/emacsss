;;; module-completion.el --- Enhanced completion framework configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up Vertico and related packages to provide a comprehensive
;; and efficient completion experience in Emacs. All packages are deferred to
;; optimize startup time.

;;; Code:


;;; Vertico: Vertical Interactive Completion
(use-package vertico
  :straight (:files (:defaults "extensions/*")) ; Include extensions
  :defer t
  :init
  (setq vertico-cycle t)
  (setq resize-mini-windows t)
  ;; Enable vertico
  (vertico-mode))




(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;;; Orderless: Flexible Completion Matching
(use-package orderless
  :defer t
  :init
  ;; Configure completion styles
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia: Enriching Completion Annotations
(use-package marginalia
  :defer t
  :after vertico
  :bind (("M-A" . marginalia-cycle) ;; Use M-A for cycling annotations
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; Consult: Enhanced Commands Based on Completing-Read
(use-package consult
  :defer t
  :bind (("C-s" . consult-line)            ;; Enhanced search within buffer
         ("C-M-l" . consult-imenu)         ;; Navigate code structure
         ("C-x b" . consult-buffer)        ;; Enhanced buffer switching
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r" . consult-recent-file)
         ("M-g M-g" . consult-goto-line))  ;; Go to line with preview
  :init
  ;; Replace `completing-read-multiple' with enhanced version
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  ;; Optionally configure narrowing key
  (setq consult-narrow-key "<")
  ;; Configure preview settings
  (setq consult-preview-key 'any)
  (setq consult-project-root-function #'projectile-project-root))

;;; Embark: Mini-Buffer Actions at Point
(use-package embark
  :defer t
  :bind (("C-." . embark-act)         ;; Invoke action on selected candidate
         ("C-;" . embark-dwim)        ;; Default action based on context
         ("C-h B" . embark-bindings)) ;; Alternative for `describe-bindings'
  :init
  ;; Replace `describe-bindings' with Embark
  (setq prefix-help-command #'embark-prefix-help-command))

;;; Embark-Consult: Integration Between Embark and Consult
(use-package embark-consult
  :defer t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Which-Key: Display Available Keybindings in Popup
(use-package which-key
  :defer t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;;; Helpful: Enhanced Help Pages
(use-package helpful
  :defer t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key))

;;; Savehist: Persist History Over Sessions
(use-package savehist
  :init
  (savehist-mode))

;;; Completion at Point Extensions (Cape)
(use-package cape
  :defer t
  :init
  ;; Add useful functions to completion-at-point-functions
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'completion-at-point-functions)
                           #'cape-dabbrev)
              (add-to-list (make-local-variable 'completion-at-point-functions)
                           #'cape-file)
              (add-to-list (make-local-variable 'completion-at-point-functions)
                           #'cape-keyword)))
  ;; Silence pcomplete capf, no errors when no matches
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure pcomplete works in Eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-super-capf
                                 #'pcomplete-completions-at-point
                                 #'cape-file)))))
  :config
  ;; Configure Cape
  (setq cape-dabbrev-min-length 3)
  (setq cape-dabbrev-check-other-buffers t))


(provide 'module-completion)
;;; module-completion.el ends here
