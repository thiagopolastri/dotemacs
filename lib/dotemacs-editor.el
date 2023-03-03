;;; dotemacs-editor.el --- Editor enhancements -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package smartparens
  :diminish smartparens-mode
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  :config (smartparens-global-mode))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :init (which-key-mode)
  :config (which-key-setup-side-window-bottom))

(use-package helpful
  :config
  (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-key] #'helpful-key)
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package magit
  :custom (magit-diff-refine-hunk t))

(use-package diff-hl
  :after magit
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh))
  :config (global-diff-hl-mode))

(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :hook (dotemacs-prog-mode . drag-stuff-mode)
  :config (drag-stuff-define-keys))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :hook (dotemacs-prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (dotemacs-prog-mode . highlight-numbers-mode))

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character) ; or 'bitmap or 'column
  (highlight-indent-guides-suppress-auto-error t)
  (highlight-indent-guides-auto-odd-face-perc 1.5)
  (highlight-indent-guides-auto-even-face-perc 1.5)
  (highlight-indent-guides-auto-character-face-perc 35))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80))

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode))

(use-package yasnippet-snippets)
(use-package crux)
(use-package multiple-cursors)
(use-package expand-region)


(provide 'dotemacs-editor)
;;; dotemacs-editor.el ends here
