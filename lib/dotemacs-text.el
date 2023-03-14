;;; dotemacs-text.el --- Plain Text -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'dotemacs-modes)

(use-package org
  :elpaca nil
  :commands (org-capture org-agenda)
  :hook (org-mode . dotemacs-text-mode)
  :custom
  (org-ellipsis " ⤵")
  (org-catch-invisible-edits 'show-and-error)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-adapt-indentation nil)
  :init
  (when dotemacs-use-variable-pitch-in-org
    (customize-set-variable 'org-auto-align-tags nil)
    (customize-set-variable 'org-tags-column 0)
    (add-hook 'org-mode-hook 'variable-pitch-mode))
  :custom-face
  (org-block ((t (:inherit 'fixed-pitch))))
  (org-block-begin-line ((t (:inherit 'fixed-pitch))))
  (org-block-end-line ((t (:inherit 'fixed-pitch))))
  (org-code ((t (:inherit 'fixed-pitch))))
  (org-document-info-keyword ((t (:inherit 'fixed-pitch))))
  (org-meta-line ((t (:inherit 'fixed-pitch))))
  (org-table ((t (:inherit 'fixed-pitch))))
  ;; (org-table-header ((t (:inherit 'fixed-pitch))))
  (org-verbatim ((t (:inherit 'fixed-pitch))))
  (org-formula ((t (:inherit 'fixed-pitch))))
  (org-special-keyword ((t (:inherit 'fixed-pitch))))
  (org-checkbox ((t (:inherit 'fixed-pitch)))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :custom
  (org-modern-star ["❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽"])
  (org-modern-block-fringe nil)
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :when (and (bound-and-true-p dotemacs-roam-dir)
             (file-exists-p dotemacs-roam-dir))
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory dotemacs-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  :config (org-roam-setup))

(use-package markdown-mode
  :hook ((markdown-mode . dotemacs-text-mode)
         (gfm-mode . dotemacs-text-mode))
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc") ; or multimarkdown
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t)
  :init
  (when dotemacs-use-variable-pitch-in-md
    (add-hook 'markdown-mode-hook 'variable-pitch-mode)
    (add-hook 'gfm-mode-hook 'variable-pitch-mode))
  :custom-face
  (markdown-code-face ((t (:inherit 'fixed-pitch))))
  (markdown-html-attr-name-face ((t (:inherit 'fixed-pitch))))
  (markdown-html-attr-value-face ((t (:inherit 'fixed-pitch))))
  (markdown-html-entity-face ((t (:inherit 'fixed-pitch))))
  (markdown-html-tag-delimiter-face ((t (:inherit 'fixed-pitch))))
  (markdown-html-tag-name-face ((t (:inherit 'fixed-pitch))))
  (markdown-inline-code-face ((t (:inherit 'fixed-pitch))))
  (markdown-language-info-face ((t (:inherit 'fixed-pitch))))
  (markdown-language-keyword-face ((t (:inherit 'fixed-pitch))))
  (markdown-pre-face ((t (:inherit 'fixed-pitch))))
  (markdown-table-face ((t (:inherit 'fixed-pitch)))))

(use-package auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config (setq-default TeX-master nil))

(use-package htmlize)
(use-package pdf-tools :defer t)

(provide 'dotemacs-text)
;;; dotemacs-text.el ends here
