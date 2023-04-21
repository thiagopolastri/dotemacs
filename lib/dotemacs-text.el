;;; dotemacs-text.el --- Plain Text -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'dotemacs-modes)
(require 'org)

(add-hook 'rst-mode-hook 'dotemacs-text-mode)
(add-hook 'org-mode-hook 'dotemacs-text-mode)

(customize-set-variable 'org-ellipsis " ⤵")
(customize-set-variable 'org-catch-invisible-edits 'show-and-error)
(customize-set-variable 'org-hide-emphasis-markers t)
(customize-set-variable 'org-pretty-entities t)
(customize-set-variable 'org-special-ctrl-a/e t)
(customize-set-variable 'org-insert-heading-respect-content t)
(customize-set-variable 'org-adapt-indentation nil)
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-edit-src-content-indentation 0)
(customize-set-variable 'org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
(customize-set-variable 'org-fontify-quote-and-verse-blocks t)
(customize-set-variable 'org-enforce-todo-dependencies t)
(customize-set-variable 'org-enforce-todo-checkbox-dependencies t)
(customize-set-variable 'org-agenda-start-with-log-mode t)
(customize-set-variable 'org-log-done 'time)
(customize-set-variable 'org-log-reschedule 'time)
(customize-set-variable 'org-log-redeadline 'time)
(customize-set-variable 'org-log-into-drawer t)
;; Latex
(customize-set-variable 'org-highlight-latex-and-related '(native))
(customize-set-variable 'org-latex-compiler "xelatex")
(customize-set-variable 'org-latex-listings t)
(customize-set-variable 'org-latex-pdf-process
                        (list (concat "latexmk -"
                                      org-latex-compiler
                                      " -recorder -synctex=1 -bibtex-cond %b")))
;; TODOs
(customize-set-variable 'org-todo-keywords '((sequence "TODO(t)"
                                                       "STARTED(s)"
                                                       "WAITING(w)"
                                                       "|"
                                                       "DONE(d)"
                                                       "CANCELLED(c)")))

(when dotemacs-use-variable-pitch-in-org
    (customize-set-variable 'org-auto-align-tags nil)
    (customize-set-variable 'org-tags-column 0)
    (add-hook 'org-mode-hook 'variable-pitch-mode))

(add-to-list 'org-latex-packages-alist '("" "listings" t))

(use-package visual-fill-column
  ;; `visual-line-mode' are already hooked on `dotemacs-text-mode'.
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom (pdf-view-display-size 'fit-page)
  :config (pdf-tools-install))

(use-package htmlize :defer t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-block-fringe nil)
  (org-modern-table nil) ; valign do this better with variable-pitch
  (org-modern-star ["❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽"]))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom (valign-fancy-bar t)
  :diminish valign-mode)

(use-package org-roam
  :when (and (bound-and-true-p dotemacs-roam-dir)
             (file-exists-p dotemacs-roam-dir))
  :custom
  (org-roam-directory dotemacs-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  :config
  (org-roam-setup)
  (add-to-list
   'org-roam-capture-templates
   '("t" "task" plain
     "* %?\n %a"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))))

(use-package markdown-mode
  :hook (((markdown-mode gfm-mode) . dotemacs-text-mode)
         ((markdown-mode gfm-mode) . valign-mode))
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc")
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
    (add-hook 'gfm-mode-hook 'variable-pitch-mode)))

(use-package pandoc-mode
  :after (markdown-mode hydra)
  :hook (markdown-mode . pandoc-mode))

(use-package auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . dotemacs-text-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config (setq-default TeX-master nil))


(provide 'dotemacs-text)
;;; dotemacs-text.el ends here
