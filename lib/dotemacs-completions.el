;;; dotemacs-completions.el --- Completions -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-category-defaults nil))

(use-package embark
  :custom (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :custom (completion-in-region-function #'consult-completion-in-region))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/*"))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  (corfu-auto nil) ; call with C-M-i
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first nil)
  (corfu-popupinfo-delay 0.2)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Sanitize the `pcomplete-completions-at-point' Capf.
  ;; The Capf has undesired side effects on Emacs 28 and earlier.

  ;; Wrap a chatty Capf and silence it.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Purify a broken Capf and ensure that it does not modify the buffer.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package consult-project-extra)
(use-package consult-eglot)


(provide 'dotemacs-completions)
;;; dotemacs-completions.el ends here
