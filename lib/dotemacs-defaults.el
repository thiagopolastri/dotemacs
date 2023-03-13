;;; dotemacs-defaults.el --- Defaults -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)

(use-package emacs
  :elpaca nil
  :custom
  (cursor-type 'bar)
  (use-short-answers t)
  (confirm-kill-emacs 'yes-or-no-p)
  (visible-bell 1)
  (use-dialog-box nil)
  (fill-column 80)
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (word-wrap t)
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  (frame-resize-pixelwise t)
  (find-file-visit-truename t)
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (kill-buffer-query-functions nil)
  (revert-without-query t)
  (create-lockfiles nil)
  (uniquify-buffer-name-style 'forward)
  (large-file-warning-threshold 100000000) ; 100MB
  (switch-to-buffer-obey-display-actions t)
  (enable-recursive-minibuffers t)
  (tab-always-indent 'complete)
  (display-time-default-load-average nil)
  (display-time-format "%d/%m/%Y %H:%M")
  :init
  (global-unset-key (kbd "C-z"))
  (delete-selection-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-so-long-mode 1)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (when (display-graphic-p)
    (customize-set-variable 'x-select-request-type
                            '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
  (add-hook 'prog-mode-hook 'dotemacs-prog-mode)
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(use-package autorevert
  :elpaca nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  (auto-revert-check-vc-info t)
  :config (global-auto-revert-mode 1))

(use-package savehist
  :elpaca nil
  :custom (history-length 25)
  :config (savehist-mode 1))

(use-package dired
  :elpaca nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-valho --group-directories-first")
  :hook (dired-mode . dired-hide-details-mode)) ; use ( to show details

(use-package eldoc
  :elpaca nil
  :diminish eldoc-mode
  :custom
  ;; Prevent echo area resize and always prefer buffer (C-h .)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package whitespace
  :elpaca nil
  :diminish whitespace-mode
  :custom
  (whitespace-style '(face tabs empty trailing tab-mark indentation::space))
  (whitespace-action '(auto-cleanup)) ; clean on save
  :hook (dotemacs-prog-mode . whitespace-mode))

(use-package display-line-numbers
  :elpaca nil
  :hook (dotemacs-prog-mode . display-line-numbers-mode)
  :preface
  (defun dotemacs-toggle-line-numbers-type ()
    "Toggle between normal and relative line-numbers."
    (interactive)
    (display-line-numbers-mode -1)
    (if (eq display-line-numbers-type 'relative)
        (setq-local display-line-numbers-type t)
      (setq-local display-line-numbers-type 'relative))
    (display-line-numbers-mode +1)))

(use-package hl-line
  :elpaca nil
  :hook ((dotemacs-prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

(use-package display-fill-column-indicator
  :elpaca nil
  :custom (display-fill-column-indicator-character ?\u258F)
  :hook (dotemacs-prog-mode . display-fill-column-indicator-mode))

(use-package hideshow
  :elpaca nil
  :hook (dotemacs-prog-mode . hs-minor-mode))

(use-package visual-line-mode
  :elpaca nil
  :hook (dotemacs-text-mode . visual-line-mode))

(use-package eglot
  :elpaca nil
  :custom (eglot-autoshutdown t)
  :commands (eglot-ensure))

(use-package proced
  :elpaca nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))


(provide 'dotemacs-defaults)
;;; dotemacs-defaults.el ends here
