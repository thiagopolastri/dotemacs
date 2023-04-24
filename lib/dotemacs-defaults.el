;;; dotemacs-defaults.el --- Defaults -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs-modes)

(customize-set-variable 'cursor-type 'bar)
(customize-set-variable 'use-short-answers t)
(customize-set-variable 'y-or-n-p-use-read-key t)
(customize-set-variable 'confirm-kill-emacs 'yes-or-no-p)
(customize-set-variable 'visible-bell 1)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'fill-column 80)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'tab-width 4)
(customize-set-variable 'require-final-newline t)
(customize-set-variable 'word-wrap t)
(customize-set-variable 'truncate-lines t)
(customize-set-variable 'truncate-partial-width-windows nil)
(customize-set-variable 'frame-resize-pixelwise t)
(customize-set-variable 'find-file-visit-truename t)
(customize-set-variable 'vc-follow-symlinks t)
(customize-set-variable 'vc-handled-backends '(Git))
(customize-set-variable 'kill-buffer-query-functions nil)
(customize-set-variable 'revert-without-query t)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'uniquify-buffer-name-style 'forward)
(customize-set-variable 'large-file-warning-threshold 100000000) ; 100MB
(customize-set-variable 'switch-to-buffer-obey-display-actions t)
(customize-set-variable 'enable-recursive-minibuffers t)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'display-time-default-load-average nil)
(customize-set-variable 'display-time-format "%d/%m/%Y %H:%M")
(customize-set-variable 'scroll-step 1)
(customize-set-variable 'scroll-conservatively 10000)
(customize-set-variable 'auto-window-vscroll nil)
(customize-set-variable 'bidi-paragraph-direction 'left-to-right)
(customize-set-variable 'bidi-inhibit-bpa t)
;; autorevert
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(customize-set-variable 'auto-revert-interval 3)
(customize-set-variable 'auto-revert-check-vc-info t)
;; savehist
(customize-set-variable 'history-length 25)
;; dired
(customize-set-variable 'dired-auto-revert-buffer t)
(customize-set-variable 'dired-dwim-target t)
(customize-set-variable 'dired-hide-details-hide-symlink-targets nil)
(customize-set-variable 'dired-recursive-copies 'always)
(customize-set-variable 'dired-recursive-deletes 'top)
(customize-set-variable 'dired-listing-switches "-valho --group-directories-first")
;; eldoc: Prevent echo area resize and always prefer buffer (C-h .)
(customize-set-variable 'eldoc-echo-area-use-multiline-p nil)
(customize-set-variable 'eldoc-echo-area-prefer-doc-buffer t)
;; whitespace
(customize-set-variable 'whitespace-style '(face tabs empty trailing tab-mark indentation::space))
(customize-set-variable 'whitespace-action '(auto-cleanup)) ; clean on save
;; display-fill-column-indicator
(customize-set-variable 'display-fill-column-indicator-character ?\u258F)
;; eglot
(customize-set-variable 'eglot-autoshutdown t)
;; proceed
(customize-set-variable 'proced-auto-update-flag t)
(customize-set-variable 'proced-auto-update-interval 1)
(customize-set-variable 'proced-enable-color-flag t)
;; project
(customize-set-variable 'project-vc-ignores '("target/"
                                              "bin/"
                                              "obj/"
                                              "node_modules/"
                                              ".vscode/"))
(customize-set-variable 'project-vc-extra-root-markers '(".dir-locals.el"
                                                         "package.json"
                                                         "cargo.toml"
                                                         "pom.xml"
                                                         "*.csproj"))

(global-unset-key (kbd "C-z"))
(delete-selection-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)

(when (fboundp 'async-bytecomp-package-mode)
  (async-bytecomp-package-mode 1))
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(when (fboundp 'glyphless-display-mode)
  (add-hook 'dotemacs-prog-mode-hook 'glyphless-display-mode)
  (with-eval-after-load 'glyphless-mode (diminish 'glyphless-display-mode)))

(add-hook 'prog-mode-hook 'dotemacs-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; use ( to show details
(add-hook 'dired-mode-hook 'hl-line-mode)

(add-hook 'dotemacs-prog-mode-hook 'whitespace-mode)
(add-hook 'dotemacs-prog-mode-hook 'display-line-numbers-mode)
(add-hook 'dotemacs-prog-mode-hook 'hl-line-mode)
(add-hook 'dotemacs-prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'dotemacs-prog-mode-hook 'hs-minor-mode)

(add-hook 'dotemacs-text-mode-hook 'visual-line-mode)

(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'hideshow (diminish 'hs-minor-mode))
(with-eval-after-load 'whitespace (diminish 'whitespace-mode))

(defun dotemacs-toggle-line-numbers-type ()
  "Toggle between normal and relative line-numbers."
  (interactive)
  (display-line-numbers-mode -1)
  (if (eq display-line-numbers-type 'relative)
      (setq-local display-line-numbers-type t)
    (setq-local display-line-numbers-type 'relative))
  (display-line-numbers-mode +1))


(provide 'dotemacs-defaults)
;;; dotemacs-defaults.el ends here
