;;; dotemacs-defaults.el --- Defaults -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs-modes)

(custom-set-variables
 '(cursor-type 'bar)
 '(use-short-answers t)
 '(y-or-n-p-use-read-key t)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(visible-bell 1)
 '(use-dialog-box nil)
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(tab-width 4)
 '(require-final-newline t)
 '(word-wrap t)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(frame-resize-pixelwise t)
 '(find-file-visit-truename t)
 '(vc-follow-symlinks t)
 '(vc-handled-backends '(Git))
 '(kill-buffer-query-functions nil)
 '(revert-without-query t)
 '(make-backup-files nil)
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(uniquify-buffer-name-style 'forward)
 '(large-file-warning-threshold 100000000) ; 100MB
 '(switch-to-buffer-obey-display-actions t)
 '(enable-recursive-minibuffers t)
 '(tab-always-indent 'complete)
 '(display-time-default-load-average nil)
 '(display-time-format "%d/%m/%Y %H:%M")
 '(scroll-step 1)
 '(scroll-conservatively 10000)
 '(auto-window-vscroll nil)
 '(bidi-paragraph-direction 'left-to-right)
 '(bidi-inhibit-bpa t)
 ;; autorevert
 '(global-auto-revert-non-file-buffers t)
 '(auto-revert-interval 3)
 '(auto-revert-check-vc-info t)
 ;; savehist
 '(history-length 25)
 ;; dired
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-listing-switches "-valho --group-directories-first")
 ;; eldoc: Prevent echo area resize and always prefer buffer (C-h .)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 ;; whitespace
 '(whitespace-style '(face tabs empty trailing tab-mark indentation::space))
 '(whitespace-action '(auto-cleanup)) ; clean on save
 ;; display-fill-column-indicator
 '(display-fill-column-indicator-character ?\u258F)
 ;; eglot
 '(eglot-autoshutdown t)
 ;; proceed
 '(proced-auto-update-flag t)
 '(proced-auto-update-interval 1)
 '(proced-enable-color-flag t)
 ;; project
 '(project-vc-ignores '("target/"
                       "bin/"
                       "obj/"
                       "node_modules/"
                       ".vscode/"))
 '(project-vc-extra-root-markers '(".dir-locals.el"
                                  "package.json"
                                  "cargo.toml"
                                  "pom.xml"
                                  "*.csproj")))

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

(add-hook 'prog-mode-hook 'dotemacs-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; use ( to show details
(add-hook 'dired-mode-hook 'hl-line-mode)

(add-hook 'dotemacs-prog-mode-hook 'glyphless-display-mode)
(add-hook 'dotemacs-prog-mode-hook 'whitespace-mode)
(add-hook 'dotemacs-prog-mode-hook 'display-line-numbers-mode)
(add-hook 'dotemacs-prog-mode-hook 'hl-line-mode)
(add-hook 'dotemacs-prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'dotemacs-prog-mode-hook 'hs-minor-mode) ; hideshow

(add-hook 'dotemacs-text-mode-hook 'visual-line-mode)

(diminish 'hs-minor-mode)

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
