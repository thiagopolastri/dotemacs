;;; dotemacs-terminal.el --- Terminal Emulator -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package vterm
  :defer t
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 100000))

;; `elpaca-wait' here could solve the build problem but add a good amount of
;; seconds on initialization.

(use-package multi-vterm
  :defer t
  :after vterm)


(provide 'dotemacs-terminal)
;;; dotemacs-terminal.el ends here
