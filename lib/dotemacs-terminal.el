;;; dotemacs-terminal.el --- Terminal Emulator -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 100000))

(when dotemacs-use-elpaca
  (elpaca-wait))

(use-package multi-vterm :after vterm)


(provide 'dotemacs-terminal)
;;; dotemacs-terminal.el ends here
