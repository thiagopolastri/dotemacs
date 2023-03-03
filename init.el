;;; init.el --- Emacs init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar dotemacs-use-elpaca t)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs loaded in %s with %d garbage collections."
    (format
     "%.2f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

(if dotemacs-use-elpaca
    (require 'dotemacs-elpaca)
  (require 'dotemacs-package))

(require 'dotemacs-defaults)
(require 'dotemacs-editor)
(require 'dotemacs-completions)
(require 'dotemacs-checkers)
(require 'dotemacs-prog)
(require 'dotemacs-text)
(require 'dotemacs-terminal)
(require 'dotemacs-keybinds)

(setq gc-cons-threshold (expt 2 24) ;; 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
