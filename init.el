;;; init.el --- Emacs init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar dotemacs-use-elpaca t) ; Just for testing

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
    (require 'dotemacs-elpaca) ; 1.49s with 2 gc (first run 215.13s with 2 gc)
  (require 'dotemacs-package)) ; 2.03s with 2 gc (first run 152.80s with 4 gc)

(require 'dotemacs-defaults)
(require 'dotemacs-editor)
(require 'dotemacs-completions)
(require 'dotemacs-checkers)
(require 'dotemacs-prog)
(require 'dotemacs-text)
(require 'dotemacs-terminal)
(require 'dotemacs-keybinds)

(when (file-exists-p (dotemacs-get-path "user.el"))
    (load (dotemacs-get-path "user.el")))

(setq gc-cons-threshold (expt 2 24) ;; 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
