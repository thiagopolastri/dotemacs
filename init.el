;;; init.el --- Emacs init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs loaded in %s with %d garbage collections."
    (format
     "%.2f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

;; package first run  - Emacs loaded in 161.79 seconds with 1 garbage collections.
;; package normal run - Emacs loaded in 2.97 seconds with 1 garbage collections.
;; elpaca first run   - Emacs loaded in 13.21 seconds with 1 garbage collections.
;; elpaca normal run  - Emacs loaded in 0.35 seconds with 1 garbage collections.

;; (require 'dotemacs-package)
(require 'dotemacs-elpaca)

(require 'dotemacs-defaults)
(require 'dotemacs-editor)
(require 'dotemacs-completions)
(require 'dotemacs-checkers)
(require 'dotemacs-prog)
(require 'dotemacs-text)
(require 'dotemacs-terminal)
(require 'dotemacs-keybinds)

;; hacky and private stuff that I dont want to commit.
(load (dotemacs-get-path "user.el") :no-error) ; 0.07s

(run-with-idle-timer 4 nil
                     (lambda ()
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1)
                       (garbage-collect)))

;;; init.el ends here
