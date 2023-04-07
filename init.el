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

;; elpaca will get stuck on multi-vterm building task, but everything works.
;; package first run  - 184.74 seconds with 5 garbage collections
;; package normal run - 2.92 seconds with 2 garbage collections
;; elpaca first run   - 12.84 seconds with 1 garbage collections
;; elpaca normal run  - 0.26 seconds with 1 garbage collections

;; (require 'dotemacs-package) ; Replace :elpaca nil with :ensure nil
(require 'dotemacs-elpaca) ; Replace :ensure nil with :elpaca nil

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

;; TODO: maybe do this on early-init with after-init-hook
(run-with-idle-timer 4 nil
                     (lambda ()
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1)
                       (garbage-collect)))

;;; init.el ends here
