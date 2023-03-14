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

;; elpaca works better on Emacs >=29.
;; elpaca will get stuck on multi-vterm building task, but everything works.
;; package first run  - 155.02 seconds with 5 garbage collections
;; package normal run - 2.64 seconds with 2 garbage collections
;; elpaca first run   - 13.69 seconds with 2 garbage collections
;; elpaca normal run  - 0.32 seconds with 7 garbage collections

;; (require 'dotemacs-package) ; Replace :elpaca nil with :ensure nil
(require 'dotemacs-elpaca) ; Replace :ensure nil with :elpaca nil

(require 'dotemacs-defaults)
(require 'dotemacs-editor)
(require 'dotemacs-completions)
(require 'dotemacs-checkers)
(require 'dotemacs-prog) ; geiser is disabled
(require 'dotemacs-text)
(require 'dotemacs-terminal)
(require 'dotemacs-keybinds)

;; hacky and private stuff that I dont want to commit.
(load (dotemacs-get-path "user.el") :no-error) ; 0.07s

(setq gc-cons-threshold (expt 2 24) ;; 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
