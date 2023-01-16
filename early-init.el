;;; early-init.el --- Emacs early init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Use `customize-group' to change `dotemacs' (local) and
;; `github-primer-theme' (faces).

;;; Code:

;; Bootstrap
;; - Increase garbage collector size for faster startup.
;; - Load newest compiled elisp files.
;; - Disable package.el startup.
;; - supress warnings
;; - Define time format
;; - Load custom variables and functions (dotemacs)
;; - Set native compilation.
;; - Set default coding system to UTF-8.
;; - Load (or create and load) custom file.

(setq gc-cons-threshold 100000000)
(customize-set-variable 'load-prefer-newer noninteractive)
(customize-set-variable 'package-enable-at-startup nil)
(customize-set-variable 'byte-compile-warnings '(not obsolete))
(customize-set-variable 'warning-suppress-log-types '((comp) (bytecomp)))
(customize-set-variable 'display-time-default-load-average nil)
(customize-set-variable 'display-time-format "%d/%m/%Y %H:%M")

(add-to-list 'load-path
             (convert-standard-filename
              (expand-file-name "modules" user-emacs-directory)))

(require 'dotemacs)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (customize-set-variable 'native-comp-async-report-warnings-errors nil)
  (customize-set-variable 'native-comp-deferred-compilation t)
  (let ((cache (dotemacs:get-path "var/eln-cache/")))
    (add-to-list 'native-comp-eln-load-path cache)
    (startup-redirect-eln-cache cache)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(customize-set-variable 'custom-file (dotemacs:get-path "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Early visual settings
;; - Remove initial message on scratch buffer.
;; - Remove tool-bar, menu-bar and scrolls.
;; - Set custom fonts

(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default
                    nil
                    :family dotemacs:font-fixed
                    :height dotemacs:font-size)
(set-face-attribute 'fixed-pitch
                    nil
                    :family dotemacs:font-fixed
                    :height dotemacs:font-size)
(set-face-attribute 'fixed-pitch-serif
                    nil
                    :family dotemacs:font-fixed
                    :height dotemacs:font-size)
(set-face-attribute 'variable-pitch
                    nil
                    :family dotemacs:font-variable
                    :height dotemacs:font-size)

;; Load theme

(add-to-list 'custom-theme-load-path (dotemacs:get-path "themes"))
(load-theme 'github-primer t)

;; Load custom modeline

(require 'custom-ml)
(custom-ml-mode 1)

;;; early-init.el ends here
