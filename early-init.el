;;; early-init.el --- Emacs early init file -*- no-byte-compile: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      load-prefer-newer t)

(add-to-list 'load-path
             (convert-standard-filename
              (expand-file-name "lib" user-emacs-directory)))

(require 'dotemacs)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (customize-set-variable 'native-comp-async-report-warnings-errors nil)
  (customize-set-variable 'native-comp-deferred-compilation t)
  (let ((cache (dotemacs-get-path "var/eln-cache/")))
    (add-to-list 'native-comp-eln-load-path cache)
    (startup-redirect-eln-cache cache)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(customize-set-variable 'custom-file (dotemacs-get-path "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size)
(set-face-attribute 'fixed-pitch
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size)
(set-face-attribute 'fixed-pitch-serif
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size)
(set-face-attribute 'variable-pitch
                    nil
                    :family dotemacs-font-variable
                    :height dotemacs-font-size-variable)

(add-to-list 'custom-theme-load-path (dotemacs-get-path "themes"))
(load-theme 'github-primer t)

(require 'dotemacs-modeline)
(dotemacs-modeline-mode 1)

;;; early-init.el ends here
