;;; dotemacs-package.el --- Package management -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(message "Using package.el...")

(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package no-littering)

(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(customize-set-variable
 'auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package diminish)

(unless (package-installed-p 'eglot)
  (package-install 'eglot))


(provide 'dotemacs-package)
;;; dotemacs-package.el ends here
