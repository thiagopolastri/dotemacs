;;; early-init.el --- Emacs early init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;;; Commentary:

;; Use `customize-group' to change `emacs-custom-config' (local) and
;; `github-primer-theme' (faces).

;;; Code:

;; Bootstrap
;; - Set garbage collector size for faster startup.
;; - Load newest compiled elisp files.
;; - Disable package.el startup.
;; - Set native compilation.
;; - Set default coding system to UTF-8.
;; - Load (or create and load) custom file.

(setq gc-cons-threshold (* 50 1000 1000))
(customize-set-variable 'load-prefer-newer noninteractive)
(customize-set-variable 'package-enable-at-startup nil)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)

  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path
                     (convert-standard-filename
                      (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache
       (convert-standard-filename
        (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(customize-set-variable 'custom-file
                        (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Custom variables

(defgroup emacs-custom-config nil
  "Custom Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom emacs-custom-config:font-size 120
  "Emacs font size.  integer in units of 1/10 point (140 = 14pt)."
  :group 'emacs-custom-config
  :type 'integer)

(defcustom emacs-custom-config:font-fixed "Monospace"
  "Emacs fixed font.  Mono or code variant."
  :group 'emacs-custom-config
  :type 'string)

(defcustom emacs-custom-config:font-variable "Sans Serif"
  "Emacs variable font.  Sans or Serif."
  :group 'emacs-custom-config
  :type 'string)

(defcustom emacs-custom-config:hunspell-dict-list '()
  "List of installed hunspell dictionaries.
You can list available dictionaries with the command `hunspell -D' in
your terminal.  Use only the locale name as a value (Eg.  en_US).
The default dictionary will be the first item."
  :group 'emacs-custom-config
  :type '(repeat string))

(defcustom emacs-custom-config:roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'emacs-custom-config
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

(defcustom emacs-custom-config:replace-modes-ts nil
  "Replace modes with the treesitter alternatives (Emacs 29).
Install treesitter and parsers to use."
  :group 'emacs-custom-config
  :type 'boolean)

;; Early visual settings
;; - Remove initial message on scratch buffer.
;; - Remove tool-bar, menu-bar and scrolls.
;; - Set custom fonts

(setq inhibit-startup-message t)
(customize-set-variable 'initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default
                    nil
                    :family emacs-custom-config:font-fixed
                    :height emacs-custom-config:font-size)
(set-face-attribute 'fixed-pitch
                    nil
                    :family emacs-custom-config:font-fixed
                    :height emacs-custom-config:font-size)
(set-face-attribute 'fixed-pitch-serif
                    nil
                    :family emacs-custom-config:font-fixed
                    :height emacs-custom-config:font-size)
(set-face-attribute 'variable-pitch
                    nil
                    :family emacs-custom-config:font-variable
                    :height emacs-custom-config:font-size)

;; Load theme

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(load-theme 'github-primer t)

;; Load custom modeline

(add-to-list 'load-path
             (expand-file-name "modules" user-emacs-directory))
(require 'custom-ml)
(custom-ml-mode 1)

;;; early-init.el ends here
