;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-quickstart nil
      package-enable-at-startup nil
      load-prefer-newer t)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (customize-set-variable 'native-comp-async-report-warnings-errors 'silent)
  (customize-set-variable 'native-comp-jit-compilation t)
  (let ((cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
    (add-to-list 'native-comp-eln-load-path cache)
    (startup-redirect-eln-cache cache)))

(customize-set-variable 'custom-file
                        (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(defgroup dotemacs nil
  "Init Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom dotemacs-font-fixed "Monospace"
  "Emacs fixed font.  Mono or code variant."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-fixed 120
  "Emacs font size.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-variable "Sans Serif"
  "Emacs variable font.  Sans or Serif."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-variable 120
  "Emacs font size (variable).  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-org-title "Sans Serif"
  "Font to use in Org titles."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-org-title 140
  "Font size to use in Org titles.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-openai-key nil
  "Api key for OpenAI (chatgpt)."
  :group 'dotemacs
  :type '(choice (string :tag "OpenAI API key")
                 (const :tag "None" nil)))

(defcustom dotemacs-roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'dotemacs
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

(set-face-attribute 'default
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'fixed-pitch
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'fixed-pitch-serif
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'variable-pitch
                    nil
                    :family dotemacs-font-variable
                    :height dotemacs-font-size-variable)

(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'initial-scratch-message "")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-charset-priority 'unicode)
