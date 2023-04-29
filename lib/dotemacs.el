;;; dotemacs.el --- Init group and functions -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'cl-macs)

(defgroup dotemacs nil
  "Init Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom dotemacs-font-size 120
  "Emacs font size.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-size-variable 120
  "Emacs font size (variable).  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-fixed "Monospace"
  "Emacs fixed font.  Mono or code variant."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-variable "Sans Serif"
  "Emacs variable font.  Sans or Serif."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-hunspell-dict-list '()
  "List of installed hunspell dictionaries.
You can list available dictionaries with the command `hunspell -D' in
your terminal.  Use only the locale name as a value (Eg.  en_US).
The default dictionary will be the first item."
  :group 'dotemacs
  :type '(repeat string))

(defcustom dotemacs-roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'dotemacs
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

(defcustom dotemacs-use-variable-pitch-in-org t
  "Use variable pitch in org mode."
  :group 'dotemacs
  :type 'boolean)

(defcustom dotemacs-use-variable-pitch-in-md t
  "Use variable pitch in markdown mode."
  :group 'dotemacs
  :type 'boolean)

(defun dotemacs-get-path (file)
  "Convert and get a FILE path relative to `user-emacs-directory'."
  (convert-standard-filename (expand-file-name file user-emacs-directory)))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (require 'treesit))

(cl-defun dotemacs-use-treesit (&key lang github path remap mode)
  "Setup treesiter for a given language.
LANG - language to setup (symbol)
GITHUB - github path to grammar (only user/repo)
PATH - path to src inside github repository
REMAP - list to add to `major-mode-remap-alist'
MODE - list to add to `auto-mode-alist'"
  (let ((tsp (and (fboundp 'treesit-available-p) (treesit-available-p))))
    (when (and tsp lang github)
      (unless (boundp 'treesit-language-source-alist)
        (setq treesit-language-source-alist '()))
      (add-to-list
       'treesit-language-source-alist
       `(,lang . (,(concat "https://github.com/" github) nil ,path))))
    (when (and tsp lang remap (treesit-ready-p lang t))
      (add-to-list 'major-mode-remap-alist remap))
    (when (and tsp lang mode (treesit-ready-p lang t))
      (add-to-list 'auto-mode-alist mode))))

(provide 'dotemacs)
;;; dotemacs.el ends here
