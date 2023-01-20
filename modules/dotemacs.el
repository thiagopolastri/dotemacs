;;; dotemacs.el --- Init group and functions -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'display-line-numbers)
(require 'flyspell)
(require 'project)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (require 'treesit))

;; Group

(defgroup dotemacs nil
  "Init Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom dotemacs:font-size 120
  "Emacs font size.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs:font-fixed "Monospace"
  "Emacs fixed font.  Mono or code variant."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs:font-variable "Sans Serif"
  "Emacs variable font.  Sans or Serif."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs:hunspell-dict-list '()
  "List of installed hunspell dictionaries.
You can list available dictionaries with the command `hunspell -D' in
your terminal.  Use only the locale name as a value (Eg.  en_US).
The default dictionary will be the first item."
  :group 'dotemacs
  :type '(repeat string))

(defcustom dotemacs:roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'dotemacs
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

;; functions

(defun dotemacs:get-path (file)
  "Convert and get a FILE relative to `user-emacs-directory'."
  (convert-standard-filename (expand-file-name file user-emacs-directory)))

(defun dotemacs:startup-profile-message ()
  "Show a startup profile message."
  (let ((package-count 0))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count
            (+ (hash-table-count straight--profile-cache) package-count)))
    (if (zerop package-count)
        (message "Emacs loaded in %s with %d garbage collections."
                 (format
                  "%.2f seconds"
                  (float-time (time-subtract after-init-time before-init-time)))
                 gcs-done)
      (message "Emacs loaded %d packages in %s with %d garbage collections."
               package-count
               (format
                "%.2f seconds"
                (float-time (time-subtract after-init-time before-init-time)))
               gcs-done))))

(defun dotemacs:hunspell-set-local-dict (dict)
  "Set ispell DICT for current buffer."
  (flyspell-mode -1)
  (setq-local ispell-local-dictionary dict)
  (flyspell-mode +1)
  (flyspell-buffer))

;; Interactive

(defun dotemacs:toggle-line-numbers-type ()
  "Toggle between normal and relative line-numbers."
  (interactive)
  (display-line-numbers-mode -1)
  (if (eq display-line-numbers-type 'relative)
      (setq-local display-line-numbers-type t)
    (setq-local display-line-numbers-type 'relative))
  (display-line-numbers-mode +1))

(defvar dotemacs:fullscreen-p nil
  "Indicate if Emacs is in fullscreen.")

(defun dotemacs:toggle-fullscreen ()
  "Toggle frame fullscreen and display time."
  (interactive)
  (setq dotemacs:fullscreen-p (not dotemacs:fullscreen-p))
  (toggle-frame-fullscreen)
  (if dotemacs:fullscreen-p
      (display-time-mode +1)
    (display-time-mode -1)))

(defun dotemacs:use-treesit (&rest args)
  "Setup treesiter with ARGS for a given language.
:lang - language to setup (symbol)
:github - github path to grammar (only user/repo)
:path - path to src inside github repository
:remap - list to add to `major-mode-remap-alist'
:mode - list to add to `auto-mode-alist'"
  (let ((cond (and (fboundp 'treesit-available-p) (treesit-available-p)))
        (lang (plist-get args :lang))
        (repo (plist-get args :github))
        (path (plist-get args :path))
        (remap (plist-get args :remap))
        (mode (plist-get args :mode)))
    (when (and cond lang repo)
      (unless (boundp 'treesit-language-source-alist)
        (setq treesit-language-source-alist '()))
      (add-to-list
       'treesit-language-source-alist
       `(,lang . (,(concat "https://github.com/" repo) nil ,path))))
    (when (and cond lang remap (treesit-ready-p lang t))
      (add-to-list 'major-mode-remap-alist remap))
    (when (and cond lang mode (treesit-ready-p lang t))
      (add-to-list 'auto-mode-alist mode))))

(defun dotemacs:treesit-available-p (lang)
  "Verify is treesitter is avalable and ready for LANG."
  (if (fboundp 'treesit-available-p)
      (and (treesit-available-p) (treesit-ready-p lang t))
    nil))

(defun dotemacs:project-npm-bin-path ()
  "Get the local npm project bin path if exists."
  (when-let* ((proj (project-current))
              (bin-path (file-name-concat
                         (project-root proj) "node_modules" ".bin"))
              (file-exists-p bin-path))
    bin-path))

(defun dotemacs:project-eslint-p ()
  "Verify if eslint is installed for project."
  (when-let ((bin-path (dotemacs:project-npm-bin-path)))
    (file-exists-p (file-name-concat bin-path "eslint"))))

;; Modes

;; Some modes that I want to treat as `prog-mode' actually are derived from
;; `text-mode' (like XML and others data files).
;; I'll create two modes to hook stuff on it and leave `text-mode' alone.
(define-minor-mode dotemacs:prog-mode
  "Stub mode with modes that should be hooked in `prog-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(define-minor-mode dotemacs:text-mode
  "Stub mode with modes that should be hooked in `text-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(provide 'dotemacs)
;;; dotemacs.el ends here
