;;; dotemacs-checkers.el --- Code/Spell checkers -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'dotemacs-modes)

(add-hook 'dotemacs-prog-mode 'flymake-mode)

(use-package ispell
  :elpaca nil
  :if (> (length dotemacs-hunspell-dict-list) 0)
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary (car dotemacs-hunspell-dict-list))
  :commands ispell-set-spellchecker-params
  :config
  (dolist (dict dotemacs-hunspell-dict-list)
    (add-to-list
     'ispell-local-dictionary-alist
     `(,dict "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" ,dict) nil utf-8)))
  (add-to-list 'ispell-skip-region-alist
               '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (ispell-set-spellchecker-params))

(use-package flyspell
  :elpaca nil
  :hook (dotemacs-text-mode . flyspell-mode)
  :preface
  (defun dotemacs-hunspell-set-local-dict (dict)
    "Set ispell DICT for current buffer."
    (flyspell-mode -1)
    (setq-local ispell-local-dictionary dict)
    (flyspell-mode +1)
    (flyspell-buffer))
  (defun dotemacs-consult-hunspell-dict ()
    "Consult interface for dictionary selection."
    (interactive)
    (dotemacs-hunspell-set-local-dict
     (consult--read
      dotemacs-hunspell-dict-list
      :prompt "Change dictionary:"
      :require-match t
      :history t
      :sort nil))))

(use-package flyspell-correct :after flyspell)

(use-package flymake-eslint
  :preface
  (require 'project)
  (defun dotemacs-project-npm-bin-path ()
    "Get the local npm project bin path if exists."
    (when-let* ((proj (project-current))
                (bin-path (file-name-concat
                           (project-root proj) "node_modules" ".bin"))
                (e (file-exists-p bin-path)))
      bin-path))
  (defun dotemacs-project-npm-bin-p (appname)
    "Verify if APPNAME is installed for a npm project."
    (when-let ((bin-path (dotemacs-project-npm-bin-path)))
      (file-exists-p (file-name-concat bin-path appname))))
  (defun dotemacs-enable-eslint ()
    "Enable eslint only if its installed for the project."
    (interactive)
    (when (dotemacs-project-npm-bin-p flymake-eslint-executable-name)
      (make-local-variable 'exec-path)
      (push (dotemacs-project-npm-bin-path) exec-path)
      (flymake-eslint-enable))))


(provide 'dotemacs-checkers)
;;; dotemacs-checkers.el ends here
