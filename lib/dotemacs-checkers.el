;;; dotemacs-checkers.el --- Code/Spell checkers -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'dotemacs-modes)

(use-package jinx
  :hook (dotemacs-text-mode . jinx-mode))

(use-package flymake-eslint
  :defer t
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
