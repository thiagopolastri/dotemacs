;;; dotemacs-modes.el --- Custom modes -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some modes that I want to treat as `prog-mode' actually are derived from
;; `text-mode' (like XML and others data files).
;; I'll create two modes to hook stuff on it and leave `text-mode' alone.

;;; Code:

(require 'eglot)

(define-minor-mode dotemacs-prog-mode
  "Stub mode with modes that should be hooked in `prog-mode'."
  :init-value nil
  :keymap (make-sparse-keymap)
  ;; This will also start Eglot only in modes that are supported.
  ;; I don't like the fact that this conditional will do an extra lookup
  ;; on `eglot-server-programs' list. But it works fine.
  (when (and (not (eq major-mode 'clojure-mode)) ; use Cider
             (not (eq major-mode 'lisp-mode)) ; use Sly
             (not (eq major-mode 'scheme-mode)) ; use Geiser
             (eglot--lookup-mode major-mode))
    (eglot-ensure)))

(define-minor-mode dotemacs-text-mode
  "Stub mode with modes that should be hooked in `text-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(provide 'dotemacs-modes)
;;; dotemacs-modes.el ends here
