;;; dotemacs-modeline.el --- Modeline customizations -*- lexical-binding: t; -*-

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

;; This is just for my personal configuration and not intended to be a package.

;;; Credits:

;; Based on:
;; - mood-line by Jessie Hildebrandt
;; - simple-modeline by Eder Elorriaga

;;; Code:

(defgroup dotemacs-modeline nil
  "Custom mode-line."
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-font-awesome "Font Awesome 6 Free"
  "Font awesome name.  Usually \"Font Awesome 6 Free\" or \"FontAwesome\"."
  :group 'dotemacs-modeline
  :type 'string)

(defface dotemacs-modeline-muted '((t (:inherit 'shadow)))
  "Mode-line muted face."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-icon-muted
  `((t (:family ,dotemacs-modeline-font-awesome :inherit 'shadow)))
  "Mode-line muted icon face."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-icon-accented
  `((t (:family ,dotemacs-modeline-font-awesome
                :inherit 'font-lock-constant-face)))
  "Mode-line accented icon face."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-icon-warning
  `((t (:family ,dotemacs-modeline-font-awesome :inherit 'error :bold nil)))
  "Mode-line warning icon face."
  :group 'dotemacs-modeline)

(defvar dotemacs-modeline-active-buffer-id nil
  "State of active buffer id.")

(defun dotemacs-modeline-set-active-buffer-id ()
  "Set the state of active buffer id."
  (setq dotemacs-modeline-active-buffer-id (selected-window)))

(defun dotemacs-modeline-update ()
  "Force update the mode-line."
  (force-mode-line-update t))

(defun dotemacs-modeline-space (content)
  "Add extra space on CONTENT sides."
  (concat " " content " "))

(defun dotemacs-modeline-mute (content)
  "Add muted face to CONTENT."
  (propertize content 'face 'dotemacs-modeline-muted))

(defun dotemacs-modeline-icon-accent (content)
  "Add accented icon face to CONTENT."
  (propertize content
              'face 'dotemacs-modeline-icon-accented
              'local-map (make-mode-line-mouse-map 'mouse-1 'menu-bar-open)))

(defun dotemacs-modeline-icon-warn (content)
  "Add warning icon face to CONTENT."
  (propertize content 'face 'dotemacs-modeline-icon-warning))

(defun dotemacs-modeline-icon-mute (content)
  "Add muted icon face to CONTENT."
  (propertize content 'face 'dotemacs-modeline-icon-muted))

(defun dotemacs-modeline-format (left right)
  "Format the mode-line in two parts and align it on LEFT and RIGHT."
  (concat
   left
   (propertize
    " "
    'display `((space :align-to (- (+ right right-fringe right-margin)
                                   ,(length right)))))
   right))

(defun dotemacs-modeline-active-buffer-segment ()
  "Indicate the active buffer and modified status."
  (let* ((activep (eq dotemacs-modeline-active-buffer-id (selected-window)))
         (modifiedp (and (not buffer-read-only) (buffer-modified-p)))
         (icon (if activep "" "")))
    (if modifiedp
        (dotemacs-modeline-icon-warn icon)
      (if activep
          (dotemacs-modeline-icon-accent icon)
        (dotemacs-modeline-icon-mute icon)))))

(defun dotemacs-modeline-file-status-segment ()
  "Display file properties (good cases are omitted)."
  (let ((lock (if (not buffer-read-only) "" ""))
        (eol (pcase (coding-system-eol-type buffer-file-coding-system)
               (0 "") (1 "") (2 ""))))
    (dotemacs-modeline-icon-mute (concat lock eol))))

(defun dotemacs-modeline-major-mode-segment ()
  "Display major mode."
  (dotemacs-modeline-space (format-mode-line mode-name 'bold)))

(defun dotemacs-modeline-minor-mode-segment ()
  "Display minor modes."
   (replace-regexp-in-string
    "%" "%%%%"
    (string-trim (format-mode-line minor-mode-alist))
    t t))

(defun dotemacs-modeline-misc-segment ()
  "Display misc info."
  (let ((misc (string-trim (format-mode-line mode-line-misc-info))))
    (unless (string= misc "")
      (dotemacs-modeline-space misc))))

(defun dotemacs-modeline-process-segment ()
  "Display process info."
  (when mode-line-process
    (dotemacs-modeline-space
     (string-trim (format-mode-line mode-line-process)))))

(defun dotemacs-modeline-very-important-segment ()
  "Display the most important feature of Emacs."
  (when (and (fboundp 'nyan-create) (bound-and-true-p nyan-mode))
    (list (nyan-create))))

(defun dotemacs-modeline-encoding-segment ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (dotemacs-modeline-mute
   (let ((sys (coding-system-plist buffer-file-coding-system)))
     (cond ((memq (plist-get sys :category)
                  '(coding-category-undecided coding-category-utf-8))
            "UTF-8")
           (t (upcase (symbol-name (plist-get sys :name))))))))

(defvar dotemacs-modeline-backup mode-line-format)

;;;###autoload
(define-minor-mode dotemacs-modeline-mode
  "Toggle dotemacs-modeline."
  :group 'dotemacs-modeline
  :global t
  (if dotemacs-modeline-mode
      (progn
        (add-hook 'post-command-hook #'dotemacs-modeline-set-active-buffer-id)
        (add-hook 'buffer-list-update-hook #'dotemacs-modeline-update)
        (setq-default
         mode-line-format
         '(:eval
           (dotemacs-modeline-format
            (format-mode-line
             '((:propertize " " display (raise 0.15))
               (:eval (dotemacs-modeline-active-buffer-segment))
               (:propertize "  %b " face mode-line-buffer-id)
               (:eval (dotemacs-modeline-file-status-segment))
               (:propertize " (%l,%c) " face dotemacs-modeline-muted)
               (:eval (dotemacs-modeline-encoding-segment))
               (:propertize " ")
               (:eval (dotemacs-modeline-very-important-segment))))
            (format-mode-line
             '((:eval (dotemacs-modeline-process-segment))
               (:eval (dotemacs-modeline-misc-segment))
               (:eval (dotemacs-modeline-major-mode-segment))
               (:eval (dotemacs-modeline-minor-mode-segment))
               (vc-mode vc-mode)
               (:propertize " " display (raise -0.15))))
            ))))
    (progn
      (remove-hook 'post-command-hook #'dotemacs-modeline-set-active-buffer-id)
      (remove-hook 'buffer-list-update-hook #'dotemacs-modeline-update)
      (setq-default mode-line-format dotemacs-modeline-backup))))


(provide 'dotemacs-modeline)
;;; dotemacs-modeline.el ends here
