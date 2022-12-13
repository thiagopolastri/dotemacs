;;; custom-ml.el --- Custom mode-line -*- lexical-binding: t; -*-

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

;; TODO: add remote file indication (mode-line-remote).
;; TODO: verify frame id (mode-line-frame-identification).
;; TODO: add keyboard macro indicator.
;; TODO: debug unecessary eval

;;; Credits:

;; Based on:
;; - mood-line by Jessie Hildebrandt
;; - simple-modeline by Eder Elorriaga

;;; Code:

(defgroup custom-ml nil
  "Custom mode-line."
  :group 'custom-ml)

(defface custom-ml:muted '((t (:inherit 'shadow)))
  "Mode-line muted face."
  :group 'custom-ml)

(defface custom-ml:icon-muted '((t (:family "Font Awesome 6 Free" :inherit 'shadow)))
  "Mode-line muted icon face."
  :group 'custom-ml)

(defface custom-ml:icon-accented '((t (:family "Font Awesome 6 Free"
                                       :inherit font-lock-constant-face)))
  "Mode-line accented icon face."
  :group 'custom-ml)

(defface custom-ml:icon-warning '((t (:family "Font Awesome 6 Free"
                                      :inherit 'error
                                      :bold nil)))
  "Mode-line warning icon face."
  :group 'custom-ml)

(defvar custom-ml:active-buffer-id nil
  "State of active buffer id.")

(defun custom-ml:set-active-buffer-id ()
  "Set the state of active buffer id."
  (setq custom-ml:active-buffer-id (selected-window)))

(defun custom-ml:update ()
  "Force update the mode-line."
  (force-mode-line-update t))

(defun custom-ml:space (content)
  "Add extra space on CONTENT sides."
  (concat " " content " "))

(defun custom-ml:mute (content)
  "Add muted face to CONTENT."
  (propertize content 'face 'custom-ml:muted))

(defun custom-ml:icon-accent (content)
  "Add accented icon face to CONTENT."
  (propertize content
              'face 'custom-ml:icon-accented
              'local-map (make-mode-line-mouse-map 'mouse-1 'menu-bar-open)))

(defun custom-ml:icon-warn (content)
  "Add warning icon face to CONTENT."
  (propertize content 'face 'custom-ml:icon-warning))

(defun custom-ml:icon-mute (content)
  "Add muted icon face to CONTENT."
  (propertize content 'face 'custom-ml:icon-muted))

(defun custom-ml:format (left right)
  "Format the mode-line in two parts and align it on LEFT and RIGHT."
  (concat
   left
   (propertize
    " "
    'display `((space :align-to (- (+ right right-fringe right-margin)
                                   ,(length right)))))
   right))

(defun custom-ml:active-buffer-segment ()
  "Indicate the active buffer and modified status."
  (let* ((activep (eq custom-ml:active-buffer-id (selected-window)))
         (modifiedp (and (not buffer-read-only) (buffer-modified-p)))
         (icon (if activep "" "")))
    (if modifiedp
        (custom-ml:icon-warn icon)
      (if activep (custom-ml:icon-accent icon) (custom-ml:icon-mute icon)))))

(defun custom-ml:file-status-segment ()
  "Display file properties (good cases are omitted)."
  (let ((lock (if (not buffer-read-only) "" ""))
        (eol (pcase (coding-system-eol-type buffer-file-coding-system)
               (0 "") (1 "") (2 ""))))
    (custom-ml:icon-mute (concat lock eol))))

(defun custom-ml:major-mode-segment ()
  "Display major mode."
  (custom-ml:space (format-mode-line mode-name 'bold)))

(defun custom-ml:minor-mode-segment ()
  "Display minor modes."
   (replace-regexp-in-string
    "%" "%%%%"
    (string-trim (format-mode-line minor-mode-alist))
    t t))

(defun custom-ml:misc-segment ()
  "Display misc info."
  (let ((misc (string-trim (format-mode-line mode-line-misc-info))))
    (unless (string= misc "")
      (custom-ml:space misc))))

(defun custom-ml:process-segment ()
  "Display process info."
  (when mode-line-process
    (custom-ml:space (string-trim (format-mode-line mode-line-process)))))

(defun custom-ml:very-important-segment ()
  "Display the most important feature of Emacs."
  (when (and (fboundp 'nyan-create) (bound-and-true-p nyan-mode))
    (list (nyan-create))))

(defun custom-ml:encoding-segment ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (custom-ml:mute
   (let ((sys (coding-system-plist buffer-file-coding-system)))
     (cond ((memq (plist-get sys :category)
                  '(coding-category-undecided coding-category-utf-8))
            "UTF-8")
           (t (upcase (symbol-name (plist-get sys :name))))))))

(defvar custom-ml:backup mode-line-format)

;;;###autoload
(define-minor-mode custom-ml-mode
  "Toggle custom-ml."
  :group 'custom-ml
  :global t
  (if custom-ml-mode
      (progn
        (add-hook 'post-command-hook #'custom-ml:set-active-buffer-id)
        (add-hook 'buffer-list-update-hook #'custom-ml:update)
        (setq-default mode-line-format
                      '(:eval
                        (custom-ml:format
                         (format-mode-line
                          '((:propertize " " display (raise 0.15))
                            (:eval (custom-ml:active-buffer-segment))
                            (:propertize "  %b " face mode-line-buffer-id)
                            (:eval (custom-ml:file-status-segment))
                            (:propertize " (%l,%c) " face custom-ml:muted)
                            (:eval (custom-ml:encoding-segment))
                            (:propertize " ")
                            (:eval (custom-ml:very-important-segment))))
                         (format-mode-line
                          '((:eval (custom-ml:process-segment))
                            (:eval (custom-ml:misc-segment))
                            (:eval (custom-ml:major-mode-segment))
                            (:eval (custom-ml:minor-mode-segment))
                            (vc-mode vc-mode)
                            (:propertize " " display (raise -0.15))))
                         ))))
    (progn
      (remove-hook 'post-command-hook #'custom-ml:set-active-buffer-id)
      (remove-hook 'buffer-list-update-hook #'custom-ml:update)
      (setq-default mode-line-format custom-ml:backup))))

(provide 'custom-ml)
;;; custom-ml.el ends here
