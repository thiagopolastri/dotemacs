;;; dotemacs-mode-line.el --- Custom mode-line -*- lexical-binding: t; -*-

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

(defgroup dotemacs-mode-line nil
  "Custom mode-line."
  :group 'dotemacs-mode-line)

(defcustom dotemacs-mode-line-font-awesome "Font Awesome 6 Free"
  "Font awesome name.  Usually \"Font Awesome 6 Free\" or \"FontAwesome\"."
  :group 'dotemacs-mode-line
  :type 'string)

(defcustom dotemacs-mode-line-padding 0.15
  "Vertical padding."
  :group 'dotemacs-mode-line
  :type 'float)

(defface dotemacs-mode-line-muted '((t (:inherit 'shadow)))
  "Mode-line muted face."
  :group 'dotemacs-mode-line)

(defface dotemacs-mode-line-accented
  '((t (:inherit 'font-lock-constant-face :bold t)))
  "Mode-line accented face."
  :group 'dotemacs-mode-line)

(defface dotemacs-mode-line-icon-muted
  `((t (:family ,dotemacs-mode-line-font-awesome :inherit 'shadow)))
  "Mode-line muted icon face."
  :group 'dotemacs-mode-line)

(defface dotemacs-mode-line-icon-accented
  `((t (:family ,dotemacs-mode-line-font-awesome
                :inherit 'font-lock-constant-face)))
  "Mode-line accented icon face."
  :group 'dotemacs-mode-line)

(defface dotemacs-mode-line-icon-warning
  `((t (:family ,dotemacs-mode-line-font-awesome :inherit 'error :bold nil)))
  "Mode-line warning icon face."
  :group 'dotemacs-mode-line)

(defun dotemacs-mode-line-active-buffer ()
  "Show active/modified buffer icon on mode-line."
  (let ((modifiedp (and (not buffer-read-only) (buffer-modified-p)))
        (start-padding (propertize " " 'display `(raise ,dotemacs-mode-line-padding))))
    (if (mode-line-window-selected-p)
        (propertize
         (concat
          start-padding
          (propertize "" 'face (if modifiedp
                                    'dotemacs-mode-line-icon-warning
                                  'dotemacs-mode-line-icon-accented))
          " ")
         'mouse-face 'mode-line-highlight
         'pointer 'hand
         'help-echo (concat (format-mode-line "%F") "\nmouse-1: Display menu")
         'local-map (purecopy (make-mode-line-mouse-map
                               'mouse-1
                               'menu-bar-open)))
      (concat
       start-padding
       (propertize "" 'face (if modifiedp
                                 'dotemacs-mode-line-icon-warning
                               'dotemacs-mode-line-icon-muted))
       " "))))

(defun dotemacs-mode-line-buffer-status ()
  "Show read-only icon on mode-line."
  (when buffer-read-only
    (concat (propertize ""
                        'face 'dotemacs-mode-line-icon-muted
                        'help-echo "Buffer is read-only")
            " ")))

(defun dotemacs-mode-line-vc-mode ()
  "Show version control icon on mode-line.
Display branch name on help-echo."
  (when vc-mode
    (concat (propertize ""
                        'face 'dotemacs-mode-line-icon-muted
                        'help-echo vc-mode)
            " ")))

(defun dotemacs-mode-line-buffer-identification ()
  "Displays file name on mode-line.
Highlight if file is open with Tramp.
Show encoding/eol and path information on help-echo."
  (propertize
   "%b"
   'face (if (mode-line-window-selected-p)
             (if (file-remote-p default-directory)
                 'dotemacs-mode-line-accented
               'mode-line-buffer-id)
           'mode-line-inactive)
   'help-echo (purecopy
               (lambda (window _object _point)
                 (concat
                  "Charset: "
                  (cond ((memq
                          (plist-get
                           (coding-system-plist buffer-file-coding-system)
                           :category)
                          '(coding-category-undecided coding-category-utf-8))
                         "UTF-8")
                        (t (upcase
                            (symbol-name
                             (plist-get
                              (coding-system-plist buffer-file-coding-system)
                              :name)))))
                  (format "%s"
                          (pcase (coding-system-eol-type
                                  buffer-file-coding-system)
                            (0 ", EOL: LF")
                            (1 ", EOL: CRLF")
                            (2 ", EOL: CR")
                            (_ "")))
                  (format "%s"
                          (with-selected-window window
                            (if (stringp default-directory)
                                (concat
                                 (if (file-remote-p default-directory)
                                     "\nRemote directory: "
                                   "\nLocal directory: ")
                                 default-directory)
                              ""))))))))

(defun dotemacs-mode-line-position ()
  "Displays column and line position on mode-line."
  (propertize
   " %l:%c "
   'face (if (mode-line-window-selected-p)
             'dotemacs-mode-line-muted
           'mode-line-inactive)))

(defun dotemacs-mode-line-nyan ()
  "Display buffer position on mode-line.
Displays `nyan-mode' if enabled."
  (if (and (fboundp 'nyan-create) (bound-and-true-p nyan-mode))
      (list (nyan-create))
    (propertize
     "%p%%  "
     'face (if (mode-line-window-selected-p)
               'dotemacs-mode-line-muted
             'mode-line-inactive))))

(defun dotemacs-mode-line-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (when (mode-line-window-selected-p)
    (let ((misc (string-trim (format-mode-line mode-line-misc-info))))
      (unless (string= misc "")
        (concat " " misc)))))

(defun dotemacs-mode-line-process ()
 "Displays the current value of `mode-line-process' in the mode-line."
 (when mode-line-process
   (concat " " (string-trim (format-mode-line mode-line-process)))))

(defun dotemacs-mode-line-major-mode ()
 "Displays the current major mode in the mode-line."
 (propertize
  (concat " " (format-mode-line mode-name) " ")
  'face 'bold))

(defun dotemacs-mode-line-minor-modes ()
  "Displays the current minor modes in the mode-line."
   (replace-regexp-in-string
    "%" "%%%%"
    (string-trim (format-mode-line minor-mode-alist))
    t t))

(defun dotemacs-mode-line-bottom-padding ()
  "Bottom padding."
  (let ((padding (- dotemacs-mode-line-padding)))
    (propertize " " 'display `(raise ,padding))))

(defvar dotemacs-mode-line-backup mode-line-format)

;;;###autoload
(define-minor-mode dotemacs-mode-line-mode
  "Custom mode-line."
  :group 'dotemacs-mode-line
  :global t
  (if dotemacs-mode-line-mode
      (progn
        (setq-default
         mode-line-format
         '((:eval
            (format-mode-line
             '((:eval (dotemacs-mode-line-active-buffer))
               (:eval (dotemacs-mode-line-buffer-status))
               (:eval (dotemacs-mode-line-vc-mode))
               (:eval (dotemacs-mode-line-buffer-identification))
               (:eval (dotemacs-mode-line-position))
               (:eval (dotemacs-mode-line-nyan))
               " %e")))
           mode-line-format-right-align
           (:eval
            (format-mode-line
             '((:eval (dotemacs-mode-line-process))
               (:eval (dotemacs-mode-line-major-mode))
               (:eval (dotemacs-mode-line-minor-modes))
               (:eval (dotemacs-mode-line-misc-info))
               (:eval (dotemacs-mode-line-bottom-padding))))))))
    (progn
      (setq-default mode-line-format dotemacs-mode-line-backup))))

(provide 'dotemacs-mode-line)

;;; dotemacs-mode-line.el ends here
