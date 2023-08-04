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

;; -----------------------------------------------------------------------------
;; this will be unecessary after `mode-line-window-selected-p' and
;; `mode-line-format-right-align' introduction (Emacs 30.1).
;; i'll keep this to make it compatible with the current stable version
;; just change all ocurrences of `dotemacs-mode-line-active-p' to
;; `mode-line-window-selected-p' and remove all the extra advice and branchs in
;; the `dotemacs-mode-line-mode' function.
(defvar dotemacs-mode-line-selected-window (selected-window)
  "Selected window state.")

(defun dotemacs-mode-line-select-window (_window)
  "Save selected window state."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq dotemacs-mode-line-selected-window (selected-window))))

(defun dotemacs-mode-line-active-p ()
  "Check if select window is active."
  (if (fboundp 'mode-line-window-selected-p)
      (mode-line-window-selected-p)
    (eq dotemacs-mode-line-selected-window (get-buffer-window))))

(defun dotemacs-mode-line-format (left right)
  "Format the mode-line in two parts and align it on LEFT and RIGHT."
  (concat
   left
   (propertize
    " "
    'display `((space :align-to (- (+ right right-fringe right-margin)
                                   ,(length right)))))
   right))
;; -----------------------------------------------------------------------------

(defun dotemacs-mode-line-active-buffer ()
  "Show active/modified buffer icon on mode-line."
  (let ((modifiedp (and (not buffer-read-only) (buffer-modified-p)))
        (start-padding (propertize " " 'display '(raise 0.15))))
    (if (dotemacs-mode-line-active-p)
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
   'face (if (dotemacs-mode-line-active-p)
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
   'face (if (dotemacs-mode-line-active-p)
             'dotemacs-mode-line-muted
           'mode-line-inactive)))

(defun dotemacs-mode-line-nyan ()
  "Display buffer position on mode-line.
Displays `nyan-mode' if enabled."
  (if (and (fboundp 'nyan-create) (bound-and-true-p nyan-mode))
      (list (nyan-create))
    (propertize
     "%p%%  "
     'face (if (dotemacs-mode-line-active-p)
               'dotemacs-mode-line-muted
             'mode-line-inactive))))

(defun dotemacs-mode-line-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc (string-trim (format-mode-line mode-line-misc-info))))
    (unless (string= misc "")
      (concat " " misc))))

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

(defvar dotemacs-mode-line-backup mode-line-format)

;;;###autoload
(define-minor-mode dotemacs-mode-line-mode
  "Custom mode-line."
  :group 'dotemacs-mode-line
  :global t
  (if dotemacs-mode-line-mode
      (progn
        (when (not (fboundp 'mode-line-window-selected-p))
          (add-function :before pre-redisplay-function
                        #'dotemacs-mode-line-select-window))

        (if mode-line-format-right-align
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
                   (:propertize " " display (raise -0.15)))))))
          (setq-default
           mode-line-format
           '(:eval
             (dotemacs-mode-line-format
              (format-mode-line
               '((:eval (dotemacs-mode-line-active-buffer))
                 (:eval (dotemacs-mode-line-buffer-status))
                 (:eval (dotemacs-mode-line-vc-mode))
                 (:eval (dotemacs-mode-line-buffer-identification))
                 (:eval (dotemacs-mode-line-position))
                 (:eval (dotemacs-mode-line-nyan))
                 " %e"))
              (format-mode-line
               '((:eval (dotemacs-mode-line-process))
                 (:eval (dotemacs-mode-line-major-mode))
                 (:eval (dotemacs-mode-line-minor-modes))
                 (:eval (dotemacs-mode-line-misc-info))
                 (:propertize " " display (raise -0.15)))))))))
    (progn
      (when (not (fboundp 'mode-line-window-selected-p))
        (remove-function pre-redisplay-function
                         #'dotemacs-mode-line-select-window))
      (setq-default mode-line-format dotemacs-mode-line-backup))))

(provide 'dotemacs-mode-line)

;;; dotemacs-mode-line.el ends here
