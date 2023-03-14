;;; dotemacs-keybinds.el --- Keybinds -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs-modes)

(use-package general
  :init
  (general-define-key
   "C-s" 'consult-line
   "C-x b" 'consult-buffer
   "C-c k" 'consult-ripgrep
   "C-." 'embark-act
   "C-h B" 'embark-bindings
   "C-S-c C-S-c" 'mc/edit-lines
   "C-c C-<" 'mc/mark-all-like-this
   "C->" 'mc/mark-next-like-this
   "C-<" 'mc/mark-previous-like-this
   "C-S-<mouse-1>" 'mc/add-cursor-on-click
   "C-=" 'er/expand-region
   "C-:" 'avy-goto-char
   "<f5>" 'github-primer-cycle
   "<f6>" 'display-time-mode)

  (general-define-key
   :prefix "C-z"
   "." 'embark-dwim
   "/" 'crux-duplicate-current-line-or-region
   "b" 'blamer-mode
   "e" 'embark-export
   "m" 'consult-minor-mode-menu
   "n" 'dotemacs-toggle-line-numbers-type
   "o" 'crux-open-with
   "p f" 'consult-project-extra-find
   "p o" 'consult-project-extra-find-other-window
   "r f" 'org-roam-node-find
   "r i" 'org-roam-node-insert
   "r c" 'org-roam-capture
   "r t" 'org-roam-buffer-toggle
   "s" 'isearch-forward
   "t" 'multi-vterm
   "C-e" 'crux-eval-and-replace
   "C-s" 'crux-sudo-edit
   "C-/" 'crux-duplicate-and-comment-current-line-or-region
   ">" 'avy-goto-char-2
   "l" 'avy-goto-line)

  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-r" 'consult-history)

  (general-define-key
   :keymaps 'dotemacs-prog-mode-map
   "C-a" 'crux-move-beginning-of-line
   "C-z g" 'highlight-indent-guides-mode
   "C-c <C-tab>" 'format-all-buffer)

  (general-define-key
   :keymaps 'flymake-mode-map
   "C-z c" 'consult-flymake)

  (general-define-key
   :keymaps 'flyspell-mode-map
   "C-z c" 'flyspell-buffer
   "C-z h" 'dotemacs-consult-hunspell-dict
   "C-;" 'flyspell-correct-wrapper)

  (general-define-key
   :keymaps 'vterm-mode-map
   "C-n" 'multi-vterm
   "C->" 'multi-vterm-next
   "C-<" 'multi-vterm-prev))


(provide 'dotemacs-keybinds)
;;; dotemacs-keybinds.el ends here
