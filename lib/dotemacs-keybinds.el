;;; dotemacs-keybinds.el --- Keybinds -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'dotemacs-modes)

(use-package hydra
  :init
  ;; My agenda files are org-roam files.
  (when (and (bound-and-true-p dotemacs-roam-dir)
             (file-exists-p dotemacs-roam-dir))
    (defhydra hydra-roam-menu (:exit t :hint nil)
      "
^Create/Open^             ^Actions on current file^          ^View^
^^^^^^^^------------------------------------------------------------------------
_f_: Find roam node       _n_: Add to agenda                 _a_: Agenda
_i_: Insert roam node     _r_: Remove from agenda            _t_: Today (daily)
_c_: Capture task/note    _s_: Schedule current TODO
_d_: Capture daily        _m_: Refile current TODO
"
      ("f" org-roam-node-find)
      ("i" org-roam-node-insert)
      ("c" org-roam-capture)
      ("d" org-roam-dailies-capture-today)
      ("n" org-agenda-file-to-front)
      ("r" org-remove-file)
      ("s" org-schedule)
      ("m" org-roam-refile)
      ("a" org-agenda)
      ("t" org-roam-dailies-goto-today)
      ("q" nil "cancel"))

    (global-set-key (kbd "<f12>") #'hydra-roam-menu/body)))

(use-package general
  :init
  (general-define-key
   "C-s" 'consult-line
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
   "<f6>" 'display-time-mode
   ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
   ;; already links to the manual, if a function is referenced there.
   "C-h F" 'helpful-function
   [remap describe-function] #'helpful-callable
   [remap describe-symbol] #'helpful-symbol
   [remap describe-variable] #'helpful-variable
   [remap describe-command] #'helpful-command
   [remap describe-key] #'helpful-key
   [remap bookmark-jump] #'consult-bookmark
   [remap goto-line] #'consult-goto-line
   [remap imenu] #'consult-imenu
   [remap locate] #'consult-locate
   [remap load-theme] #'consult-theme
   [remap man] #'consult-man
   [remap recentf-open-files] #'consult-recent-file
   [remap switch-to-buffer] #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
   [remap yank-pop] #'consult-yank-pop)

  (general-define-key
   :prefix "C-z"
   "." 'embark-dwim
   "/" 'crux-duplicate-current-line-or-region
   ">" 'avy-goto-char-2
   "b" 'blamer-mode
   "e" 'embark-export
   "l" 'avy-goto-line
   "m" 'consult-minor-mode-menu
   "n" 'dotemacs-toggle-line-numbers-type
   "o" 'crux-open-with
   "p f" 'consult-project-extra-find
   "p o" 'consult-project-extra-find-other-window
   "s" 'isearch-forward
   "t" 'multi-vterm
   "C-e" 'crux-eval-and-replace
   "C-s" 'crux-sudo-edit
   "C-/" 'crux-duplicate-and-comment-current-line-or-region)

  (general-define-key
   :keymaps 'helpful-mode-map
   [remap revert-buffer] #'helpful-update)

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
   :keymaps 'jinx-mode-map
   "C-z h" 'jinx-languages ; just to keep my old dict keybind (h for hunspell)
   "C-;" 'jinx-correct)

  (general-define-key
   :keymaps 'haskell-mode-map
   "<f8>" 'haskell-navigate-imports)

  (general-define-key
   :keymaps 'vterm-mode-map
   "C-n" 'multi-vterm
   "C->" 'multi-vterm-next
   "C-<" 'multi-vterm-prev))


(provide 'dotemacs-keybinds)
;;; dotemacs-keybinds.el ends here
