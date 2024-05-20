;; -*- lexical-binding: t -*-

(load (expand-file-name "elpaca-bootstrap.el" user-emacs-directory))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca no-littering)
(elpaca delight)

;; Emacs 28
;; (unless (fboundp 'eglot)
;;   (elpaca project)
;;   (elpaca eglot))
;; (unless (fboundp 'treesit-available-p)
;;   (defun treesit-available-p () nil))

(elpaca geiser)
(elpaca vterm)

(elpaca-wait)

(customize-set-variable 'use-package-expand-minimally t)

(require 'recentf)
(require 'no-littering)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(defgroup dotemacs nil
  "Init Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom dotemacs-roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'dotemacs
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

(use-package fontaine
  ;; :ensure (fontaine :repo "https://git.sr.ht/~protesilaos/fontaine")
  :if (display-graphic-p)
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "var/fontaine-latest-state.eld"))
  (fontaine-presets
   '((safe
      :default-family "Monospace"
      :default-height 120
      :fixed-pitch-height 120
      :fixed-pitch-serif-height 120
      :variable-pitch-family "Sans Serif"
      :variable-pitch-height 120)
     (monolisa
      :default-family "MonoLisa"
      :default-height 140
      :fixed-pitch-height 140
      :fixed-pitch-serif-height 140
      :variable-pitch-family "Concourse T3"
      :variable-pitch-height 170)
     (ibm
      :default-family "IBM Plex Mono"
      :default-height 150
      :fixed-pitch-height 150
      :fixed-pitch-serif-height 150
      :variable-pitch-family "IBM Plex Sans"
      :variable-pitch-height 150)
     (roboto
      :default-family "Roboto Mono"
      :default-height 140
      :fixed-pitch-height 140
      :fixed-pitch-serif-height 140
      :variable-pitch-family "Roboto"
      :variable-pitch-height 140)
     (jetbrains
      :default-family "JetBrains Mono"
      :default-height 150
      :fixed-pitch-height 150
      :fixed-pitch-serif-height 150
      :variable-pitch-family "Noto Sans"
      :variable-pitch-height 150)
     (t
      :default-weight normal
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :fixed-pitch-serif-family nil
      :fixed-pitch-serif-weight nil
      :variable-pitch-weight nil
      :bold-family nil
      :bold-weight bold
      :italic-family nil
      :italic-slant italic
      :line-spacing nil)))
  :preface
  (defun dotemacs-change-org-faces (&rest _)
    "Set custom org faces for a given preset."
    (let* ((family '(:family "Concourse C3"))
           (heading (if (eq fontaine-current-preset 'monolisa) `(:height 180 ,@family) '(:height 180))))
      (custom-theme-set-faces
       'user
       `(org-level-1 ((t (:inherit outline-1 ,@heading))))
       `(org-level-2 ((t (:inherit outline-2 ,@heading))))
       `(org-level-3 ((t (:inherit outline-3 ,@heading))))
       `(org-level-4 ((t (:inherit outline-4 ,@heading))))
       `(org-level-5 ((t (:inherit outline-5 ,@heading))))
       `(org-level-6 ((t (:inherit outline-6 ,@heading))))
       `(org-level-7 ((t (:inherit outline-7 ,@heading))))
       `(org-level-8 ((t (:inherit outline-8 ,@heading))))
       `(org-headline-todo ((t (:inherit org-default))))
       `(org-headline-done ((t (:inherit (org-headline-todo font-lock-comment-face))))))))
  :init
  (advice-add 'fontaine-set-preset :after #'dotemacs-change-org-faces)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'safe))
  :config
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  :bind ("C-z f" . fontaine-set-preset))

(defun dotemacs-disable-theme (&rest _)
  "Disable current theme before setting a new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'dotemacs-disable-theme)

(use-package github-primer-theme
  :ensure (github-primer-theme :repo "https://github.com/thiagopolastri/github-primer-theme-emacs")
  :commands (github-primer-cycle)
  :bind ("<f5>" . github-primer-cycle)
  :init (load-theme 'github-primer t))

(use-package dotemacs-mode-line
  :ensure nil
  :load-path "lisp"
  :config (dotemacs-mode-line-mode 1))

(use-package emacs
  :ensure nil
  :custom
  (y-or-n-p-use-read-key t)
  (tab-always-indent 'complete)
  (cursor-type 'bar)
  (use-short-answers t)
  ;; (visible-bell 1)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (fill-column 80)
  (tab-width 4)
  (word-wrap t)
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  ;; (frame-inhibit-implied-resize t) ;; if you use tiling wm
  (frame-resize-pixelwise t)
  (kill-buffer-query-functions nil)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (auto-window-vscroll nil)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (read-process-output-max (* 1024 1024))
  (delete-by-moving-to-trash t)
  :init
  (global-unset-key (kbd "C-z"))
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-so-long-mode 1))

(use-package mule
  :ensure nil
  :init
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8))

(use-package async
  :ensure nil
  :if (fboundp 'async-bytecomp-package-mode)
  :init (async-bytecomp-package-mode 1))

(use-package pixel-scroll
  :ensure nil
  :custom (pixel-scroll-precision-interpolate-page t)
  :init (pixel-scroll-precision-mode 1))

(use-package window
  :ensure nil
  :custom (switch-to-buffer-obey-display-actions t))

(use-package files
  :ensure nil
  :custom
  (confirm-kill-emacs 'yes-or-no-p)
  (large-file-warning-threshold 100000000) ; 100MB
  (require-final-newline t)
  (find-file-visit-truename t)
  (revert-without-query t)
  (make-backup-files nil)
  (auto-save-default nil)
  ;; (remote-file-name-inhibit-delete-by-moving-to-trash t)  ; Emacs 30
  :init
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

(use-package time
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%H:%M") ; %d/%m/%Y %H:%M
  :bind ("<f6>" . display-time-mode))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  (auto-revert-check-vc-info t)
  :config (global-auto-revert-mode 1))

(use-package savehist
  :ensure nil
  :custom (history-length 25)
  :config (savehist-mode 1))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-valho --group-directories-first")
  :hook (dired-mode . dired-hide-details-mode)) ; use ( to show details

(use-package eldoc
  :ensure nil
  :delight eldoc-mode
  :custom
  ;; Prevent echo area resize and always prefer buffer (C-h .)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package isearch
  :ensure nil
  :bind ("C-z s" . isearch-forward))

(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package speedbar
  :ensure nil
  :custom (speedbar-show-unknown-files t))

(use-package sr-speedbar
  :defer t
  :custom (sr-speedbar-right-side nil))

(use-package face-remap
  :ensure nil
  :no-require t
  :delight buffer-face-mode)

(use-package ibuffer
  :ensure nil
  :no-require t
  :bind ("C-z b" . ibuffer))

(use-package subword
  :ensure nil
  :no-require t
  :delight (subword-mode) (superword-mode))

(require 'cl-macs)

(use-package treesit
  :ensure nil
  :if (treesit-available-p)
  :preface
  (defun dotemacs-treesit-install-all ()
    "Install all language grammar."
    (interactive)
    (with-temp-buffer
      (dolist (lang-list treesit-language-source-alist)
        (let ((lang (car lang-list)))
          (unless (treesit-language-available-p lang)
            (message "Installing %s" lang)
            (treesit-install-language-grammar lang)))))))

(cl-defun dotemacs-use-treesit (&key lang github path remap mode)
  "Setup treesiter for a given language.
LANG - language to setup (symbol)
GITHUB - github path to grammar (only user/repo)
PATH - path to src inside github repository
REMAP - list to add to `major-mode-remap-alist'
MODE - list to add to `auto-mode-alist'"
  (let ((tsp (treesit-available-p)))
    (when (and tsp lang github)
      (add-to-list
       'treesit-language-source-alist
       `(,lang . (,(concat "https://github.com/" github) nil ,path))))
    (when (and tsp lang remap (treesit-ready-p lang t))
      (add-to-list 'major-mode-remap-alist remap))
    (when (and tsp lang mode (treesit-ready-p lang t))
      (add-to-list 'auto-mode-alist mode))))

(use-package project
  :ensure nil
  :demand t
  :custom
  (project-vc-ignores '("target/"
                        "bin/"
                        "obj/"
                        "node_modules/"
                        ".vscode/"))
  (project-vc-extra-root-markers '(".dir-locals.el"
                                   "package.json"
                                   "cargo.toml"
                                   "pom.xml"
                                   "*.csproj")))

(use-package eglot
  :ensure nil
  :demand t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-report-progress nil)
  :init (fset #'jsonrpc--log-event #'ignore)
  :commands (eglot-ensure))

(define-minor-mode dotemacs-prog-mode
  "Stub mode with modes that should be hooked in `prog-mode'."
  :init-value nil
  :keymap (make-sparse-keymap)
  (when (and (not (eq major-mode 'clojure-mode)) ; use Cider
             (not (eq major-mode 'clojure-ts-mode)) ; use Cider
             (not (eq major-mode 'lisp-mode)) ; use Sly
             (not (eq major-mode 'scheme-mode)) ; use Geiser
             (eglot--lookup-mode major-mode))
    (eglot-ensure)))

(define-minor-mode dotemacs-text-mode
  "Stub mode with modes that should be hooked in `text-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(add-hook 'prog-mode-hook 'dotemacs-prog-mode)

(use-package drag-stuff
  :defer t
  :delight drag-stuff-mode
  :config (drag-stuff-define-keys)
  :hook (dotemacs-prog-mode . drag-stuff-mode))

(use-package combobulate
  :ensure (combobulate :repo "https://github.com/mickeynp/combobulate")
  :defer t
  :delight combobulate-mode
  :if (treesit-available-p)
  :custom (combobulate-key-prefix "C-c o")
  :hook ((combobulate-mode . (lambda () (drag-stuff-mode -1)))
         (python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

(use-package smartparens
  :defer t
  :delight smartparens-mode
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  ;; :config (smartparens-global-mode)
  :hook (dotemacs-prog-mode . smartparens-mode))

(use-package paredit
  :defer t
  :delight paredit-mode
  :hook ((paredit-mode . (lambda ()
                           (smartparens-mode -1)
                           (drag-stuff-mode -1)))
         (lisp-mode-hook . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (common-lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package eglot-x
  :ensure (eglot-x :repo "https://github.com/nemethf/eglot-x")
  :config (eglot-x-setup))

(use-package format-all
  :bind (:map dotemacs-prog-mode-map
              ("C-c <C-tab>" . format-all-buffer)))

(use-package realgud :defer t)

(use-package simple
  :ensure nil
  :delight visual-line-mode
  :custom
  (indent-tabs-mode nil)
  ;; (remote-file-name-inhibit-auto-save t) ; Emacs 30
  :hook (dotemacs-text-mode . visual-line-mode)
  :init
  (line-number-mode 1)
  (column-number-mode 1))

(use-package glyphless-mode
  :ensure nil
  :delight glyphless-display-mode
  :hook (dotemacs-prog-mode . glyphless-display-mode))

(use-package whitespace
  :ensure nil
  :delight whitespace-mode
  :custom
  (whitespace-style '(face tabs empty trailing tab-mark indentation::space))
  (whitespace-action '(auto-cleanup)) ; clean on save
  :hook (dotemacs-prog-mode . whitespace-mode))

(use-package display-line-numbers
  :ensure nil
  :hook (dotemacs-prog-mode . display-line-numbers-mode)
  :preface
  (defun dotemacs-toggle-line-numbers-type ()
    "Toggle between normal and relative line-numbers."
    (interactive)
    (display-line-numbers-mode -1)
    (if (eq display-line-numbers-type 'relative)
        (setq-local display-line-numbers-type t)
      (setq-local display-line-numbers-type 'relative))
    (display-line-numbers-mode +1))
  :bind (:map dotemacs-prog-mode-map
              ("C-z n" . dotemacs-toggle-line-numbers-type)))

(use-package hl-line
  :ensure nil
  :hook ((dotemacs-prog-mode dired-mode) . hl-line-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :custom (display-fill-column-indicator-character ?\u258F)
  :hook (dotemacs-prog-mode . display-fill-column-indicator-mode))

(use-package hideshow
  :ensure nil
  :delight hs-minor-mode
  :hook (dotemacs-prog-mode . hs-minor-mode))

(use-package which-key
  :delight which-key-mode
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :init (which-key-mode)
  :config (which-key-setup-side-window-bottom))

;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-key]      . helpful-key)
         ("C-h F"                   . helpful-function)
         ("C-c C-d"                 . helpful-at-point)
         :map helpful-mode-map
         ([remap revert-buffer] . helpful-update)))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package devdocs :defer t)
(use-package sicp :no-require t)

(use-package transient :no-require t)
(use-package magit
  :after transient
  :custom (magit-diff-refine-hunk t))

(use-package diff-hl
  :after magit
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh))
  :config (global-diff-hl-mode))

(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :bind ("C-z g" . blamer-mode))

(use-package editorconfig
  :delight editorconfig-mode
  :config (editorconfig-mode 1))

(use-package rainbow-mode :defer t :delight)

(use-package rainbow-delimiters
  :hook (dotemacs-prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (dotemacs-prog-mode . highlight-numbers-mode))

(use-package crux
  :bind (("C-z /"   . crux-duplicate-current-line-or-region)
         ("C-z C-f" . crux-open-with)
         ("C-z C-e" . crux-eval-and-replace)
         ("C-z C-s" . crux-sudo-edit)
         ("C-z C-/" . crux-duplicate-and-comment-current-line-or-region)
         :map dotemacs-prog-mode-map
         ("C-a" . crux-move-beginning-of-line)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expreg ; expand-region alternative
  :bind (("C-=" . expreg-expand) ; er/expand-region
         ("C-+" . expreg-contract)))

(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-z >" . avy-goto-char-2)
         ("C-z l" . avy-goto-line)))

(use-package yasnippet
  :delight yas-minor-mode
  :init (yas-global-mode)
  :bind (("C-z y" . yas-expand)))

(use-package yasnippet-snippets :no-require t)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-category-defaults nil))

(use-package embark
  :custom (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-."   . embark-act)
         ("C-h B" . embark-bindings)
         ("C-z ." . embark-dwim)
         ("C-z e" . embark-export)))

(use-package consult
  :custom (completion-in-region-function #'consult-completion-in-region)
  :bind (([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ("C-s"                                 . consult-line)
         ("C-c k"                               . consult-ripgrep)
         ("C-z m"                               . consult-minor-mode-menu)
         ("C-z o"                               . consult-outline)
         :map minibuffer-local-map
         ("C-r" . consult-history)
         :map flymake-mode-map
         ("C-z c" . consult-flymake)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :custom (vertico-cycle t)
  :init (vertico-mode)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  (corfu-auto nil) ; call with C-M-i
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first nil)
  (corfu-popupinfo-delay 0.2)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Sanitize the `pcomplete-completions-at-point' Capf.
  ;; The Capf has undesired side effects on Emacs 28 and earlier.

  ;; Wrap a chatty Capf and silence it.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Purify a broken Capf and ensure that it does not modify the buffer.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package consult-project-extra
  :bind (("C-z p f" . consult-project-extra-find)
         ("C-z p o" . consult-project-extra-find-other-window)))

(use-package consult-eglot
  :after consult
  :defer t)

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :defer t)

(use-package jinx
  :hook (dotemacs-text-mode . jinx-mode)
  :delight '(:eval jinx-languages)
  :bind (:map jinx-mode-map
              ("C-z h" . jinx-languages)
              ("C-;"   . jinx-correct)))

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 2)
  (flymake-mode-line-lighter "!"))

(use-package flymake-eslint
  :defer t
  :preface
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

(dotemacs-use-treesit
 :lang 'bash
 :github "tree-sitter/tree-sitter-bash"
 :remap '(sh-mode . bash-ts-mode))

(dotemacs-use-treesit
 :lang 'c
 :github "tree-sitter/tree-sitter-c"
 :remap '(c-mode . c-ts-mode))

(dotemacs-use-treesit
 :lang 'cpp
 :github "tree-sitter/tree-sitter-cpp"
 :remap '(c++-mode . c++-ts-mode))

(dotemacs-use-treesit
 :lang 'csharp
 :github "tree-sitter/tree-sitter-c-sharp"
 :remap '(csharp-mode . csharp-ts-mode))

(dotemacs-use-treesit
 :lang 'java
 :github "tree-sitter/tree-sitter-java"
 :remap '(java-mode . java-ts-mode))

(dotemacs-use-treesit
 :lang 'ruby
 :github "tree-sitter/tree-sitter-ruby"
 :remap '(ruby-mode . ruby-ts-mode))

(dotemacs-use-treesit
 :lang 'python
 :github "tree-sitter/tree-sitter-python"
 :remap '(python-mode . python-ts-mode))

(dotemacs-use-treesit
 :lang 'css
 :github "tree-sitter/tree-sitter-css"
 :remap '(css-mode . css-ts-mode))

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'css-ts-mode-hook 'rainbow-mode)

(dotemacs-use-treesit
 :lang 'javascript
 :github "tree-sitter/tree-sitter-javascript"
 :remap '(javascript-mode . js-ts-mode))

(add-hook 'javascript-mode-hook 'subword-mode)
(add-hook 'js-ts-mode-hook 'subword-mode)

(dotemacs-use-treesit
 :lang 'toml
 :github "tree-sitter/tree-sitter-toml"
 :remap '(conf-toml-mode . toml-ts-mode))

(dotemacs-use-treesit
 :lang 'html
 :github "tree-sitter/tree-sitter-html"
 :remap '(html-mode . html-ts-mode))

(add-hook 'conf-toml-mode-hook 'dotemacs-prog-mode)
(add-hook 'toml-ts-mode-hook 'dotemacs-prog-mode)
(add-hook 'conf-mode-hook 'dotemacs-prog-mode)
(add-hook 'html-ts-mode-hook 'dotemacs-prog-mode)
(add-hook 'html-mode-hook 'dotemacs-prog-mode) ; sgml-mode
(add-hook 'mhtml-mode-hook 'dotemacs-prog-mode) ; sgml-mode
(add-hook 'xml-mode-hook 'dotemacs-prog-mode)
(add-hook 'nxml-mode-hook 'dotemacs-prog-mode)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(customize-set-variable 'inferior-lisp-program "sbcl")
(customize-set-variable 'scheme-program-name "guile")

(use-package sly
  :defer t
  :custom (sly-symbol-completion-mode nil)
  :hook (lisp-mode . sly-editing-mode))

(use-package sly-asdf :after sly :defer t)
(use-package sly-quicklisp :after sly :defer t)

(use-package geiser-guile :defer t)

(use-package racket-mode
  :hook (racket-mode . paredit-mode))

(use-package clojure-ts-mode
  :if (treesit-available-p)
  :hook ((clojure-ts-mode clojurescript-ts-mode) . paredit-mode))

(use-package clojure-mode
  :hook ((clojure-mode clojurescript-mode) . paredit-mode)
  :init
  (dotemacs-use-treesit
   :lang 'clojure
   :github "sogaiu/tree-sitter-clojure"
   :remap '(clojure-mode . clojure-ts-mode)
   :mode '("\\.cljs\\'" . clojurescript-ts-mode)))

(use-package cider
  :defer t
  :after clojure-mode
  :hook ((clojure-mode clojure-ts-mode) . cider-mode))

(use-package haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-tags-on-save t)
  :init
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  :bind (:map haskell-mode-map
              ("<f8>" . haskell-navigate-imports)))

(use-package hindent
  :hook (haskell-mode . hindent-mode))

(use-package rust-mode
  :hook ((rust-mode rust-ts-mode) . superword-mode)
  :init
  (dotemacs-use-treesit
   :lang 'rust
   :github "tree-sitter/tree-sitter-rust"
   :remap '(rust-mode . rust-ts-mode)))

(use-package cargo
  :delight cargo-minor-mode
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

(use-package typescript-mode
  :init
  (dotemacs-use-treesit
   :lang 'typescript
   :github "tree-sitter/tree-sitter-typescript"
   :path "typescript/src"
   :remap '(typescript-mode . typescript-ts-mode))
  (dotemacs-use-treesit
   :lang 'tsx
   :github "tree-sitter/tree-sitter-typescript"
   :path "tsx/src"
   :mode '("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-mode typescript-ts-mode tsx-ts-mode) . subword-mode))

(use-package deno-ts-mode :if (treesit-available-p)) ; :config (deno-ts-setup-eglot)

(use-package npm-mode
  :defer t
  :delight
  :hook ((javascript-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) . npm-mode))

(use-package json-mode
  :init
  (dotemacs-use-treesit
   :lang 'json
   :github "tree-sitter/tree-sitter-json"
   :remap '(json-mode . json-ts-mode)))

(use-package pyvenv
  :defer t
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(use-package cmake-mode
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'cmake
   :github "uyha/tree-sitter-cmake"
   :remap '(cmake-mode . cmake-ts-mode)))

(use-package yaml-mode
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'yaml
   :github "ikatyang/tree-sitter-yaml"
   :remap '(yaml-mode . yaml-ts-mode))
  :hook ((yaml-mode yaml-ts-mode) . dotemacs-prog-mode))

(use-package dockerfile-mode
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'dockerfile
   :github "camdencheek/tree-sitter-dockerfile"
   :remap '(dockerfile-mode . dockerfile-ts-mode)))

(use-package go-mode
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'go
   :github "tree-sitter/tree-sitter-go"
   :remap '(go-mode . go-ts-mode))
  (dotemacs-use-treesit
   :lang 'go-mod
   :github "camdencheek/tree-sitter-go-mod"
   :remap '(go-dot-mod-mode . go-mod-ts-mode)))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.sface\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :custom
  (web-mode-enable-html-entities-fontification t)
  (web-mode-auto-close-style 1)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing t))

(define-derived-mode dotemacs-svelte-mode web-mode "Web/Svelte"
  "Custom Svelte major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . dotemacs-svelte-mode))
(add-to-list 'eglot-server-programs
             '(dotemacs-svelte-mode . ("svelteserver" "--stdio")))

(unless (and (treesit-available-p) (treesit-ready-p 'vue t))
  (define-derived-mode dotemacs-vue-mode web-mode "Web/Vue"
    "Custom Vue major mode derived from `web-mode'.")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . dotemacs-vue-mode))
  (add-to-list 'eglot-server-programs '(dotemacs-vue-mode . "vls")))

(use-package vue-ts-mode
  :ensure (vue-ts-mode :repo "https://github.com/8uff3r/vue-ts-mode.git")
  :when (treesit-available-p)
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'vue
   :github "ikatyang/tree-sitter-vue"
   :mode '("\\.vue\\'" . vue-ts-mode))
  (add-to-list 'eglot-server-programs '(dotemacs-vue-mode . "vls")))

(use-package elixir-mode
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'elixir
   :github "elixir-lang/tree-sitter-elixir"
   :remap '(elixir-mode . elixir-ts-mode))
  (dotemacs-use-treesit
   :lang 'heex
   :github "phoenixframework/tree-sitter-heex"
   :mode '("\\.(l|h)?eex\\'" . heex-ts-mode)))

(use-package php-mode
  :no-require t
  :config
  ;; add ("intelephense" "--stdio" :initializationOptions (:licenseKey "KEY"))
  ;; for premium access
  (add-to-list 'eglot-server-programs
               '((php-mode :language-id "php") . ("intelephense" "--stdio"))))

(use-package lua-mode :defer t)
(use-package zig-mode :defer t)
(use-package julia-mode :defer t)
(use-package meson-mode :defer t)
(use-package just-mode :defer t)

(use-package blueprint-ts-mode
  ;; https://jwestman.pages.gitlab.gnome.org/blueprint-compiler/
  :when (treesit-available-p)
  :no-require t
  :init
  (dotemacs-use-treesit
   :lang 'blueprint
   :github "huanie/tree-sitter-blueprint"
   :mode '("\\.blp\\'" . blueprint-ts-mode))
  (add-to-list 'eglot-server-programs
               '(blueprint-ts-mode . ("blueprint-compiler" "lsp"))))

(add-hook 'rst-mode-hook 'dotemacs-text-mode)

(use-package org
  :ensure nil
  :custom
  (org-ellipsis " ⤵")
  (org-catch-invisible-edits 'show-and-error)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-adapt-indentation nil)
  (org-return-follows-link t)
  (org-edit-src-content-indentation 0)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
  (org-fontify-quote-and-verse-blocks t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-redeadline 'time)
  (org-log-into-drawer t)
  (org-auto-align-tags nil) ; only for variable-pitch
  (org-tags-column 0) ; only for variable-pitch
  (org-fontify-todo-headline t)
  ;; Latex
  (org-highlight-latex-and-related '(native))
  (org-latex-compiler "xelatex")
  (org-latex-listings t)
  (org-latex-pdf-process
   (list (concat "latexmk -"
                 org-latex-compiler
                 " -recorder -synctex=1 -bibtex-cond %b")))
  (org-todo-keywords '((sequence "TODO(t)"
                                 "STARTED(s)"
                                 "WAITING(w)"
                                 "|"
                                 "DONE(d)"
                                 "CANCELLED(c)")))
  :hook ((org-mode . dotemacs-text-mode)
         (org-mode . variable-pitch-mode))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings" t)))

(use-package visual-fill-column
  ;; `visual-line-mode' are already hooked on `dotemacs-text-mode'.
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom (pdf-view-display-size 'fit-page)
  :config (pdf-tools-install))

(use-package htmlize :defer t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-block-fringe nil)
  (org-modern-table nil) ; valign do this better with variable-pitch
  ;; (org-modern-star ["❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽"])
  )

(use-package valign
  :hook (org-mode . valign-mode)
  :custom (valign-fancy-bar t)
  :delight valign-mode)

(use-package org-roam
  :when (and (bound-and-true-p dotemacs-roam-dir)
             (file-exists-p dotemacs-roam-dir))
  :custom
  (org-roam-directory dotemacs-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  :config
  (org-roam-setup)
  (add-to-list
   'org-roam-capture-templates
   '("t" "task" plain
     "* %?\n %a"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))))

(use-package markdown-mode
  :hook (((markdown-mode gfm-mode) . dotemacs-text-mode)
         ((markdown-mode gfm-mode) . valign-mode)
         ((markdown-mode gfm-mode) . variable-pitch-mode))
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t))

(use-package pandoc-mode
  :after (markdown-mode hydra)
  :hook (markdown-mode . pandoc-mode))

(use-package f :demand t)

(defun dotemacs-elpaca-build-dir (p)
  "Return the elpaca build directory for package symbol P."
  (-first-item
   (f-directories elpaca-builds-directory
                  (lambda (dir) (string-match-p (concat "^" (symbol-name p) "$") (f-filename dir))))))

(use-package tex
  :ensure (auctex :pre-build (("./autogen.sh")
                              ("./configure" "--without-texmf-dir" "--with-lispdir=.")
                              ("make")
                              ("install-info" "doc/auctex.info" "doc/dir")
                              ("install-info" "doc/preview-latex.info" "doc/dir")))
  :mode (("\\.tex\\'" . TeX-latex-mode)
         ("\\.tex\\.erb\\'" . TeX-latex-mode)
         ("\\.etx\\'" . TeX-latex-mode))
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . dotemacs-text-mode))
  :init
  (require 'info)
  (add-to-list 'Info-additional-directory-list (f-join (dotemacs-elpaca-build-dir 'auctex) "doc"))
  (add-hook 'tex-mode-hook
            (lambda ()
              (load "auctex.el")
              (setq TeX-command-extra-options "-shell-escape")))
  :config
  (setq-default TeX-global-PDF-mode 1)
  (setq-default  preview-scale-function 1.5)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-source-correlate-method 'synctex))

(use-package le-thesaurus
  :bind (:map dotemacs-text-mode-map
              ("C-z w s" . le-thesaurus-get-synonyms)
              ("C-z w a" . le-thesaurus-get-antonyms)))

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

(use-package elpher :defer t)
(use-package restclient :defer t)
(use-package es-mode :defer t) ;; https://github.com/dakrone/es-mode
(use-package gptel :defer t)

(use-package vterm
  :ensure nil
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 100000))

(use-package multi-vterm
  :bind (("C-z t" . multi-vterm)
         :map vterm-mode-map
         ("C-n" . multi-vterm)
         ("C->" . multi-vterm-next)
         ("C-<" . multi-vterm-prev)))

(use-package dape
  :defer t
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-buffer-window-arrangement 'right)
  :hook ((dape-compile-compile-hooks . kill-buffer)))

(load (expand-file-name "user.el" user-emacs-directory) :no-error)
