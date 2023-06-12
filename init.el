;; -*- lexical-binding: t -*-

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca no-littering)
(elpaca delight)
(unless (fboundp 'eglot)
  (elpaca project)
  (elpaca eglot))
(elpaca geiser)
(elpaca vterm)
(elpaca-wait)

(require 'recentf)
(require 'no-littering)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(defgroup dotemacs nil
  "Init Emacs settings (You must restart Emacs to apply these changes)."
  :group 'local)

(defcustom dotemacs-font-fixed "Monospace"
  "Emacs fixed font.  Mono or code variant."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-fixed 120
  "Emacs font size.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-variable "Sans Serif"
  "Emacs variable font.  Sans or Serif."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-variable 120
  "Emacs font size (variable).  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-font-org-title "Sans Serif"
  "Font to use in Org titles."
  :group 'dotemacs
  :type 'string)

(defcustom dotemacs-font-size-org-title 140
  "Font size to use in Org titles.  integer in units of 1/10 point (140 = 14pt)."
  :group 'dotemacs
  :type 'integer)

(defcustom dotemacs-openai-key nil
  "Api key for OpenAI (chatgpt)."
  :group 'dotemacs
  :type '(choice (string :tag "OpenAI API key")
                 (const :tag "None" nil)))

(defcustom dotemacs-roam-dir nil
  "Org Roam directory (where your org files will live)."
  :group 'dotemacs
  :type '(choice (directory :tag "Roam directory")
                 (const :tag "None" nil)))

(defcustom dotemacs-elfeed-org-file nil
  "Org file with feed urls (elfeed)."
  :group 'dotemacs
  :type '(repeat (file :must-match t)))

(set-face-attribute 'default
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'fixed-pitch
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'fixed-pitch-serif
                    nil
                    :family dotemacs-font-fixed
                    :height dotemacs-font-size-fixed)
(set-face-attribute 'variable-pitch
                    nil
                    :family dotemacs-font-variable
                    :height dotemacs-font-size-variable)

(require 'cl-macs)

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (require 'treesit))

(cl-defun dotemacs-use-treesit (&key lang github path remap mode)
  "Setup treesiter for a given language.
LANG - language to setup (symbol)
GITHUB - github path to grammar (only user/repo)
PATH - path to src inside github repository
REMAP - list to add to `major-mode-remap-alist'
MODE - list to add to `auto-mode-alist'"
  (let ((tsp (and (fboundp 'treesit-available-p) (treesit-available-p))))
    (when (and tsp lang github)
      (unless (boundp 'treesit-language-source-alist)
        (setq treesit-language-source-alist '()))
      (add-to-list
       'treesit-language-source-alist
       `(,lang . (,(concat "https://github.com/" github) nil ,path))))
    (when (and tsp lang remap (treesit-ready-p lang t))
      (add-to-list 'major-mode-remap-alist remap))
    (when (and tsp lang mode (treesit-ready-p lang t))
      (add-to-list 'auto-mode-alist mode))))

(use-package project
  :elpaca nil
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
  :elpaca nil
  :demand t
  :custom (eglot-autoshutdown t)
  :commands (eglot-ensure))

(use-package drag-stuff
  :delight drag-stuff-mode
  :demand t
  :config (drag-stuff-define-keys))

(use-package combobulate
  :elpaca (combobulate :repo "https://github.com/mickeynp/combobulate")
  :delight combobulate-mode
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :custom (combobulate-key-prefix "C-c o"))

(define-minor-mode dotemacs-prog-mode
  "Stub mode with modes that should be hooked in `prog-mode'."
  :init-value nil
  :keymap (make-sparse-keymap)
  (when (and (not (eq major-mode 'clojure-mode)) ; use Cider
             (not (eq major-mode 'lisp-mode)) ; use Sly
             (not (eq major-mode 'scheme-mode)) ; use Geiser
             (eglot--lookup-mode major-mode))
    (eglot-ensure))
  (if (and (fboundp 'combobulate-mode)
           (string-match "-ts-mode" (symbol-name major-mode)))
      (combobulate-mode)
    (drag-stuff-mode)))

(define-minor-mode dotemacs-text-mode
  "Stub mode with modes that should be hooked in `text-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(add-hook 'prog-mode-hook 'dotemacs-prog-mode)

(use-package eglot-x
  :elpaca (eglot-x :repo "https://github.com/nemethf/eglot-x")
  :config (eglot-x-setup))

(use-package format-all
  :bind (:map dotemacs-prog-mode-map
              ("C-c <C-tab>" . format-all-buffer)))

(use-package realgud :defer t)

(use-package emacs
  :elpaca nil
  :custom
  (y-or-n-p-use-read-key t)
  (tab-always-indent 'complete)
  (cursor-type 'bar)
  (use-short-answers t)
  (visible-bell 1)
  (use-dialog-box nil)
  (fill-column 80)
  (tab-width 4)
  (word-wrap t)
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  (frame-resize-pixelwise t)
  (kill-buffer-query-functions nil)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (auto-window-vscroll nil)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  :init
  (global-unset-key (kbd "C-z"))
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-so-long-mode 1))

(use-package mule
  :elpaca nil
  :init
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8))

(use-package async ; maybe unecessary
  :elpaca nil
  :if (fboundp 'async-bytecomp-package-mode)
  :init (async-bytecomp-package-mode 1))

(use-package pixel-scroll
  :elpaca nil
  :if (fboundp 'pixel-scroll-precision-mode)
  :init (pixel-scroll-precision-mode 1))

(use-package simple
  :elpaca nil
  :delight visual-line-mode
  :custom (indent-tabs-mode nil)
  :hook (dotemacs-text-mode . visual-line-mode)
  :init
  (line-number-mode 1)
  (column-number-mode 1))

(use-package window
  :elpaca nil
  :custom (switch-to-buffer-obey-display-actions t))

(use-package files
  :elpaca nil
  :custom
  (confirm-kill-emacs 'yes-or-no-p)
  (large-file-warning-threshold 100000000) ; 100MB
  (require-final-newline t)
  (find-file-visit-truename t)
  (revert-without-query t)
  (make-backup-files nil)
  (auto-save-default nil)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package vc ; vc-hooks
  :elpaca nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

(use-package time
  :elpaca nil
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%H:%M")
  :bind ("<f6>" . display-time-mode))

(use-package uniquify
  :elpaca nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package glyphless-mode
  :elpaca nil
  :if (fboundp 'glyphless-display-mode)
  :delight glyphless-display-mode
  :hook (dotemacs-prog-mode . glyphless-display-mode))

(use-package autorevert
  :elpaca nil
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  (auto-revert-check-vc-info t)
  :config (global-auto-revert-mode 1))

(use-package savehist
  :elpaca nil
  :custom (history-length 25)
  :config (savehist-mode 1))

(use-package dired
  :elpaca nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-valho --group-directories-first")
  :hook (dired-mode . dired-hide-details-mode)) ; use ( to show details

(use-package eldoc
  :elpaca nil
  :delight eldoc-mode
  :custom
  ;; Prevent echo area resize and always prefer buffer (C-h .)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package whitespace
  :elpaca nil
  :delight whitespace-mode
  :custom
  (whitespace-style '(face tabs empty trailing tab-mark indentation::space))
  (whitespace-action '(auto-cleanup)) ; clean on save
  :hook (dotemacs-prog-mode . whitespace-mode))

(use-package display-line-numbers
  :elpaca nil
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
  :elpaca nil
  :hook ((dotemacs-prog-mode dired-mode) . hl-line-mode))

(use-package display-fill-column-indicator
  :elpaca nil
  :custom (display-fill-column-indicator-character ?\u258F)
  :hook (dotemacs-prog-mode . display-fill-column-indicator-mode))

(use-package hideshow
  :elpaca nil
  :delight hs-minor-mode
  :hook (dotemacs-prog-mode . hs-minor-mode))

(use-package proced
  :elpaca nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package isearch
  :elpaca nil
  :bind ("C-z s" . isearch-forward))

(use-package gdb-mi
  :elpaca nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package speedbar
  :elpaca nil
  :custom (speedbar-show-unknown-files t))

(use-package sr-speedbar
  :custom (sr-speedbar-right-side nil))

(use-package face-remap
  :elpaca nil
  :delight buffer-face-mode)

(use-package github-primer-theme
  :elpaca nil
  :load-path "themes"
  :commands (github-primer-cycle)
  :bind ("<f5>" . github-primer-cycle)
  :init
  ;; https://github.com/jwiegley/use-package/issues/963
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))
  (load-theme 'github-primer t))

(use-package dotemacs-mode-line
  :elpaca nil
  :load-path "lisp"
  :config (dotemacs-mode-line-mode 1))

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

(use-package devdocs)
(use-package sicp)

(use-package magit
  :custom (magit-diff-refine-hunk t))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

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
  :bind ("C-z b" . blamer-mode))

(use-package editorconfig
  :delight editorconfig-mode
  :config (editorconfig-mode 1))

(use-package smartparens
  :delight smartparens-mode
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  :config (smartparens-global-mode))

(use-package rainbow-mode :defer t :delight)

(use-package rainbow-delimiters
  :hook (dotemacs-prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (dotemacs-prog-mode . highlight-numbers-mode))

(use-package highlight-indent-guides
  :delight highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character) ; or 'bitmap or 'column
  (highlight-indent-guides-suppress-auto-error t)
  (highlight-indent-guides-auto-odd-face-perc 1.5)
  (highlight-indent-guides-auto-even-face-perc 1.5)
  (highlight-indent-guides-auto-character-face-perc 35)
  :bind (:map dotemacs-prog-mode-map
              ("C-z g" . highlight-indent-guides-mode)))

(use-package crux
  :bind (("C-z /"   . crux-duplicate-current-line-or-region)
         ("C-z o"   . crux-open-with)
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

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-z >" . avy-goto-char-2)
         ("C-z l" . avy-goto-line)))

(use-package yasnippet
  :delight yas-minor-mode
  :init (yas-global-mode))

(use-package yasnippet-snippets)

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
         :map minibuffer-local-map
         ("C-r" . consult-history)
         :map flymake-mode-map
         ("C-z c" . consult-flymake)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/*"))
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
  :elpaca (corfu :files (:defaults "extensions/*"))
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

(use-package jinx
  :hook (dotemacs-text-mode . jinx-mode)
  :bind (:map jinx-mode-map
         ("C-z h" . jinx-languages)
         ("C-;"   . jinx-correct)))

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

(add-hook 'conf-mode-hook 'dotemacs-prog-mode)
(add-hook 'html-ts-mode-hook 'dotemacs-prog-mode)
(add-hook 'html-mode-hook 'dotemacs-prog-mode) ; sgml-mode
(add-hook 'mhtml-mode-hook 'dotemacs-prog-mode) ; sgml-mode
(add-hook 'xml-mode-hook 'dotemacs-prog-mode)
(add-hook 'nxml-mode-hook 'dotemacs-prog-mode)

(use-package paredit
  :after smartparens
  :delight paredit-mode
  :init
  (add-hook 'paredit-mode-hook (lambda ()
                                 (smartparens-mode -1)
                                 (drag-stuff-mode -1)))
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'common-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(customize-set-variable 'inferior-lisp-program "sbcl")

(use-package sly
  :defer t
  :custom (sly-symbol-completion-mode nil)
  :hook (lisp-mode . sly-editing-mode))

(use-package sly-asdf :after sly)
(use-package sly-quicklisp :after sly)

(customize-set-variable 'scheme-program-name "guile")
(use-package geiser-guile :defer t)

(use-package racket-mode
  :hook (racket-mode . paredit-mode))

(use-package clojure-mode
  :hook ((clojure-mode . paredit-mode)))

(use-package cider
  :defer t
  :after clojure-mode
  :hook (cider-mode . cider-turn-on-eldoc-mode))

(use-package haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  :init
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (custom-set-variables '(haskell-tags-on-save t))
  :bind (:map haskell-mode-map
              ("<f8>" . haskell-navigate-imports)))

(use-package hindent
  :hook (haskell-mode . hindent-mode))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  ;; (rustic-treesitter-derive t)
  :hook (rustic-mode . superword-mode)
  :init
  ;; treesitter integration not ready yet
  ;; https://github.com/brotzeit/rustic/issues/475
  (dotemacs-use-treesit
   :lang 'rust
   :github "tree-sitter/tree-sitter-rust"))

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

(use-package npm-mode
  :delight
  :hook ((javascript-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) . npm-mode))

(use-package json-mode
  :init
  (dotemacs-use-treesit
   :lang 'json
   :github "tree-sitter/tree-sitter-json"
   :remap '(json-mode . json-ts-mode)))

(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(use-package cmake-mode
  :init
  (dotemacs-use-treesit
   :lang 'cmake
   :github "uyha/tree-sitter-cmake"
   :remap '(cmake-mode . cmake-ts-mode)))

(use-package yaml-mode
  :init
  (dotemacs-use-treesit
   :lang 'yaml
   :github "ikatyang/tree-sitter-yaml"
   :remap '(yaml-mode . yaml-ts-mode))
  :hook ((yaml-mode yaml-ts-mode) . dotemacs-prog-mode))

(use-package dockerfile-mode
  :init
  (dotemacs-use-treesit
   :lang 'dockerfile
   :github "camdencheek/tree-sitter-dockerfile"
   :remap '(dockerfile-mode . dockerfile-ts-mode)))

(use-package go-mode
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
  :mode "\\.[lh]?eex\\'"
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

(define-derived-mode dotemacs-vue-mode web-mode "Web/Vue"
  "Custom Vue major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . dotemacs-vue-mode))
(add-to-list 'eglot-server-programs '(dotemacs-vue-mode . "vls"))

(define-derived-mode dotemacs-svelte-mode web-mode "Web/Svelte"
  "Custom Svelte major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . dotemacs-svelte-mode))
;; TODO: fix lsp for svelte
(add-to-list 'eglot-server-programs '(dotemacs-svelte-mode . "svelteserver"))

(use-package elixir-mode
  :init
  (dotemacs-use-treesit
   :lang 'elixir
   :github "elixir-lang/tree-sitter-elixir"
   :remap '(elixir-mode . elixir-ts-mode))
  (dotemacs-use-treesit
   :lang 'heex
   :github "phoenixframework/tree-sitter-heex"
   :mode '("\\.(l|h)?eex\\'" . heex-ts-mode)))

(use-package php-mode :defer t)
(use-package lua-mode :defer t)
(use-package zig-mode :defer t)
(use-package julia-mode :defer t)

(add-hook 'rst-mode-hook 'dotemacs-text-mode)

(use-package org
  :elpaca nil
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
  (add-to-list 'org-latex-packages-alist '("" "listings" t))
  (let* ((family `(:family ,dotemacs-font-org-title))
         (heading `(:height ,dotemacs-font-size-org-title ,@family)))
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
     `(org-document-title ((t (,@family)))))))

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
  (org-modern-star ["❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽"]))

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

(use-package tex
  :elpaca auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . dotemacs-text-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config (setq-default TeX-master nil))

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

(use-package elfeed
  :defer t
  :if (bound-and-true-p dotemacs-elfeed-org-file))

(use-package elfeed-org
  :defer t
  :if (bound-and-true-p dotemacs-elfeed-org-file)
  :custom (rmh-elfeed-org-files dotemacs-elfeed-org-file))

(use-package vterm
  :elpaca nil
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 100000))

(use-package multi-vterm
  :bind (("C-z t" . multi-vterm)
         :map vterm-mode-map
         ("C-n" . multi-vterm)
         ("C->" . multi-vterm-next)
         ("C-<" . multi-vterm-prev)))

(use-package gptel ; use C-c <Enter> to send request
  :defer t
  :if (bound-and-true-p dotemacs-openai-key)
  :custom (gptel-api-key dotemacs-openai-key))
