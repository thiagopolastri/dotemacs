;;; init.el --- Emacs init file -*- lexical-binding: t -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I'm too lazy to separate all the modules and too inexpressive
;; to write this as literate org file.

;; Enjoy this long file.

;; TODO: setup sly
;; TODO: configure other modes that should use variable-pitch
;; TODO: make svelte derived mode work with eglot

;;; Code:

;; Create a profile message on startup

(defun emacs-custom-config:startup-profile-message ()
  "Show a startup profile message."
  (let ((package-count 0))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count
            (+ (hash-table-count straight--profile-cache) package-count)))
    (if (zerop package-count)
        (message "Emacs loaded in %s with %d garbage collections."
                 (format
                  "%.2f seconds"
                  (float-time (time-subtract after-init-time before-init-time)))
                 gcs-done)
      (message "Emacs loaded %d packages in %s with %d garbage collections."
               package-count
               (format
                "%.2f seconds"
                (float-time (time-subtract after-init-time before-init-time)))
               gcs-done))))

(add-hook 'emacs-startup-hook #'emacs-custom-config:startup-profile-message)

;; Set package

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(eval-when-compile (require 'use-package))
(use-package diminish :ensure t)

;; Set no-littering

(use-package no-littering :ensure t)

(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(customize-set-variable 'auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Defaults

(global-unset-key (kbd "C-z")) ; C-x C-z still `suspend-frame' if needed
(customize-set-variable 'display-time-format "%d/%m/%Y %H:%M")

(setq-default cursor-type 'bar
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent 'complete ; indent if needed, else complete
              word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil
              fill-column 80
              find-file-visit-truename t ; follow symlinks
              vc-follow-symlinks t
              vc-handled-backends '(Git) ; only git, read the docs before change
              kill-buffer-query-functions nil ; kill process when kill a buffer
              revert-without-query t ; revert buffer without asking
              enable-recursive-minibuffers t ; useful for vertico
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(use-package emacs
  :custom
  (use-short-answers t) ; yes-or-no-p becomes y-or-n-p
  (confirm-kill-emacs 'yes-or-no-p)
  (visible-bell 1)
  (use-dialog-box nil) ; use minibuffer instead
  (create-lockfiles nil)
  (require-final-newline t)
  (uniquify-buffer-name-style 'forward)
  (large-file-warning-threshold 100000000) ; 100MB
  (line-move-visual t)
  ;; The following clipboard config breaks in emacs 29
  ;; Just using the default too see if tweaks are still necessary
  ;;(save-interprogram-paste-before-kill t)
  ;;(kill-do-not-save-duplicates t)
  ;;(x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (mouse-yank-at-point t)
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :init
  (delete-selection-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-so-long-mode 1)
  (when (fboundp 'pixel-scroll-precision-mode) ; only on emacs 29
    (pixel-scroll-precision-mode 1)))

(use-package autorevert ; revert buffers when changed
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

(use-package savehist ; save command history
  :custom (history-length 25)
  :config (savehist-mode 1))

(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-valho --group-directories-first")
  :hook (dired-mode . dired-hide-details-mode)) ; use ( to show details

(use-package eldoc
  :diminish eldoc-mode
  :custom
  ;; Prevent echo area resize and always prefer buffer (C-h .)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

;; Some modes that I want to treat as `prog-mode' actually are derived from
;; `text-mode' (like XML and others data files).
;; I'll create two modes to hook stuff on it and leave `text-mode' alone.
(define-minor-mode emacs-custom-config:prog-mode
  "Stub mode with modes that should be hooked in `prog-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(define-minor-mode emacs-custom-config:text-mode
  "Stub mode with modes that should be hooked in `text-mode'."
  :init-value nil
  :keymap (make-sparse-keymap))

(add-hook 'prog-mode-hook 'emacs-custom-config:prog-mode)

(use-package whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-style '(face tabs empty trailing tab-mark indentation::space))
  (whitespace-action '(auto-cleanup)) ; clean on save
  :hook (emacs-custom-config:prog-mode . whitespace-mode))

(defun emacs-custom-config:toggle-line-numbers-type ()
  "Toggle between normal and relative line-numbers."
  (interactive)
  (display-line-numbers-mode -1)
  (if (eq display-line-numbers-type 'relative)
      (setq-local display-line-numbers-type t)
    (setq-local display-line-numbers-type 'relative))
  (display-line-numbers-mode +1))

(use-package display-line-numbers
  :hook (emacs-custom-config:prog-mode . display-line-numbers-mode)
  :bind ("C-z n" . emacs-custom-config:toggle-line-numbers-type))

(use-package hl-line
  :hook ((emacs-custom-config:prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

(use-package fill-column
  :custom (display-fill-column-indicator-character ?\u2506)
  :hook (emacs-custom-config:prog-mode . display-fill-column-indicator-mode))

(use-package visual-line-mode
  :hook (emacs-custom-config:text-mode . visual-line-mode))

(defvar emacs-custom-config:fullscreen-p nil
  "Indicate if Emacs is in fullscreen.")

(defun emacs-custom-config:toggle-fullscreen ()
  "Toggle frame fullscreen and display time."
  (interactive)
  (setq emacs-custom-config:fullscreen-p (not emacs-custom-config:fullscreen-p))
  (toggle-frame-fullscreen)
  (if emacs-custom-config:fullscreen-p
      (display-time-mode +1)
    (display-time-mode -1)))
(global-set-key (kbd "<f11>") 'emacs-custom-config:toggle-fullscreen)

(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(use-package eglot
  :custom (eglot-autoshutdown t)
  :commands (eglot-ensure))

(unless (treesit-available-p)
  (use-package tree-sitter
    :ensure t
    :diminish tree-sitter-mode
    :init (global-tree-sitter-mode))
  (use-package tree-sitter-langs :ensure t))

;; Help enhancements packages

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :init (which-key-mode)
  :config (which-key-setup-side-window-bottom))

(use-package helpful
  :ensure t
  :config
  (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-key] #'helpful-key)
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package elisp-demos
  :ensure t
  :init (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Editor enhancements packages

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :hook (emacs-custom-config:prog-mode . drag-stuff-mode)
  :config (drag-stuff-define-keys))

(use-package avy
  :ensure t
  :bind (("C-:" . 'avy-goto-char)
         ("C-z >" . 'avy-goto-char-2)
         ("C-z l" . 'avy-goto-line)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  :init (smartparens-global-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-custom-config:prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :ensure t
  :hook (emacs-custom-config:prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character) ; or 'bitmap or 'column
  (highlight-indent-guides-suppress-auto-error t)
  (highlight-indent-guides-auto-odd-face-perc 1.5)
  (highlight-indent-guides-auto-even-face-perc 1.5)
  (highlight-indent-guides-auto-character-face-perc 35)
  :bind (:map emacs-custom-config:prog-mode-map
              ("C-z g" . highlight-indent-guides-mode)))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 80))

;; Version control packages

(use-package magit
  :ensure t
  :custom (magit-diff-refine-hunk t))

(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :hook (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  :init (global-diff-hl-mode) ; or diff-hl-flydiff-mode
  :config (diff-hl-margin-mode))

(use-package blamer
  :ensure t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :bind ("C-z b" . blamer-mode))

;; IDE packages

(use-package format-all
  :ensure t
  :bind (:map emacs-custom-config:prog-mode-map
              ("C-c <C-tab>" . format-all-buffer)))

(use-package realgud :ensure t :defer t)

;; Completions packages

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-category-defaults nil))

(use-package embark
  :ensure t
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind (("C-."   . embark-act)
         ("C-z ." . embark-dwim) ; alternative to M-.
         ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package vertico
  :ensure t
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package isearch
  :bind ("C-z s" . isearch-forward))

(use-package consult
  :ensure t
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c k" . consult-ripgrep)
         ("C-z m" . consult-minor-mode-menu)
         :map emacs-custom-config:prog-mode-map ("C-z c"  . consult-flymake)
         :map minibuffer-local-map ("C-r" . consult-history)))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay 0.2) ; popupinfo replaces corfu-doc
  (corfu-preselect-first nil) ; avoid wrong completion with <Enter>
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Sanitize the `pcomplete-completions-at-point' Capf.
;; The Capf has undesired side effects on Emacs 28 and earlier.

;; Wrap a chatty Capf and silence it.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;; Purify a broken Capf and ensure that it does not modify the buffer.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package consult-eglot :ensure t)

;; Speel/Code checking

(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode)) ; eglot will hook it in other languages

(use-package ispell
  :if (> (length emacs-custom-config:hunspell-dict-list) 0)
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary (car emacs-custom-config:hunspell-dict-list))
  :config
  (dolist (dict emacs-custom-config:hunspell-dict-list)
    (add-to-list
     'ispell-local-dictionary-alist
     `(,dict "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" ,dict) nil utf-8)))

  ;; Don't spellcheck org blocks
  (add-to-list 'ispell-skip-region-alist
               '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (ispell-set-spellchecker-params))

(defun emacs-custom-config:hunspell-set-local-dict (dict)
  "Set ispell DICT for current buffer."
  (flyspell-mode -1)
  (setq-local ispell-local-dictionary dict)
  (flyspell-mode +1)
  (flyspell-buffer))

(defun emacs-custom-config:consult-hunspell-dict ()
  "Consult interface for dictionary selection."
  (interactive)
  (emacs-custom-config:hunspell-set-local-dict
   (consult--read
    emacs-custom-config:hunspell-dict-list
    :prompt "Change dictionary:"
    :require-match t
    :history t
    :sort nil)))

(use-package flyspell
  :hook (emacs-custom-config:text-mode . flyspell-mode)
  :bind (:map emacs-custom-config:text-mode-map ("C-z c" . flyspell-buffer)
         :map flyspell-mode-map
              ("C-z h" . emacs-custom-config:consult-hunspell-dict)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Programming languages

(use-package c-mode
  :hook (c-mode . eglot-ensure))

(use-package c++-mode
  :hook (c++-mode . eglot-ensure))

(use-package ruby-mode
  :hook ((ruby-mode . eglot-ensure)
         (ruby-mode . tree-sitter-hl-mode)))

(use-package python-mode
  :hook ((python-mode . eglot-ensure)
         (python-mode . tree-sitter-hl-mode)))

(use-package perl-mode
  :hook (perl-mode . eglot-ensure))

(use-package html-mode
  :hook ((html-mode . emacs-custom-config:prog-mode)
         (html-mode . eglot-ensure)
         (html-mode . tree-sitter-hl-mode)))

(use-package mhtml-mode
  :hook ((mhtml-mode . emacs-custom-config:prog-mode)
         (mhtml-mode . eglot-ensure)
         (mhtml-mode . tree-sitter-hl-mode)))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . eglot-ensure))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . eglot-ensure))

(use-package rustic
  :ensure t
  :hook (rustic-mode . tree-sitter-hl-mode)
  :custom (rustic-lsp-client 'eglot))

(use-package go-mode
  :ensure t
  :hook ((go-mode . tree-sitter-hl-mode)
         (go-mode . eglot-ensure)))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . tree-sitter-hl-mode)
         (haskell-mode . eglot-ensure)))

(use-package php-mode
  :ensure t
  :hook ((php-mode . tree-sitter-hl-mode)
         (php-mode . eglot-ensure)))

(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode))

(use-package js-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.jsx\\'"
  :hook ((js-mode . tree-sitter-hl-mode)
         (js-mode . eglot-ensure)
         (js-mode . subword-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :hook ((json-mode . tree-sitter-hl-mode)
         (json-mode . eglot-ensure)))

(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-mode . eglot-ensure)
         (typescript-mode . subword-mode)))

(use-package yaml-mode
  :ensure t
  :mode "Procfile\\'"
  :hook ((yaml-mode . emacs-custom-config:prog-mode)
         (yaml-mode . eglot-ensure)))

(use-package nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :hook (nxml-mode . emacs-custom-config:prog-mode)
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

(use-package conf-mode
  :hook (conf-mode . emacs-custom-config:prog-mode))

(use-package css-mode
  :hook ((css-mode . tree-sitter-hl-mode)
         (css-mode . rainbow-mode)
         (css-mode . eglot-ensure)))

(use-package sass-mode
  :ensure t
  :hook (sass-mode . rainbow-mode))

(use-package scss-mode
  :ensure t
  :hook (scss-mode . rainbow-mode))

(use-package julia-mode
  :ensure t
  :hook (julia-mode . tree-sitter-hl-mode))

(use-package csharp-mode
  :ensure t
  :hook ((csharp-mode . csharp-tree-sitter-mode)
         (csharp-mode . eglot-ensure)))

(use-package elixir-mode
  :ensure t
  :hook ((elixir-mode . tree-sitter-hl-mode)
         (elixir-mode . eglot-ensure)))

(use-package lua-mode
  :ensure t
  :hook (lua-mode . eglot-ensure))

(use-package nasm-mode :ensure t)

(use-package sml-mode :ensure t)

(use-package web-mode
  :ensure t
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

(define-derived-mode emacs-custom-config:vue-mode web-mode "Web/Vue"
  "Custom Vue major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . emacs-custom-config:vue-mode))
(add-to-list 'eglot-server-programs '(emacs-custom-config:vue-mode "vls"))
(add-hook 'emacs-custom-config:vue-mode-hook #'eglot-ensure)

(define-derived-mode emacs-custom-config:svelte-mode web-mode "Web/Svelte"
  "Custom Svelte major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist
             '("\\.svelte\\'" . emacs-custom-config:svelte-mode))

;; Text modes

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . emacs-custom-config:text-mode)
         (org-mode . variable-pitch-mode))
  :custom
  (org-return-follows-link t) ; Follow links when press enter
  (org-edit-src-content-indentation 0) ; Disable the extra indentation on code
  (org-catch-invisible-edits 'show-and-error) ; prevent accidental edits
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t) ; hide markers (`org-appear' will show then)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-pretty-entities t) ; show entities as UTF-8 character
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  ;; dont close a TODO with open subtasks
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  ;; Images
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  ;; Agenda
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-redeadline 'time)
  (org-log-into-drawer t)
  ;; testing stuff
  (org-blank-before-new-entry '((heading . t) (plain-list-item . t)))
  (org-adapt-indentation nil)
  :custom-face
  (org-block ((t (:inherit 'fixed-pitch))))
  (org-block-begin-line ((t (:inherit 'fixed-pitch))))
  (org-block-end-line ((t (:inherit 'fixed-pitch))))
  (org-table ((t (:inherit 'fixed-pitch))))
  (org-formula ((t (:inherit 'fixed-pitch))))
  (org-code ((t (:inherit 'fixed-pitch))))
  (org-verbatim ((t (:inherit 'fixed-pitch))))
  (org-special-keyword ((t (:inherit 'fixed-pitch))))
  (org-meta-line ((t (:inherit 'fixed-pitch))))
  (org-checkbox ((t (:inherit 'fixed-pitch))))
  :config
  (plist-put org-format-latex-options :scale 2))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-star ["❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽"])
  (org-modern-block-fringe nil)
  :hook (org-mode . org-modern-mode))

(when (and (bound-and-true-p emacs-custom-config:roam-dir)
           (file-exists-p emacs-custom-config:roam-dir))
  (use-package org-roam
    :ensure t
    :after org
    :init (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory emacs-custom-config:roam-dir)
    (org-roam-completion-everywhere t)
    (org-roam-completion-system 'default)
    :bind (("C-z r f" . org-roam-node-find)
           ("C-z r i" . org-roam-node-insert)
           ("C-z r c" . org-roam-capture)
           ("C-z r t" . org-roam-buffer-toggle))
    :config (org-roam-setup)))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . emacs-custom-config:text-mode)
         (gfm-mode . emacs-custom-config:text-mode))
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc") ; or multimarkdown
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t))

(use-package auctex
  :ensure t
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . eglot-ensure))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-engine 'luatex)
  :config
  (setq-default TeX-master nil))

(use-package cdlatex
  :ensure t
  :after org
  :diminish (cdlatex-mode org-cdlatex-mode)
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode)))

(use-package pdf-tools :ensure t :defer t)

;; Snippets

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

;; Extras packages (testing area; not in the daily workflow)

(use-package restclient :ensure t :defer t)
(use-package sicp :ensure t)
(use-package nyan-mode :ensure t)
(use-package elfeed
  :ensure t
  :custom (elfeed-feeds
           '("https://sachachua.com/blog/category/emacs-news/feed"
             "https://planet.emacslife.com/atom.xml"
             "https://this-week-in-rust.org/atom.xml"
             "https://thisweek.gnome.org/index.xml")))

(use-package devdocs ; use devdocs-install to install relevant documentantion
  :ensure t
  :bind ("C-z d" . devdocs-lookup))

;; hacky stuff that I dont want to commit
(if (file-exists-p (expand-file-name "mess.el" user-emacs-directory))
    (load (expand-file-name "mess.el" user-emacs-directory)))


;; Reset GC to Finish

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
