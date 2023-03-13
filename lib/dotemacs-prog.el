;;; dotemacs-prog.el --- Programming languages -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eglot)
(require 'dotemacs)

(customize-set-variable 'inferior-lisp-program "sbcl")

(use-package paredit
  :after smartparens
  :diminish paredit-mode
  :init
  (add-hook 'paredit-mode-hook (lambda () (smartparens-mode -1)))
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'common-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode))

(use-package racket-mode
  ;; language server: racket-langserver
  :hook ((racket-mode . eglot-ensure)
         (racket-mode . paredit-mode)))

(use-package clojure-mode
  ;; language server: clojure-lsp
  :hook ((clojure-mode . eglot-ensure)
         (clojure-mode . paredit-mode)))

(use-package cider :defer t)
(use-package sly :defer t)
(use-package sicp)

;; This will add 0.64s on init. Enable when necessary
;; (when (fboundp 'elpaca)
;;   (elpaca geiser)
;;   (elpaca-wait))
;; (use-package geiser-guile :defer t)

(use-package rustic
  ;; language server: rust-analyzer
  :custom (rustic-lsp-client 'eglot)
  :hook (rustic-mode . superword-mode)
  :init
  ;; treesitter integration not ready yet
  ;; https://github.com/brotzeit/rustic/issues/475
  (dotemacs-use-treesit
   :lang 'rust
   :github "tree-sitter/tree-sitter-rust"))

(use-package css-mode
  ;; language server: vscode-langservers-extracted
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'css
   :github "tree-sitter/tree-sitter-css"
   :remap '(css-mode . css-ts-mode))
  :hook ((css-mode . eglot-ensure)
         (css-mode . rainbow-mode)
         (css-ts-mode . eglot-ensure)
         (css-ts-mode . rainbow-mode)))

(use-package npm-mode :diminish npm-mode)

(use-package javascript-mode
  ;; language server: typescript-language-server
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'javascript
   :github "tree-sitter/tree-sitter-javascript"
   :remap '(javascript-mode . js-ts-mode))
  :hook ((javascript-mode . eglot-ensure)
         (javascript-mode . subword-mode)
         (javascript-mode . npm-mode)
         (js-ts-mode . eglot-ensure)
         (js-ts-mode . subword-mode)
         (js-ts-mode . npm-mode)))

(use-package typescript-mode
  ;; language server: typescript-language-server
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
  :hook ((typescript-mode . eglot-ensure)
         (typescript-mode . subword-mode)
         (typescript-mode . npm-mode)
         (typescript-ts-mode . eglot-ensure)
         (typescript-ts-mode . subword-mode)
         (typescript-ts-mode . npm-mode)
         (tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . subword-mode)
         (typescript-ts-mode . npm-mode)))

(use-package json-mode
  ;; language server: vscode-langservers-extracted
  :init
  (dotemacs-use-treesit
   :lang 'json
   :github "tree-sitter/tree-sitter-json"
   :remap '(json-mode . json-ts-mode))
  :hook ((json-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)))

(use-package conf-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'toml
   :github "tree-sitter/tree-sitter-toml"
   :remap '(conf-toml-mode . toml-ts-mode))
  :hook (conf-mode . dotemacs-prog-mode))

(use-package sh-mode
  ;; language server: bash-language-server
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'bash
   :github "tree-sitter/tree-sitter-bash"
   :remap '(sh-mode . bash-ts-mode))
  :hook ((sh-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)))

(use-package c-mode
  ;; language server: clangd
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'c
   :github "tree-sitter/tree-sitter-c"
   :remap '(c-mode . c-ts-mode))
  :hook (c-mode . eglot-ensure))

(use-package c++-mode
  ;; language server: clangd
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'cpp
   :github "tree-sitter/tree-sitter-cpp"
   :remap '(c++-mode . c++-ts-mode))
  :hook (c++-mode . eglot-ensure))

(use-package cmake-mode
  :init
  (dotemacs-use-treesit
   :lang 'cmake
   :github "uyha/tree-sitter-cmake"
   :remap '(cmake-mode . cmake-ts-mode)))

(use-package csharp-mode
  ;; language server: omnisharp
  :init
  (dotemacs-use-treesit
   :lang 'csharp
   :github "tree-sitter/tree-sitter-c-sharp"
   :remap '(csharp-mode . csharp-ts-mode))
  :hook ((csharp-mode . eglot-ensure)
         (csharp-ts-mode . eglot-ensure)))

(use-package pyvenv)
(use-package python-mode
  ;; language server: pylsp
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'python
   :github "tree-sitter/tree-sitter-python"
   :remap '(python-mode . python-ts-mode))
  :hook ((python-mode . eglot-ensure)
         (python-mode . pyvenv-mode)
         (python-ts-mode . eglot-ensure)
         (python-ts-mode . pyvenv-mode)))

(use-package ruby-mode
  ;; language server: solargraph
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'ruby
   :github "tree-sitter/tree-sitter-ruby"
   :remap '(ruby-mode . ruby-ts-mode))
  :hook ((ruby-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure)))

(use-package yaml-mode
  ;; language server: yaml-language-server
  :init
  (dotemacs-use-treesit
   :lang 'yaml
   :github "ikatyang/tree-sitter-yaml"
   :remap '(yaml-mode . yaml-ts-mode))
  :hook ((yaml-mode . eglot-ensure)
         (yaml-mode . dotemacs-prog-mode)
         (yaml-ts-mode . eglot-ensure)
         (yaml-ts-mode . dotemacs-prog-mode)))

(use-package java-mode
  ;; language server: Eclipse JDT Language Server
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'java
   :github "tree-sitter/tree-sitter-java"
   :remap '(java-mode . java-ts-mode))
  :hook ((java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)))

(use-package dockerfile-mode
  :init
  (dotemacs-use-treesit
   :lang 'dockerfile
   :github "camdencheek/tree-sitter-dockerfile"
   :remap '(dockerfile-mode . dockerfile-ts-mode)))

(use-package go-mode
  ;; language server: gopls
  :init
  (dotemacs-use-treesit
   :lang 'go
   :github "tree-sitter/tree-sitter-go"
   :remap '(go-mode . go-ts-mode))
  (dotemacs-use-treesit
   :lang 'go-mod
   :github "camdencheek/tree-sitter-go-mod"
   :remap '(go-dot-mod-mode . go-mod-ts-mode))
  :hook ((go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)))

(use-package php-mode
  ;; language server: php-language-server
  ;; tree-sitter grammar: https://github.com/tree-sitter/tree-sitter-php
  :hook (php-mode . eglot-ensure))

(use-package haskell-mode
  ;; language server: hls
  ;; tree-sitter grammar: https://github.com/tree-sitter/tree-sitter-haskell
  :hook (haskell-mode . eglot-ensure))

(use-package elixir-mode
  ;; language server: elixir-ls
  :hook (elixir-mode . eglot-ensure))

(use-package lua-mode
  ;; language server: lua-lsp
  ;; tree-sitter grammar: https://github.com/Azganoth/tree-sitter-lua
  :hook (lua-mode . eglot-ensure))

(use-package zig-mode
  ;; language server: zls
  ;; tree-sitter grammar: https://github.com/GrayJack/tree-sitter-zig
  :hook (zig-mode . eglot-ensure))

(use-package sgml-mode
  ;; language server: vscode-langservers-extracted
  ;; tree-sitter grammar: https://github.com/tree-sitter/tree-sitter-html
  :elpaca nil
  :hook ((html-mode . eglot-ensure)
         (html-mode . dotemacs-prog-mode)
         (mhtml-mode . eglot-ensure)
         (mhtml-mode . dotemacs-prog-mode)))

(use-package xml-mode
  :elpaca nil
  :hook ((xml-mode . dotemacs-prog-mode)
         (nxml-mode . dotemacs-prog-mode)))

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
(add-to-list 'eglot-server-programs '(dotemacs-vue-mode "vls"))
(add-hook 'dotemacs-vue-mode-hook #'eglot-ensure)

(define-derived-mode dotemacs-svelte-mode web-mode "Web/Svelte"
  "Custom Svelte major mode derived from `web-mode'.")
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . dotemacs-svelte-mode))
(add-to-list 'eglot-server-programs '(dotemacs-svelte-mode "svelteserver"))
(add-hook 'dotemacs-svelte-mode-hook #'eglot-ensure)

(use-package julia-mode)
(use-package restclient)
(use-package format-all)
(use-package devdocs)
(use-package realgud :defer t)

(provide 'dotemacs-prog)
;;; dotemacs-prog.el ends here
