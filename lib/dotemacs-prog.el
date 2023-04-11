;;; dotemacs-prog.el --- Programming languages -*- lexical-binding: t; -*-

;; Author: Thiago Polastri

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eglot)
(require 'dotemacs)
(require 'dotemacs-modes)

(use-package paredit
  :after smartparens
  :diminish paredit-mode
  :init
  (add-hook 'paredit-mode-hook (lambda () (smartparens-mode -1)))
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'common-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode))

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

(use-package rustic
  :custom (rustic-lsp-client 'eglot)
  :hook (rustic-mode . superword-mode)
  :init
  ;; treesitter integration not ready yet
  ;; https://github.com/brotzeit/rustic/issues/475
  (dotemacs-use-treesit
   :lang 'rust
   :github "tree-sitter/tree-sitter-rust"))

(use-package css-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'css
   :github "tree-sitter/tree-sitter-css"
   :remap '(css-mode . css-ts-mode))
  :hook ((css-mode css-ts-mode) . rainbow-mode))

(use-package npm-mode :diminish npm-mode)

(use-package javascript-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'javascript
   :github "tree-sitter/tree-sitter-javascript"
   :remap '(javascript-mode . js-ts-mode))
  :hook (((javascript-mode js-ts-mode) . subword-mode)
         ((javascript-mode js-ts-mode) . npm-mode)))

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
  :hook (((typescript-mode typescript-ts-mode tsx-ts-mode) . subword-mode)
         ((typescript-mode typescript-ts-mode tsx-ts-mode) . npm-mode)))

(use-package json-mode
  :init
  (dotemacs-use-treesit
   :lang 'json
   :github "tree-sitter/tree-sitter-json"
   :remap '(json-mode . json-ts-mode)))

(use-package conf-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'toml
   :github "tree-sitter/tree-sitter-toml"
   :remap '(conf-toml-mode . toml-ts-mode))
  :hook (conf-mode . dotemacs-prog-mode))

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

(use-package cmake-mode
  :init
  (dotemacs-use-treesit
   :lang 'cmake
   :github "uyha/tree-sitter-cmake"
   :remap '(cmake-mode . cmake-ts-mode)))

(use-package pyvenv)
(use-package python-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'python
   :github "tree-sitter/tree-sitter-python"
   :remap '(python-mode . python-ts-mode))
  :hook ((python-mode python-ts-mode) . pyvenv-mode))

(use-package ruby-mode
  :elpaca nil
  :init
  (dotemacs-use-treesit
   :lang 'ruby
   :github "tree-sitter/tree-sitter-ruby"
   :remap '(ruby-mode . ruby-ts-mode)))

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

(use-package sgml-mode
  :elpaca nil
  :hook ((html-mode mhtml-mode) . dotemacs-prog-mode))

(use-package xml-mode
  :elpaca nil
  :hook ((xml-mode nxml-mode) . dotemacs-prog-mode))

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

(use-package php-mode)
(use-package haskell-mode)
(use-package elixir-mode)
(use-package lua-mode)
(use-package zig-mode)
(use-package julia-mode)
(use-package restclient)
(use-package format-all)
(use-package devdocs)
(use-package sicp)
(use-package realgud :defer t)

(provide 'dotemacs-prog)
;;; dotemacs-prog.el ends here
