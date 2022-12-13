;;; github-primer-theme.el --- port of Github themes for Emacs -*- lexical-binding: t -*-

;; Author: Thiago Polastri
;; Version: 0.0.1

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

;; An attempt at recreating the look and feel of the GitHub design system in
;; Emacs.

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme file which Github theme is based on.
;; Philip Arvidsson created the Github theme file which this file is based on.

;;; Code:

(deftheme github-primer "Port of Github theme for Emacs.")

(defgroup github-primer-theme nil
  "Github Primer theme."
  :group 'faces)

(defcustom github-primer-color-theme "dark-dimmed"
  "Color theme for github-primer-theme.
Available options are: light, light-high-contrast, dark, dark-high-contrast
and dark-dimmed (default)."
  :group 'github-primer-theme
  :type '(choice (const :tag "Light" "light")
                 (const :tag "Light (High Contrast)" "light-high-contrast")
                 (const :tag "Dark" "dark")
                 (const :tag "Dark (High Contrast)" "dark-high-contrast")
                 (const :tag "Dark (Dimmed)" "dark-dimmed")))

(defcustom github-primer-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist before loading the theme."
  :group 'github-primer-theme
  :type '(alist :key-type string :value-type string))

(defun github-primer-colors-alist ()
  "Get the color palette based on `github-primer-color-theme' value.
returns the palette with the override alist applied.
If value is invalid defaults to dark-dimmed."
  (let ((colors-light
         '(("bg" . "#f6f8fa")
           ("bg-alt" . "#fff8c5")
           ("bg-focus" . "#eaeef2")
           ("bg-faint" . "#d0d7de")
           ("bg-modeline" . "#d0d7de")
           ("bg-modeline-inactive" . "#eaeef2")
           ("bg-border" . "#d0d7de")
           ("bg-highlight" . "#b6e3ff")
           ("bg-highlight-alt" . "#ddf4ff")
           ("bg-secondary" . "#ecd8ff")
           ("bg-secondary-alt" . "#fbefff")
           ("bg-success" . "#aceebb")
           ("bg-success-alt" . "#dafbe1")
           ("bg-warning" . "#fae17d")
           ("bg-warning-alt" . "#fff8c5")
           ("bg-error" . "#ffcecb")
           ("bg-error-alt" . "#FFEBE9")
           ("bg-tooltip" . "#fff1e5")
           ("fg" . "#24292f")
           ("fg-comment" . "#57606a")
           ("fg-shadow" . "#6e7781")
           ("fg-string" . "#0a3069")
           ("fg-constant" . "#0550ae")
           ("fg-function" . "#6639ba")
           ("fg-variable" . "#953800")
           ("fg-builtin" . "#0550ae")
           ("fg-type" . "#32383f")
           ("fg-keyword" . "#cf222e")
           ("fg-markup" . "#116329")
           ("fg-success" . "#116329")
           ("fg-warning" . "#4d2d00")
           ("fg-error" . "#a40e26")
           ("fg-alert" . "#1b7c83")
           ("fg-link" . "#0969da")
           ("fg-link-visited" . "#8250df")
           ("fg-rainbow-1" . "#32383f")
           ("fg-rainbow-2" . "#3e1f79")
           ("fg-rainbow-3" . "#0a3069")
           ("fg-rainbow-4" . "#611347")
           ("fg-black" . "#24292f")
           ("fg-black-bright" . "#32383f")
           ("fg-red" . "#a40e26")
           ("fg-red-bright" . "#cf222e")
           ("fg-green" . "#116329")
           ("fg-green-bright" . "#1a7f37")
           ("fg-yellow" . "#4d2d00")
           ("fg-yellow-bright" . "#633c01")
           ("fg-blue" . "#0550ae")
           ("fg-blue-bright" . "#0969da")
           ("fg-magenta" . "#6639ba")
           ("fg-magenta-bright" . "#8250df")
           ("fg-cyan" . "#1b7c83")
           ("fg-cyan-bright" . "#3192aa")
           ("fg-white" . "#6e7781")
           ("fg-white-bright" . "#f6f8fa")
           ("fg-purple" . "#6639ba")
           ("fg-purple-bright" . "#8250df")
           ("fg-pink" . "#99286e")
           ("fg-pink-bright" . "#bf3989")
           ("fg-coral" . "#9E2F1C")
           ("fg-coral-bright" . "#C4432B")
           ("fg-orange" . "#953800")
           ("fg-orange-bright" . "#bc4c00")))
        (colors-light-hc
         '(("bg" . "#FFFFFF")
           ("bg-alt" . "#fcf7be")
           ("bg-focus" . "#E7ECF0")
           ("bg-faint" . "#CED5DC")
           ("bg-modeline" . "#CED5DC")
           ("bg-modeline-inactive" . "#E7ECF0")
           ("bg-border" . "#CED5DC")
           ("bg-highlight" . "#9cd7ff")
           ("bg-highlight-alt" . "#dff7ff")
           ("bg-secondary" . "#e0c5ff")
           ("bg-secondary-alt" . "#faf0fe")
           ("bg-success" . "#82e596")
           ("bg-success-alt" . "#d2fedb")
           ("bg-warning" . "#f0ce53")
           ("bg-warning-alt" . "#fcf7be")
           ("bg-error" . "#ffc1bc")
           ("bg-error-alt" . "#fff0ee")
           ("bg-tooltip" . "#fff2d5")
           ("fg" . "#0E1116")
           ("fg-comment" . "#4B535D")
           ("fg-shadow" . "#66707B")
           ("fg-string" . "#032563")
           ("fg-constant" . "#023b95")
           ("fg-function" . "#512598")
           ("fg-variable" . "#702c00")
           ("fg-builtin" . "#023b95")
           ("fg-type" . "#20252C")
           ("fg-keyword" . "#a0111f")
           ("fg-markup" . "#024c1a")
           ("fg-success" . "#024c1a")
           ("fg-warning" . "#3f2200")
           ("fg-error" . "#86061d")
           ("fg-alert" . "#1b7c83")
           ("fg-link" . "#0349b4")
           ("fg-link-visited" . "#622cbc")
           ("fg-rainbow-1" . "#20252C")
           ("fg-rainbow-2" . "#341763")
           ("fg-rainbow-3" . "#032563")
           ("fg-rainbow-4" . "#53043a")
           ("fg-black" . "#0E1116")
           ("fg-black-bright" . "#20252C")
           ("fg-red" . "#86061d")
           ("fg-red-bright" . "#a0111f")
           ("fg-green" . "#024c1a")
           ("fg-green-bright" . "#055d20")
           ("fg-yellow" . "#3f2200")
           ("fg-yellow-bright" . "#4e2c00")
           ("fg-blue" . "#023b95")
           ("fg-blue-bright" . "#0349b4")
           ("fg-magenta" . "#512598")
           ("fg-magenta-bright" . "#622cbc")
           ("fg-cyan" . "#1b7c83")
           ("fg-cyan-bright" . "#3192aa")
           ("fg-white" . "#66707B")
           ("fg-white-bright" . "#FFFFFF")
           ("fg-purple" . "#512598")
           ("fg-purple-bright" . "#622cbc")
           ("fg-pink" . "#7d0c57")
           ("fg-pink-bright" . "#971368")
           ("fg-coral" . "#870706")
           ("fg-coral-bright" . "#9f1710")
           ("fg-orange" . "#702c00")
           ("fg-orange-bright" . "#873800")))
        (colors-dark
         '(("bg" . "#0d1117")
           ("bg-alt" . "#010409")
           ("bg-focus" . "#161b22")
           ("bg-faint" . "#21262d")
           ("bg-modeline" . "#21262d")
           ("bg-modeline-inactive" . "#161b22")
           ("bg-border" . "#21262d")
           ("bg-highlight" . "#0c2d6b")
           ("bg-highlight-alt" . "#051d4d")
           ("bg-secondary" . "#3c1e70")
           ("bg-secondary-alt" . "#271052")
           ("bg-success" . "#033a16")
           ("bg-success-alt" . "#04260f")
           ("bg-warning" . "#4b2900")
           ("bg-warning-alt" . "#341a00")
           ("bg-error" . "#67060c")
           ("bg-error-alt" . "#490202")
           ("bg-tooltip" . "#010409")
           ("fg" . "#c9d1d9")
           ("fg-comment" . "#8b949e")
           ("fg-shadow" . "#484f58")
           ("fg-string" . "#a5d6ff")
           ("fg-constant" . "#79c0ff")
           ("fg-function" . "#d2a8ff")
           ("fg-variable" . "#ffa657")
           ("fg-builtin" . "#79c0ff")
           ("fg-type" . "#b1bac4") ; TODO: Problem here
           ("fg-keyword" . "#ff7b72")
           ("fg-markup" . "#56d364")
           ("fg-success" . "#56d364")
           ("fg-warning" . "#e3b341")
           ("fg-error" . "#f85149")
           ("fg-alert" . "#b3f0ff")
           ("fg-link" . "#58a6ff")
           ("fg-link-visited" . "#bc8cff")
           ("fg-rainbow-1" . "#c9d1d9")
           ("fg-rainbow-2" . "#e2c5ff")
           ("fg-rainbow-3" . "#a5d6ff")
           ("fg-rainbow-4" . "#ffbedd")
           ("fg-black" . "#0d1117")
           ("fg-black-bright" . "#161b22")
           ("fg-red" . "#ff7b72")
           ("fg-red-bright" . "#ffa198")
           ("fg-green" . "#3fb950")
           ("fg-green-bright" . "#56d364")
           ("fg-yellow" . "#d29922")
           ("fg-yellow-bright" . "#e3b341")
           ("fg-blue" . "#58a6ff")
           ("fg-blue-bright" . "#79c0ff")
           ("fg-magenta" . "#bc8cff")
           ("fg-magenta-bright" . "#d2a8ff")
           ("fg-cyan" . "#76e3ea")
           ("fg-cyan-bright" . "#b3f0ff")
           ("fg-white" . "#8b949e")
           ("fg-white-bright" . "#c9d1d9")
           ("fg-purple" . "#bc8cff")
           ("fg-purple-bright" . "#d2a8ff")
           ("fg-pink" . "#f778ba")
           ("fg-pink-bright" . "#ff9bce")
           ("fg-coral" . "#F78166")
           ("fg-coral-bright" . "#FFA28B")
           ("fg-orange" . "#f0883e")
           ("fg-orange-bright" . "#ffa657")))
        (colors-dark-hc
         '(("bg" . "#0a0c10")
           ("bg-alt" . "#010409")
           ("bg-focus" . "#1c1f25")
           ("bg-faint" . "#272b33")
           ("bg-modeline" . "#272b33")
           ("bg-modeline-inactive" . "#1c1f25")
           ("bg-border" . "#272b33")
           ("bg-highlight" . "#1e60d5")
           ("bg-highlight-alt" . "#194fb1")
           ("bg-secondary" . "#8031f7")
           ("bg-secondary-alt" . "#6921d7")
           ("bg-success" . "#007728")
           ("bg-success-alt" . "#006222")
           ("bg-warning" . "#945d02")
           ("bg-warning-alt" . "#7b4900")
           ("bg-error" . "#cc1421")
           ("bg-error-alt" . "#ad0116")
           ("bg-tooltip" . "#010409")
           ("fg" . "#f0f3f6")
           ("fg-comment" . "#bdc4cc")
           ("fg-shadow" . "#7a828e")
           ("fg-string" . "#addcff")
           ("fg-constant" . "#91cbff")
           ("fg-function" . "#dbb7ff")
           ("fg-variable" . "#ffb757")
           ("fg-builtin" . "#91cbff")
           ("fg-type" . "#d9dee3")
           ("fg-keyword" . "#ff9492")
           ("fg-markup" . "#4ae168")
           ("fg-success" . "#4ae168")
           ("fg-warning" . "#f7c843")
           ("fg-error" . "#ff6a69")
           ("fg-alert" . "#b3f0ff")
           ("fg-link" . "#71b7ff")
           ("fg-link-visited" . "#cb9eff")
           ("fg-rainbow-1" . "#f0f3f6")
           ("fg-rainbow-2" . "#e6ccff")
           ("fg-rainbow-3" . "#addcff")
           ("fg-rainbow-4" . "#ffc7e1")
           ("fg-black" . "#0a0c10")
           ("fg-black-bright" . "#1c1f25")
           ("fg-red" . "#ff9492")
           ("fg-red-bright" . "#ffb1af")
           ("fg-green" . "#26cd4d")
           ("fg-green-bright" . "#4ae168")
           ("fg-yellow" . "#f0b72f")
           ("fg-yellow-bright" . "#f7c843")
           ("fg-blue" . "#71b7ff")
           ("fg-blue-bright" . "#91cbff")
           ("fg-magenta" . "#cb9eff")
           ("fg-magenta-bright" . "#dbb7ff")
           ("fg-cyan" . "#76e3ea")
           ("fg-cyan-bright" . "#b3f0ff")
           ("fg-white" . "#bdc4cc")
           ("fg-white-bright" . "#f0f3f6")
           ("fg-purple" . "#cb9eff")
           ("fg-purple-bright" . "#dbb7ff")
           ("fg-pink" . "#ff8dc7")
           ("fg-pink-bright" . "#ffadd4")
           ("fg-coral" . "#FF967D")
           ("fg-coral-bright" . "#FFB39B")
           ("fg-orange" . "#fe9a2d")
           ("fg-orange-bright" . "#ffb757")))
        (colors-dark-dimmed
         '(("bg" . "#22272e")
           ("bg-alt" . "#1c2128")
           ("bg-focus" . "#2d333b")
           ("bg-faint" . "#373e47")
           ("bg-modeline" . "#2d333b")
           ("bg-modeline-inactive" . "#22272e")
           ("bg-border" . "#373e47")
           ("bg-highlight" . "#143d79")
           ("bg-highlight-alt" . "#0f2d5c")
           ("bg-secondary" . "#472c82")
           ("bg-secondary-alt" . "#352160")
           ("bg-success" . "#1b4721")
           ("bg-success-alt" . "#113417")
           ("bg-warning" . "#593600")
           ("bg-warning-alt" . "#452700")
           ("bg-error" . "#78191b")
           ("bg-error-alt" . "#5D0F12")
           ("bg-tooltip" . "#1c2128")
           ("fg" . "#adbac7")
           ("fg-comment" . "#768390")
           ("fg-shadow" . "#545d68")
           ("fg-string" . "#96d0ff")
           ("fg-constant" . "#6cb6ff")
           ("fg-function" . "#dcbdfb")
           ("fg-variable" . "#f69d50")
           ("fg-builtin" . "#6cb6ff")
           ("fg-type" . "#909dab")
           ("fg-keyword" . "#f47067")
           ("fg-markup" . "#6bc46d")
           ("fg-success" . "#6bc46d")
           ("fg-warning" . "#daaa3f")
           ("fg-error" . "#e5534b")
           ("fg-alert" . "#56d4dd")
           ("fg-link" . "#539bf5")
           ("fg-link-visited" . "#b083f0")
           ("fg-rainbow-1" . "#adbac7")
           ("fg-rainbow-2" . "#dcbdfb")
           ("fg-rainbow-3" . "#96d0ff")
           ("fg-rainbow-4" . "#ffb3d8")
           ("fg-black" . "#22272e")
           ("fg-black-bright" . "#2d333b")
           ("fg-red" . "#f47067")
           ("fg-red-bright" . "#ff938a")
           ("fg-green" . "#57ab5a")
           ("fg-green-bright" . "#6bc46d")
           ("fg-yellow" . "#c69026")
           ("fg-yellow-bright" . "#daaa3f")
           ("fg-blue" . "#539bf5")
           ("fg-blue-bright" . "#6cb6ff")
           ("fg-magenta" . "#b083f0")
           ("fg-magenta-bright" . "#dcbdfb")
           ("fg-cyan" . "#39c5cf")
           ("fg-cyan-bright" . "#56d4dd")
           ("fg-white" . "#768390")
           ("fg-white-bright" . "#adbac7")
           ("fg-purple" . "#b083f0")
           ("fg-purple-bright" . "#dcbdfb")
           ("fg-pink" . "#e275ad")
           ("fg-pink-bright" . "#fc8dc7")
           ("fg-coral" . "#EC775C")
           ("fg-coral-bright" . "#F79981")
           ("fg-orange" . "#e0823d")
           ("fg-orange-bright" . "#f69d50"))))
    (pcase github-primer-color-theme
      ("light"
       (append colors-light github-primer-override-colors-alist))
      ("light-high-contrast"
       (append colors-light-hc github-primer-override-colors-alist))
      ("dark"
       (append colors-dark github-primer-override-colors-alist))
      ("dark-high-contrast"
       (append colors-dark-hc github-primer-override-colors-alist))
      (_ (append colors-dark-dimmed github-primer-override-colors-alist)))))

(defmacro github-primer-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (github-primer-colors-alist)))
     ,@body))

(github-primer-with-color-variables
 (custom-theme-set-faces
  'github-primer

  ;; default & basic faces
  `(default ((,class :foreground ,fg :background ,bg)))
  `(fringe ((,class :foreground ,fg :background ,bg)))
  `(cursor ((,class :foreground ,fg :background ,fg)))
  `(shadow ((,class :foreground ,fg-comment)))
  `(file-name-shadow ((,class :inherit 'shadow)))
  `(link ((,class :foreground ,fg-link :underline t)))
  `(link-visited ((,class :foreground ,fg-link-visited :underline t)))
  `(button ((,class :inherit 'link)))
  `(browse-url-button ((,class :inherit 'link)))
  `(success ((,class :foreground ,fg-success :bold t)))
  `(warning ((,class :foreground ,fg-warning :bold t)))
  `(error ((,class :foreground ,fg-error :bold t)))

  ;; font-lock
  `(font-lock-builtin-face ((,class :foreground ,fg-builtin)))
  `(font-lock-constant-face ((,class :foreground ,fg-constant)))
  `(font-lock-function-name-face ((,class :foreground ,fg-function)))
  `(font-lock-string-face ((,class :foreground ,fg-string)))
  `(font-lock-keyword-face ((,class :foreground ,fg-keyword)))
  `(font-lock-variable-name-face ((,class :foreground ,fg-variable)))
  `(font-lock-type-face ((,class :foreground ,fg :italic t))) ; fg-type
  `(font-lock-warning-face ((,class :foreground ,fg-error :bold t)))
  `(font-lock-comment-face ((,class :foreground ,fg-comment :italic t)))
  `(font-lock-comment-delimiter-face ((,class :inherit 'font-lock-comment-face
                                              :italic nil)))
  `(font-lock-doc-face ((,class :inherit 'font-lock-string-face)))
  `(font-lock-doc-markup-face ((,class :inherit 'font-lock-constant-face)))
  `(font-lock-preprocessor-face ((,class :inherit 'font-lock-builtin-face)))
  `(font-lock-regexp-grouping-backslash ((,class :inherit 'bold)))
  `(font-lock-regexp-grouping-construct ((,class :inherit 'bold)))
  `(elisp-shorthand-font-lock-face ((,class :inherit 'font-lock-keyword-face)))

  ;; special characters
  `(trailing-whitespace ((,class :background ,bg-error)))
  `(escape-glyph ((,class :foreground ,fg-alert)))
  `(nobreak-hyphen ((,class :inherit 'escape-glyph)))
  `(nobreak-space ((,class :inherit 'escape-glyph :underline t)))
  `(homoglyph ((,class :foreground ,fg-alert)))
  `(glyphless-char ((,class :height 0.6)))

  ;; borders & lines
  `(vertical-border ((,class :foreground ,bg-border)))
  `(border ((,class :background ,bg-border)))
  `(internal-border ((,class :inherit 'border)))
  `(window-divider ((,class :inherit 'vertical-border)))
  `(window-divider-first-pixel ((,class :inherit 'vertical-border)))
  `(window-divider-last-pixel ((,class :inherit 'vertical-border)))
  `(separator-line ((,class :background ,bg-border :height 0.1)))
  `(fill-column-indicator ((,class :foreground ,bg-focus)))

  ;; line-numbers
  `(line-number ((,class :foreground ,fg-shadow :background ,bg)))
  `(line-number-current-line ((,class :foreground ,fg :background ,bg-faint)))
  `(line-number-major-tick ((,class :foreground ,fg-shadow
                                    :background ,bg-highlight-alt)))
  `(line-number-minor-tick ((,class :foreground ,fg-shadow
                                    :background ,bg-secondary-alt)))
  `(linum ((,class :inherit 'line-number)))

  ;; region & highlights
  `(hl-line ((,class :background ,bg-focus)))
  `(highlight ((,class :background ,bg-highlight)))
  `(region ((,class :background ,bg-highlight)))
  `(mouse-drag-and-drop-region ((,class :inherit 'region)))
  `(secondary-selection ((,class :background ,bg-secondary)))
  `(next-error ((,class :inherit 'highlight)))
  `(next-error-message ((,class :inherit 'highlight)))

  ;; search & replace
  `(isearch ((,class :foreground ,fg :background ,bg-highlight)))
  `(isearch-fail ((,class :foreground ,fg :background ,bg-error)))
  `(isearch-group-1 ((,class :foreground ,fg :background ,bg-secondary)))
  `(isearch-group-2 ((,class :foreground ,fg :background ,bg-secondary-alt)))
  `(lazy-highlight ((,class :foreground ,fg :background ,bg-secondary-alt)))
  `(match ((,class :foreground ,fg :background ,bg-success)))
  `(query-replace ((,class :inherit 'isearch)))

  ;; mode-line
  `(mode-line ((,class :foreground ,fg
                       :background ,bg-modeline
                       :box (:line-width 1 :color ,bg-border :style nil))))
  `(mode-line-active ((,class :inherit 'mode-line)))
  `(mode-line-inactive ((,class :foreground ,fg-shadow
                                :background ,bg-modeline-inactive
                                :box (:line-width 1
                                      :color ,bg-border
                                      :style nil))))
  `(mode-line-highlight ((,class :foreground ,fg
                                 :background ,bg-highlight
                                 :box nil)))
  `(mode-line-buffer-id ((,class :bold t)))
  `(mode-line-emphasis  ((,class :bold t)))

  ;; minibuffer
  `(minibuffer-prompt ((,class :foreground ,fg-alert :bold nil)))
  `(completions-annotations ((,class :inherit 'shadow :italic t)))
  `(completions-common-part ((,class :foreground ,fg-string)))
  `(completions-first-difference ((,class :inherit 'bold)))
  `(completions-group-separator ((,class :inherit 'shadow :strike-through t)))
  `(completions-group-title ((,class :inherit 'completions-annotations)))
  `(completions-highlight ((,class :inherit 'highlight)))
  `(buffer-menu-buffer ((,class :bold t)))

  ;; help & doc
  `(eldoc-highlight-function-argument ((,class :inherit 'bold)))
  `(help-argument-name ((,class :inherit 'italic)))
  `(help-for-help-header ((,class :height 1.26)))
  `(help-key-binding ((,class :foreground ,fg-keyword
                              :background ,bg-alt
                              :box (:line-width 1
                                    :color ,bg-border
                                    :style nil))))

  ;; whitespace-mode
  `(whitespace-space ((,class :foreground ,fg-shadow :background unspecified)))
  `(whitespace-space-after-tab ((,class :foreground ,fg-shadow
                                        :background unspecified)))
  `(whitespace-space-before-tab ((,class :foreground ,fg-shadow
                                         :background unspecified)))
  `(whitespace-hspace ((,class :foreground ,fg-shadow :background unspecified)))
  `(whitespace-tab ((,class :foreground ,fg-shadow :background unspecified)))
  `(whitespace-newline ((,class :foreground ,fg-shadow :background unspecified)))
  `(whitespace-trailing ((,class :foreground ,fg-comment :background ,bg-error)))
  `(whitespace-line ((,class :foreground ,fg-comment :background ,bg-error)))
  `(whitespace-missing-newline-at-eof ((,class :foreground ,fg-comment
                                               :background ,bg-warning)))
  `(whitespace-empty ((,class :foreground unspecified :background unspecified)))
  `(whitespace-indentation ((,class :foreground ,fg-comment
                                    :background ,bg-warning-alt)))
  `(whitespace-big-indent ((,class :foreground ,fg-comment
                                   :background ,bg-error-alt)))

  ;; show-paren
  `(show-paren-match ((,class :foreground ,fg :background ,bg-highlight-alt)))
  `(show-paren-match-expression ((,class :inherit 'show-paren-match)))
  `(show-paren-mismatch ((,class :foreground ,fg :background ,bg-error)))

  ;; general ui
  `(header-line ((,class :inherit 'mode-line)))
  `(header-line-highlight ((,class :inherit 'mode-line-highlight)))
  `(tooltip ((,class :background ,bg-tooltip)))
  `(tool-bar ((,class :foreground ,fg :background ,bg-alt)))
  `(widget-field ((,class :foreground ,fg
                          :background ,bg-faint
                          :box (:line-width 1 :color ,bg-border :style nil))))

  ;; Compilation
  `(compilation-error ((,class :inherit 'error)))
  `(compilation-warning ((,class :inherit 'warning)))
  `(compilation-info ((,class :inherit 'success)))
  `(compilation-line-number ((,class :foreground ,fg-alert)))
  `(compilation-column-number ((,class :foreground ,fg-string)))
  `(compilation-mode-line-exit ((,class :inherit 'success :bold t)))
  `(compilation-mode-line-fail ((,class :inherit 'error)))
  `(compilation-mode-line-run ((,class :inherit 'warning :bold t)))

  ;; hi-lock
  `(hi-blue ((,class :background ,fg-blue :foreground ,bg)))
  `(hi-green ((,class :background ,fg-green :foreground ,bg)))
  `(hi-pink ((,class :background ,fg-pink :foreground ,bg)))
  `(hi-yellow ((,class :background ,fg-yellow :foreground ,bg)))
  `(hi-blue-b ((,class :foreground ,fg-blue :bold t)))
  `(hi-green-b ((,class :foreground ,fg-green :bold t)))
  `(hi-red-b ((,class :foreground ,fg-red :bold t)))

  ;; ansi-color
  `(ansi-color-black ((,class :foreground ,fg-black :background ,fg-black)))
  `(ansi-color-red ((,class :foreground ,fg-red :background ,fg-red)))
  `(ansi-color-green ((,class :foreground ,fg-green :background ,fg-green)))
  `(ansi-color-yellow ((,class :foreground ,fg-yellow :background ,fg-yellow)))
  `(ansi-color-blue ((,class :foreground ,fg-blue :background ,fg-blue)))
  `(ansi-color-magenta ((,class :foreground ,fg-magenta
                                :background ,fg-magenta)))
  `(ansi-color-cyan ((,class :foreground ,fg-cyan :background ,fg-cyan)))
  `(ansi-color-white ((,class :foreground ,fg-white :background ,fg-white)))
  `(ansi-color-bright-black ((,class :foreground ,fg-black-bright
                                     :background ,fg-black-bright)))
  `(ansi-color-bright-red ((,class :foreground ,fg-red-bright
                                   :background ,fg-red-bright)))
  `(ansi-color-bright-green ((,class :foreground ,fg-green-bright
                                     :background ,fg-green-bright)))
  `(ansi-color-bright-yellow ((,class :foreground ,fg-yellow-bright
                                      :background ,fg-yellow-bright)))
  `(ansi-color-bright-blue ((,class :foreground ,fg-blue-bright
                                    :background ,fg-blue-bright)))
  `(ansi-color-bright-magenta ((,class :foreground ,fg-magenta-bright
                                       :background ,fg-magenta-bright)))
  `(ansi-color-bright-cyan ((,class :foreground ,fg-cyan-bright
                                    :background ,fg-cyan-bright)))
  `(ansi-color-bright-white ((,class :foreground ,fg-white-bright
                                     :background ,fg-white-bright)))

  ;; TTY
  `(tty-menu-disabled-face ((,class :foreground ,fg-shadow
                                    :background ,bg-highlight-alt)))
  `(tty-menu-enabled-face ((,class :foreground ,fg
                                   :background ,bg-highlight-alt)))
  `(tty-menu-selected-face ((,class :foreground ,fg-warning
                                    :background ,bg-error-alt)))

  ;; Flymake & Flyspell
  `(flymake-error ((,class :underline (:style wave :color ,fg-error))))
  `(flymake-note ((,class :underline (:style wave :color ,fg-success))))
  `(flymake-warning ((,class :underline (:style wave :color ,fg-warning))))
  `(flyspell-incorrect ((,class :underline (:style wave :color ,fg-error))))
  `(flyspell-duplicate ((,class :underline (:style wave :color ,fg-warning))))

  ;; diffs
  `(diff-added ((,class :foreground ,fg-success :background ,bg-success-alt)))
  `(diff-indicator-added ((,class :inherit 'diff-added)))
  `(diff-changed ((,class :foreground ,fg-constant
                          :background ,bg-highlight-alt)))
  `(diff-indicator-changed ((,class :inherit 'diff-changed)))
  `(diff-removed ((,class :foreground ,fg-error :background ,bg-error-alt)))
  `(diff-indicator-removed ((,class :inherit 'diff-removed)))
  `(diff-changed-unspecified ((,class :background ,bg-focus)))
  `(diff-error ((,class :foreground ,fg-error :bold t)))
  `(diff-file-header ((,class :background ,bg-focus :bold t)))
  `(diff-function ((,class :inherit 'diff-file-header)))
  `(diff-index ((,class :inherit 'diff-file-header)))
  `(diff-header ((,class :background ,bg-focus)))
  `(diff-hunk-header ((,class :inherit 'diff-header)))
  `(diff-refine-added ((,class :background ,bg-success)))
  `(diff-refine-changed ((,class :background ,bg-success-alt)))
  `(diff-refine-removed ((,class :background ,bg-error)))
  `(diff-nonexistent ((,class :background ,bg-faint)))

  ;; Dired
  `(dired-broken-symlink ((,class :foreground ,fg-warning
                                  :background ,bg-error)))
  `(dired-directory ((,class :foreground ,fg-constant)))
  `(dired-header ((,class :foreground ,fg-string)))

  ;; Tabs
  `(tab-bar ((,class :inherit 'variable-pitch
                     :foreground ,fg
                     :background ,bg-faint)))
  `(tab-bar-tab ((,class :inherit 'tab-bar
                         :background ,bg
                         :box (:line-width 4 :color ,bg :style nil))))
  `(tab-bar-tab-inactive ((,class :inherit 'tab-bar
                                  :foreground ,fg-comment
                                  :background ,bg-faint
                                  :box (:line-width 4
                                        :color ,bg-faint
                                        :style nil))))
  `(tab-bar-tab-group-current ((,class :inherit 'tab-bar-tab)))
  `(tab-bar-tab-group-inactive ((,class :inherit 'tab-bar-tab-inactive)))
  `(tab-bar-tab-ungrouped ((,class :inherit 'tab-bar-tab-inactive)))
  `(tab-line ((,class :inherit 'tab-bar :background ,bg-focus :height 0.9)))


  ;; diff-hl & indicator
  `(diff-indicator-changed ((,class :inherit 'diff-changed)))
  `(diff-indicator-added ((,class :inherit 'diff-added)))
  `(diff-indicator-removed ((,class :inherit 'diff-removed)))
  `(diff-hl-change ((,class :inherit 'diff-changed)))
  `(diff-hl-delete ((,class :inherit 'diff-removed)))
  `(diff-hl-insert ((,class :inherit 'diff-added)))

  ;; corfu
  `(corfu-default ((,class :background ,bg-alt)))
  `(corfu-border ((,class :inherit 'border)))
  `(corfu-current ((,class :inherit 'highlight)))
  `(corfu-bar ((,class :background ,bg-faint)))
  `(corfu-annotations ((,class :inherit 'completions-annotations)))
  `(corfu-deprecated ((,class :inherit 'shadow :strike-through t)))
  `(corfu-echo ((,class :inherit 'completions-annotations)))

  ;; multiple-cursor
  `(mc/cursor-bar-face ((,class :background ,fg-function :height 1)))
  `(mc/cursor-face ((,class :inverse-video t)))
  `(mc/region-face ((,class :inherit 'region)))

  ;; highlight numbers
  `(highlight-numbers-number ((,class :foreground ,fg-string :bold nil)))

  ;; rainbow-delimiters
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg-rainbow-1)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,fg-rainbow-2)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,fg-rainbow-3)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,fg-rainbow-4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,fg-rainbow-1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg-rainbow-2)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,fg-rainbow-3)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,fg-rainbow-4)))
  `(rainbow-delimiters-depth-9-face ((,class :foreground ,fg-rainbow-1)))
  `(rainbow-delimiters-depth-10-face ((,class :foreground ,fg-rainbow-2)))
  `(rainbow-delimiters-depth-11-face ((,class :foreground ,fg-rainbow-3)))
  `(rainbow-delimiters-depth-12-face ((,class :foreground ,fg-rainbow-4)))
  `(rainbow-delimiters-base-error-face ((,class :foreground ,fg-error
                                                :background unspecified)))
  `(rainbow-delimiters-unmatched-face
    ((,class :inherit 'rainbow-delimiters-base-error-face)))
  `(rainbow-delimiters-mismatched-face
    ((,class :inherit 'rainbow-delimiters-base-error-face)))

  ;; marginalia (just fix the wrong italic)
  `(marginalia-file-priv-read ((,class :inherit 'font-lock-type-face
                                       :italic nil)))

  ;; elfeed
  `(elfeed-log-debug-level-face ((,class :foreground ,fg-pink)))
  `(elfeed-log-error-level-face ((,class :foreground ,fg-error)))
  `(elfeed-log-info-level-face ((,class :foreground ,fg-constant)))
  `(elfeed-log-warn-level-face ((,class :foreground ,fg-warning)))
  `(elfeed-search-date-face ((,class :foreground ,fg-function)))
  `(elfeed-search-feed-face ((,class :foreground ,fg-alert)))
  `(elfeed-search-tag-face ((,class :foreground ,fg-markup)))
  `(elfeed-search-title-face ((,class :foreground ,fg)))
  `(elfeed-search-unread-count-face ((,class :foreground ,fg)))
  `(elfeed-search-unread-title-face ((,class :inherit 'bold)))

  ;; Just make org, web and sitter more pleasant while I'm not diving into it
  ;; ORG
  `(org-block ((,class :foreground ,fg :background ,bg-alt)))
  `(org-block-begin-line ((,class :foreground ,fg-comment
                                  :background ,bg-focus)))
  `(org-block-end-line ((,class :foreground ,fg-comment :background ,bg-focus)))

  ;; Tree Sitter:
  `(tree-sitter-hl-face:function.call ((,class :foreground ,fg)))
  `(tree-sitter-hl-face:tag ((,class :foreground ,fg-markup)))

  ;; Web Mode
  `(web-mode-html-attr-name-face ((,class :inherit 'font-lock-constant-face)))
  `(web-mode-html-tag-face ((,class :foreground ,fg-markup)))


  ;; This theme is still incomplete
  ;; TODO:
  ;; - Info-mode
  ;; - customize (custom)
  ;; - eww
  ;; - grep
  ;; - c-annotation-face
  ;; - man
  ;; - woman
  ;; - newsticker
  ;; - avy
  ;; - auctex
  ;; - org (org-ref)
  ;; - vc and better diff (magit ediff)
  ;; - eshell (term)
  ;; - gnus
  ;; - message
  ;; - orderless
  ;; - racket-mode
  ;; - realgud
  ;; - speedbar
  ;; - vertico
  ))

(github-primer-with-color-variables
 (custom-theme-set-variables
  'github-primer
  ;; ansi-color vector (deprecated after Emacs 28)
  `(ansi-color-names-vector [,fg-black
                             ,fg-red
                             ,fg-green
                             ,fg-yellow
                             ,fg-blue
                             ,fg-magenta
                             ,fg-cyan
                             ,fg-white-bright])
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'github-primer)

;;; github-primer-theme.el ends here
