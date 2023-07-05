;;; github-primer-theme.el --- port of Github themes for Emacs -*- lexical-binding: t -*-

;; Author: Thiago Polastri
;; Version: 0.0.2
;; Keywords: faces themes
;; Package-Requires: ((emacs "27.1"))

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

;; Usage:
;; (customize-set-variable 'github-primer-color-theme "dark")
;; (load-theme 'github-primer t)

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme file which Github theme is based on.
;; Philip Arvidsson created the Github theme file which this file is based on.

;;; Code:

(deftheme github-primer "Port of Github theme for Emacs.")

(defgroup github-primer-theme nil
  "Github Primer theme."
  :group 'faces
  :prefix "github-primer-")

(defcustom github-primer-color-theme "dark"
  "Color theme for github-primer-theme.
Available options are: light, light-high-contrast, dark, dark-high-contrast
and dark-dimmed (default)."
  :group 'github-primer-theme
  :type '(choice (const :tag "Light" "light")
                 (const :tag "Light (High Contrast)" "light-high-contrast")
                 (const :tag "Light (Colorblind)" "light-colorblind")
                 (const :tag "Light (Tritanopia)" "light-tritanopia")
                 (const :tag "Dark" "dark")
                 (const :tag "Dark (High Contrast)" "dark-high-contrast")
                 (const :tag "Dark (Colorblind)" "dark-colorblind")
                 (const :tag "Dark (Tritanopia)" "dark-tritanopia")
                 (const :tag "Dark (Dimmed)" "dark-dimmed")))

(defcustom github-primer-cycle-list '("light" "dark" "dark-dimmed")
  "List of theme variant to cycle through."
  :group 'github-primer-theme
  :type '(repeat string))

(defcustom github-primer-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist before loading the theme."
  :group 'github-primer-theme
  :type '(alist :key-type string :value-type string))

;;;###autoload
(defun github-primer-cycle ()
  "Cycle through themes."
  (interactive)
  (when-let* ((max (- (length github-primer-cycle-list) 1))
              (pos (seq-position github-primer-cycle-list github-primer-color-theme))
              (next (+ pos 1)))
    (if (> next max)
        (customize-set-variable 'github-primer-color-theme
                                (nth 0 github-primer-cycle-list))
      (customize-set-variable 'github-primer-color-theme
                              (nth next github-primer-cycle-list)))
    (load-theme 'github-primer t)))

(defun github-primer-colors-alist ()
  "Get the color palette based on `github-primer-color-theme' value.
returns the palette with the override alist applied.
If value is invalid defaults to dark.
The colors are generated from https://www.npmjs.com/package/@primer/primitives
json files."
  (let ((colors-light
         ;; (my/get-primer-normalized-colors "light.json" nil)
         '(("fg-rainbow-4" . "#611347") ("fg-rainbow-3" . "#0a3069") ("fg-rainbow-2" . "#3e1f79") ("fg-rainbow-1" . "#32383f") ("fg-dir" . "#0550ae") ("fg-type" . "#24292f") ("fg-variable" . "#953800") ("fg-keyword" . "#a40e26") ("fg-function" . "#8250df") ("fg-string" . "#0a3069") ("fg-constant" . "#0550ae") ("fg-builtin" . "#0550ae") ("fg-error" . "#82071e") ("fg-warning" . "#7d4e00") ("fg-success" . "#116329") ("fg-link-visited" . "#6639ba") ("fg-link" . "#0550ae") ("bd-light" . "#d0d7de") ("bd" . "#afb8c1") ("fg-disabled" . "#8c959f") ("fg-comment" . "#57606a") ("bg-min" . "#afb8c1") ("bg-hl" . "#d0d7de") ("bg" . "#eaeef2") ("fg" . "#24292f") ("bg-max" . "#ffffff") ("fg-max" . "#1b1f24") ("fg-white-bright" . "#eaeef2") ("fg-white" . "#d0d7de") ("fg-black-bright" . "#32383f") ("fg-black" . "#24292f") ("bg-cyan-max" . "#d3f9fe") ("bg-cyan" . "#6bf1fd") ("bg-cyan-min" . "#37dae6") ("fg-cyan-bright" . "#3192aa") ("fg-cyan" . "#1b7c83") ("bg-coral-min" . "#ffb4a1") ("bg-coral" . "#ffd6cc") ("bg-coral-max" . "#fff0eb") ("fg-coral-bright" . "#c4432b") ("fg-coral" . "#9e2f1c") ("fg-coral-dark" . "#801f0f") ("bg-pink-min" . "#ffadda") ("bg-pink" . "#ffd3eb") ("bg-pink-max" . "#ffeff7") ("fg-pink-bright" . "#bf3989") ("fg-pink" . "#99286e") ("fg-pink-dark" . "#772057") ("bg-purple-min" . "#d8b9ff") ("bg-purple" . "#ecd8ff") ("bg-purple-max" . "#fbefff") ("fg-purple-bright" . "#8250df") ("fg-purple" . "#6639ba") ("fg-purple-dark" . "#512a97") ("bg-red-min" . "#ffaba8") ("bg-red" . "#ffcecb") ("bg-red-max" . "#ffebe9") ("fg-red-bright" . "#cf222e") ("fg-red" . "#a40e26") ("fg-red-dark" . "#82071e") ("bg-orange-min" . "#ffb77c") ("bg-orange" . "#ffd8b5") ("bg-orange-max" . "#fff1e5") ("fg-orange-bright" . "#bc4c00") ("fg-orange" . "#953800") ("fg-orange-dark" . "#762c00") ("bg-yellow-min" . "#eac54f") ("bg-yellow" . "#fae17d") ("bg-yellow-max" . "#fff8c5") ("fg-yellow-bright" . "#9a6700") ("fg-yellow" . "#7d4e00") ("fg-yellow-dark" . "#633c01") ("bg-green-min" . "#6fdd8b") ("bg-green" . "#aceebb") ("bg-green-max" . "#dafbe1") ("fg-green-bright" . "#1a7f37") ("fg-green" . "#116329") ("fg-green-dark" . "#044f1e") ("bg-blue-min" . "#80ccff") ("bg-blue" . "#b6e3ff") ("bg-blue-max" . "#ddf4ff") ("fg-blue-bright" . "#0969da") ("fg-blue" . "#0550ae") ("fg-blue-dark" . "#033d8b"))
         )
        (colors-light-hc
         ;; (my/get-primer-normalized-colors "light_high_contrast.json" nil)
         '(("fg-rainbow-4" . "#53043a") ("fg-rainbow-3" . "#032563") ("fg-rainbow-2" . "#341763") ("fg-rainbow-1" . "#20252c") ("fg-dir" . "#023b95") ("fg-type" . "#0e1116") ("fg-variable" . "#702c00") ("fg-keyword" . "#86061d") ("fg-function" . "#622cbc") ("fg-string" . "#032563") ("fg-constant" . "#023b95") ("fg-builtin" . "#023b95") ("fg-error" . "#6e011a") ("fg-warning" . "#603700") ("fg-success" . "#024c1a") ("fg-link-visited" . "#512598") ("fg-link" . "#023b95") ("bd-light" . "#ced5dc") ("bd" . "#acb6c0") ("fg-disabled" . "#88929d") ("fg-comment" . "#4b535d") ("bg-min" . "#acb6c0") ("bg-hl" . "#ced5dc") ("bg" . "#e7ecf0") ("fg" . "#0e1116") ("bg-max" . "#ffffff") ("fg-max" . "#010409") ("fg-white-bright" . "#e7ecf0") ("fg-white" . "#ced5dc") ("fg-black-bright" . "#20252c") ("fg-black" . "#0e1116") ("bg-cyan-max" . "#d5f9fe") ("bg-cyan" . "#3ae4f0") ("bg-cyan-min" . "#2fbfca") ("fg-cyan-bright" . "#3192aa") ("fg-cyan" . "#1b7c83") ("bg-coral-min" . "#ff8f7e") ("bg-coral" . "#ffc2b6") ("bg-coral-max" . "#fff0ed") ("fg-coral-bright" . "#9f1710") ("fg-coral" . "#870706") ("fg-coral-dark" . "#6f0107") ("bg-pink-min" . "#fc87ca") ("bg-pink" . "#ffbde0") ("bg-pink-max" . "#feeff7") ("fg-pink-bright" . "#971368") ("fg-pink" . "#7d0c57") ("fg-pink-dark" . "#660847") ("bg-purple-min" . "#c49bff") ("bg-purple" . "#e0c5ff") ("bg-purple-max" . "#faf0fe") ("fg-purple-bright" . "#622cbc") ("fg-purple" . "#512598") ("fg-purple-dark" . "#411d7b") ("bg-red-min" . "#ff8e8a") ("bg-red" . "#ffc1bc") ("bg-red-max" . "#fff0ee") ("fg-red-bright" . "#a0111f") ("fg-red" . "#86061d") ("fg-red-dark" . "#6e011a") ("bg-orange-min" . "#f99636") ("bg-orange" . "#ffc67b") ("bg-orange-max" . "#fff2d5") ("fg-orange-bright" . "#873800") ("fg-orange" . "#702c00") ("fg-orange-dark" . "#5b2300") ("bg-yellow-min" . "#d5a824") ("bg-yellow" . "#f0ce53") ("bg-yellow-max" . "#fcf7be") ("fg-yellow-bright" . "#744500") ("fg-yellow" . "#603700") ("fg-yellow-dark" . "#4e2c00") ("bg-green-min" . "#43c663") ("bg-green" . "#82e596") ("bg-green-max" . "#d2fedb") ("fg-green-bright" . "#055d20") ("fg-green" . "#024c1a") ("fg-green-dark" . "#013d14") ("bg-blue-min" . "#67b3fd") ("bg-blue" . "#9cd7ff") ("bg-blue-max" . "#dff7ff") ("fg-blue-bright" . "#0349b4") ("fg-blue" . "#023b95") ("fg-blue-dark" . "#022f7a"))
         )
        (colors-light-cb
         ;; (my/get-primer-normalized-colors "light_colorblind.json" nil)
         '(("fg-rainbow-4" . "#611347") ("fg-rainbow-3" . "#0a3069") ("fg-rainbow-2" . "#3e1f79") ("fg-rainbow-1" . "#32383f") ("fg-dir" . "#0550ae") ("fg-type" . "#24292f") ("fg-variable" . "#8a4600") ("fg-keyword" . "#8a4600") ("fg-function" . "#8250df") ("fg-string" . "#0a3069") ("fg-constant" . "#0550ae") ("fg-builtin" . "#0550ae") ("fg-error" . "#6f3800") ("fg-warning" . "#7d4e00") ("fg-success" . "#0550ae") ("fg-link-visited" . "#6639ba") ("fg-link" . "#0550ae") ("bd-light" . "#d0d7de") ("bd" . "#afb8c1") ("fg-disabled" . "#8c959f") ("fg-comment" . "#57606a") ("bg-min" . "#afb8c1") ("bg-hl" . "#d0d7de") ("bg" . "#eaeef2") ("fg" . "#24292f") ("bg-max" . "#ffffff") ("fg-max" . "#1b1f24") ("fg-white-bright" . "#eaeef2") ("fg-white" . "#d0d7de") ("fg-black-bright" . "#32383f") ("fg-black" . "#24292f") ("bg-cyan-max" . "#caf8fe") ("bg-cyan" . "#5af0fc") ("bg-cyan-min" . "#36d7e2") ("fg-cyan-bright" . "#3192aa") ("fg-cyan" . "#1b7c83") ("bg-coral-min" . "#ffb4a1") ("bg-coral" . "#ffd6cc") ("bg-coral-max" . "#fff0eb") ("fg-coral-bright" . "#c4432b") ("fg-coral" . "#9e2f1c") ("fg-coral-dark" . "#801f0f") ("bg-pink-min" . "#ffadda") ("bg-pink" . "#ffd3eb") ("bg-pink-max" . "#ffeff7") ("fg-pink-bright" . "#bf3989") ("fg-pink" . "#99286e") ("fg-pink-dark" . "#772057") ("bg-purple-min" . "#d8b9ff") ("bg-purple" . "#ecd8ff") ("bg-purple-max" . "#fbefff") ("fg-purple-bright" . "#8250df") ("fg-purple" . "#6639ba") ("fg-purple-dark" . "#512a97") ("bg-red-min" . "#ffbc6d") ("bg-red" . "#ffddb0") ("bg-red-max" . "#fff5e8") ("fg-red-bright" . "#b35900") ("fg-red" . "#8a4600") ("fg-red-dark" . "#6f3800") ("bg-orange-min" . "#ffbc6d") ("bg-orange" . "#ffddb0") ("bg-orange-max" . "#fff5e8") ("fg-orange-bright" . "#b35900") ("fg-orange" . "#8a4600") ("fg-orange-dark" . "#6f3800") ("bg-yellow-min" . "#eac54f") ("bg-yellow" . "#fae17d") ("bg-yellow-max" . "#fff8c5") ("fg-yellow-bright" . "#9a6700") ("fg-yellow" . "#7d4e00") ("fg-yellow-dark" . "#633c01") ("bg-green-min" . "#80ccff") ("bg-green" . "#b6e3ff") ("bg-green-max" . "#ddf4ff") ("fg-green-bright" . "#0969da") ("fg-green" . "#0550ae") ("fg-green-dark" . "#033d8b") ("bg-blue-min" . "#80ccff") ("bg-blue" . "#b6e3ff") ("bg-blue-max" . "#ddf4ff") ("fg-blue-bright" . "#0969da") ("fg-blue" . "#0550ae") ("fg-blue-dark" . "#033d8b"))
         )
        (colors-light-t
         ;; (my/get-primer-normalized-colors "light_tritanopia.json" nil)
         '(("fg-rainbow-4" . "#611347") ("fg-rainbow-3" . "#0a3069") ("fg-rainbow-2" . "#3e1f79") ("fg-rainbow-1" . "#32383f") ("fg-dir" . "#0550ae") ("fg-type" . "#24292f") ("fg-variable" . "#a40e26") ("fg-keyword" . "#a40e26") ("fg-function" . "#8250df") ("fg-string" . "#0a3069") ("fg-constant" . "#0550ae") ("fg-builtin" . "#0550ae") ("fg-error" . "#82071e") ("fg-warning" . "#7d4e00") ("fg-success" . "#0550ae") ("fg-link-visited" . "#6639ba") ("fg-link" . "#0550ae") ("bd-light" . "#d0d7de") ("bd" . "#afb8c1") ("fg-disabled" . "#8c959f") ("fg-comment" . "#57606a") ("bg-min" . "#afb8c1") ("bg-hl" . "#d0d7de") ("bg" . "#eaeef2") ("fg" . "#24292f") ("bg-max" . "#ffffff") ("fg-max" . "#1b1f24") ("fg-white-bright" . "#eaeef2") ("fg-white" . "#d0d7de") ("fg-black-bright" . "#32383f") ("fg-black" . "#24292f") ("bg-cyan-max" . "#caf8fe") ("bg-cyan" . "#5af0fc") ("bg-cyan-min" . "#36d7e2") ("fg-cyan-bright" . "#3192aa") ("fg-cyan" . "#1b7c83") ("bg-coral-min" . "#ffb4a1") ("bg-coral" . "#ffd6cc") ("bg-coral-max" . "#fff0eb") ("fg-coral-bright" . "#c4432b") ("fg-coral" . "#9e2f1c") ("fg-coral-dark" . "#801f0f") ("bg-pink-min" . "#ffadda") ("bg-pink" . "#ffd3eb") ("bg-pink-max" . "#ffeff7") ("fg-pink-bright" . "#bf3989") ("fg-pink" . "#99286e") ("fg-pink-dark" . "#772057") ("bg-purple-min" . "#d8b9ff") ("bg-purple" . "#ecd8ff") ("bg-purple-max" . "#fbefff") ("fg-purple-bright" . "#8250df") ("fg-purple" . "#6639ba") ("fg-purple-dark" . "#512a97") ("bg-red-min" . "#ffaba8") ("bg-red" . "#ffcecb") ("bg-red-max" . "#ffebe9") ("fg-red-bright" . "#cf222e") ("fg-red" . "#a40e26") ("fg-red-dark" . "#82071e") ("bg-orange-min" . "#ffaba8") ("bg-orange" . "#ffcecb") ("bg-orange-max" . "#ffebe9") ("fg-orange-bright" . "#cf222e") ("fg-orange" . "#a40e26") ("fg-orange-dark" . "#82071e") ("bg-yellow-min" . "#eac54f") ("bg-yellow" . "#fae17d") ("bg-yellow-max" . "#fff8c5") ("fg-yellow-bright" . "#9a6700") ("fg-yellow" . "#7d4e00") ("fg-yellow-dark" . "#633c01") ("bg-green-min" . "#80ccff") ("bg-green" . "#b6e3ff") ("bg-green-max" . "#ddf4ff") ("fg-green-bright" . "#0969da") ("fg-green" . "#0550ae") ("fg-green-dark" . "#033d8b") ("bg-blue-min" . "#80ccff") ("bg-blue" . "#b6e3ff") ("bg-blue-max" . "#ddf4ff") ("fg-blue-bright" . "#0969da") ("fg-blue" . "#0550ae") ("fg-blue-dark" . "#033d8b"))
         )
        (colors-dark
         ;; (my/get-primer-normalized-colors "dark.json" t)
         '(("fg-rainbow-4" . "#ffbedd") ("fg-rainbow-3" . "#a5d6ff") ("fg-rainbow-2" . "#e2c5ff") ("fg-rainbow-1" . "#c9d1d9") ("fg-dir" . "#79c0ff") ("fg-type" . "#c9d1d9") ("fg-variable" . "#ffa657") ("fg-keyword" . "#ff7b72") ("fg-function" . "#d2a8ff") ("fg-string" . "#a5d6ff") ("fg-constant" . "#79c0ff") ("fg-builtin" . "#79c0ff") ("fg-error" . "#f85149") ("fg-warning" . "#d29922") ("fg-success" . "#3fb950") ("fg-link-visited" . "#bc8cff") ("fg-link" . "#58a6ff") ("bd-light" . "#21262d") ("bd" . "#30363d") ("fg-disabled" . "#484f58") ("fg-comment" . "#8b949e") ("bg-min" . "#21262d") ("bg-hl" . "#161b22") ("bg" . "#0d1117") ("fg" . "#c9d1d9") ("bg-max" . "#010409") ("fg-max" . "#ffffff") ("fg-white-bright" . "#c9d1d9") ("fg-white" . "#b1bac4") ("fg-black-bright" . "#161b22") ("fg-black" . "#0d1117") ("bg-cyan-max" . "#042426") ("bg-cyan" . "#09373a") ("bg-cyan-min" . "#114f53") ("fg-cyan-bright" . "#56d4dd") ("fg-cyan" . "#39c5cf") ("bg-coral-min" . "#872012") ("bg-coral" . "#640d04") ("bg-coral-max" . "#460701") ("fg-coral-bright" . "#ffa28b") ("fg-coral" . "#f78166") ("fg-coral-dark" . "#ea6045") ("bg-pink-min" . "#7d2457") ("bg-pink" . "#5e103e") ("bg-pink-max" . "#42062a") ("fg-pink-bright" . "#ff9bce") ("fg-pink" . "#f778ba") ("fg-pink-dark" . "#db61a2") ("bg-purple-min" . "#553098") ("bg-purple" . "#3c1e70") ("bg-purple-max" . "#271052") ("fg-purple-bright" . "#d2a8ff") ("fg-purple" . "#bc8cff") ("fg-purple-dark" . "#a371f7") ("bg-red-min" . "#8e1519") ("bg-red" . "#67060c") ("bg-red-max" . "#490202") ("fg-red-bright" . "#ffa198") ("fg-red" . "#ff7b72") ("fg-red-dark" . "#f85149") ("bg-orange-min" . "#762d0a") ("bg-orange" . "#5a1e02") ("bg-orange-max" . "#3d1300") ("fg-orange-bright" . "#ffa657") ("fg-orange" . "#f0883e") ("fg-orange-dark" . "#db6d28") ("bg-yellow-min" . "#693e00") ("bg-yellow" . "#4b2900") ("bg-yellow-max" . "#341a00") ("fg-yellow-bright" . "#e3b341") ("fg-yellow" . "#d29922") ("fg-yellow-dark" . "#bb8009") ("bg-green-min" . "#0f5323") ("bg-green" . "#033a16") ("bg-green-max" . "#04260f") ("fg-green-bright" . "#56d364") ("fg-green" . "#3fb950") ("fg-green-dark" . "#2ea043") ("bg-blue-min" . "#0d419d") ("bg-blue" . "#0c2d6b") ("bg-blue-max" . "#051d4d") ("fg-blue-bright" . "#79c0ff") ("fg-blue" . "#58a6ff") ("fg-blue-dark" . "#388bfd"))
         )
        (colors-dark-hc
         ;; (my/get-primer-normalized-colors "dark_high_contrast.json" t)
         '(("fg-rainbow-4" . "#ffc7e1") ("fg-rainbow-3" . "#addcff") ("fg-rainbow-2" . "#e6ccff") ("fg-rainbow-1" . "#f0f3f6") ("fg-dir" . "#91cbff") ("fg-type" . "#f0f3f6") ("fg-variable" . "#ffb757") ("fg-keyword" . "#ff9492") ("fg-function" . "#dbb7ff") ("fg-string" . "#addcff") ("fg-constant" . "#91cbff") ("fg-builtin" . "#91cbff") ("fg-error" . "#ff6a69") ("fg-warning" . "#f0b72f") ("fg-success" . "#26cd4d") ("fg-link-visited" . "#cb9eff") ("fg-link" . "#71b7ff") ("bd-light" . "#272b33") ("bd" . "#525964") ("fg-disabled" . "#7a828e") ("fg-comment" . "#bdc4cc") ("bg-min" . "#272b33") ("bg-hl" . "#272b33") ("bg" . "#0a0c10") ("fg" . "#f0f3f6") ("bg-max" . "#010409") ("fg-max" . "#ffffff") ("fg-white-bright" . "#f0f3f6") ("fg-white" . "#d9dee3") ("fg-black-bright" . "#272b33") ("fg-black" . "#0a0c10") ("bg-cyan-max" . "#155c61") ("bg-cyan" . "#1c7076") ("bg-cyan-min" . "#23848b") ("fg-cyan-bright" . "#56d4dd") ("fg-cyan" . "#39c5cf") ("bg-coral-min" . "#e03b21") ("bg-coral" . "#c62612") ("bg-coral-max" . "#a91500") ("fg-coral-bright" . "#ffb39b") ("fg-coral" . "#ff967d") ("fg-coral-dark" . "#fc704f") ("bg-pink-min" . "#d23d91") ("bg-pink" . "#b72c7d") ("bg-pink-max" . "#9c1d6a") ("fg-pink-bright" . "#ffadd4") ("fg-pink" . "#ff8dc7") ("fg-pink-dark" . "#ef6eb1") ("bg-purple-min" . "#954ffd") ("bg-purple" . "#8031f7") ("bg-purple-max" . "#6921d7") ("fg-purple-bright" . "#dbb7ff") ("fg-purple" . "#cb9eff") ("fg-purple-dark" . "#b780ff") ("bg-red-min" . "#e82a2f") ("bg-red" . "#cc1421") ("bg-red-max" . "#ad0116") ("fg-red-bright" . "#ffb1af") ("fg-red" . "#ff9492") ("fg-red-dark" . "#ff6a69") ("bg-orange-min" . "#bf5e0a") ("bg-orange" . "#a74c00") ("bg-orange-max" . "#8f3c00") ("fg-orange-bright" . "#ffb757") ("fg-orange" . "#fe9a2d") ("fg-orange-dark" . "#e7811d") ("bg-yellow-min" . "#ae7104") ("bg-yellow" . "#945d02") ("bg-yellow-max" . "#7b4900") ("fg-yellow-bright" . "#f7c843") ("fg-yellow" . "#f0b72f") ("fg-yellow-dark" . "#e09b13") ("bg-green-min" . "#008c2c") ("bg-green" . "#007728") ("bg-green-max" . "#006222") ("fg-green-bright" . "#4ae168") ("fg-green" . "#26cd4d") ("fg-green-dark" . "#09b43a") ("bg-blue-min" . "#2672f3") ("bg-blue" . "#1e60d5") ("bg-blue-max" . "#194fb1") ("fg-blue-bright" . "#91cbff") ("fg-blue" . "#71b7ff") ("fg-blue-dark" . "#409eff"))
         )
        (colors-dark-cb
         ;; (my/get-primer-normalized-colors "dark_colorblind.json" t)
         '(("fg-rainbow-4" . "#ffbedd") ("fg-rainbow-3" . "#a5d6ff") ("fg-rainbow-2" . "#e2c5ff") ("fg-rainbow-1" . "#c9d1d9") ("fg-dir" . "#79c0ff") ("fg-type" . "#c9d1d9") ("fg-variable" . "#fdac54") ("fg-keyword" . "#ec8e2c") ("fg-function" . "#d2a8ff") ("fg-string" . "#a5d6ff") ("fg-constant" . "#79c0ff") ("fg-builtin" . "#79c0ff") ("fg-error" . "#d47616") ("fg-warning" . "#d29922") ("fg-success" . "#58a6ff") ("fg-link-visited" . "#bc8cff") ("fg-link" . "#58a6ff") ("bd-light" . "#21262d") ("bd" . "#30363d") ("fg-disabled" . "#484f58") ("fg-comment" . "#8b949e") ("bg-min" . "#21262d") ("bg-hl" . "#161b22") ("bg" . "#0d1117") ("fg" . "#c9d1d9") ("bg-max" . "#010409") ("fg-max" . "#ffffff") ("fg-white-bright" . "#c9d1d9") ("fg-white" . "#b1bac4") ("fg-black-bright" . "#161b22") ("fg-black" . "#0d1117") ("bg-cyan-max" . "#042426") ("bg-cyan" . "#083639") ("bg-cyan-min" . "#114e53") ("fg-cyan-bright" . "#56d4dd") ("fg-cyan" . "#39c5cf") ("bg-coral-min" . "#872012") ("bg-coral" . "#640d04") ("bg-coral-max" . "#460701") ("fg-coral-bright" . "#ffa28b") ("fg-coral" . "#f78166") ("fg-coral-dark" . "#ea6045") ("bg-pink-min" . "#7d2457") ("bg-pink" . "#5e103e") ("bg-pink-max" . "#42062a") ("fg-pink-bright" . "#ff9bce") ("fg-pink" . "#f778ba") ("fg-pink-dark" . "#db61a2") ("bg-purple-min" . "#553098") ("bg-purple" . "#3c1e70") ("bg-purple-max" . "#271052") ("fg-purple-bright" . "#d2a8ff") ("fg-purple" . "#bc8cff") ("fg-purple-dark" . "#a371f7") ("bg-red-min" . "#6c3906") ("bg-red" . "#4e2906") ("bg-red-max" . "#331c04") ("fg-red-bright" . "#fdac54") ("fg-red" . "#ec8e2c") ("fg-red-dark" . "#d47616") ("bg-orange-min" . "#6c3906") ("bg-orange" . "#4e2906") ("bg-orange-max" . "#331c04") ("fg-orange-bright" . "#fdac54") ("fg-orange" . "#ec8e2c") ("fg-orange-dark" . "#d47616") ("bg-yellow-min" . "#693e00") ("bg-yellow" . "#4b2900") ("bg-yellow-max" . "#341a00") ("fg-yellow-bright" . "#e3b341") ("fg-yellow" . "#d29922") ("fg-yellow-dark" . "#bb8009") ("bg-green-min" . "#0d419d") ("bg-green" . "#0c2d6b") ("bg-green-max" . "#051d4d") ("fg-green-bright" . "#79c0ff") ("fg-green" . "#58a6ff") ("fg-green-dark" . "#388bfd") ("bg-blue-min" . "#0d419d") ("bg-blue" . "#0c2d6b") ("bg-blue-max" . "#051d4d") ("fg-blue-bright" . "#79c0ff") ("fg-blue" . "#58a6ff") ("fg-blue-dark" . "#388bfd"))
         )
        (colors-dark-t
         ;; (my/get-primer-normalized-colors "dark_tritanopia.json" t)
         '(("fg-rainbow-4" . "#ffbedd") ("fg-rainbow-3" . "#a5d6ff") ("fg-rainbow-2" . "#e2c5ff") ("fg-rainbow-1" . "#c9d1d9") ("fg-dir" . "#79c0ff") ("fg-type" . "#c9d1d9") ("fg-variable" . "#ffa198") ("fg-keyword" . "#ff7b72") ("fg-function" . "#d2a8ff") ("fg-string" . "#a5d6ff") ("fg-constant" . "#79c0ff") ("fg-builtin" . "#79c0ff") ("fg-error" . "#f85149") ("fg-warning" . "#d29922") ("fg-success" . "#58a6ff") ("fg-link-visited" . "#bc8cff") ("fg-link" . "#58a6ff") ("bd-light" . "#21262d") ("bd" . "#30363d") ("fg-disabled" . "#484f58") ("fg-comment" . "#8b949e") ("bg-min" . "#21262d") ("bg-hl" . "#161b22") ("bg" . "#0d1117") ("fg" . "#c9d1d9") ("bg-max" . "#010409") ("fg-max" . "#ffffff") ("fg-white-bright" . "#c9d1d9") ("fg-white" . "#b1bac4") ("fg-black-bright" . "#161b22") ("fg-black" . "#0d1117") ("bg-cyan-max" . "#042426") ("bg-cyan" . "#083639") ("bg-cyan-min" . "#114e53") ("fg-cyan-bright" . "#56d4dd") ("fg-cyan" . "#39c5cf") ("bg-coral-min" . "#872012") ("bg-coral" . "#640d04") ("bg-coral-max" . "#460701") ("fg-coral-bright" . "#ffa28b") ("fg-coral" . "#f78166") ("fg-coral-dark" . "#ea6045") ("bg-pink-min" . "#7d2457") ("bg-pink" . "#5e103e") ("bg-pink-max" . "#42062a") ("fg-pink-bright" . "#ff9bce") ("fg-pink" . "#f778ba") ("fg-pink-dark" . "#db61a2") ("bg-purple-min" . "#553098") ("bg-purple" . "#3c1e70") ("bg-purple-max" . "#271052") ("fg-purple-bright" . "#d2a8ff") ("fg-purple" . "#bc8cff") ("fg-purple-dark" . "#a371f7") ("bg-red-min" . "#8e1519") ("bg-red" . "#67060c") ("bg-red-max" . "#490202") ("fg-red-bright" . "#ffa198") ("fg-red" . "#ff7b72") ("fg-red-dark" . "#f85149") ("bg-orange-min" . "#8e1519") ("bg-orange" . "#67060c") ("bg-orange-max" . "#490202") ("fg-orange-bright" . "#ffa198") ("fg-orange" . "#ff7b72") ("fg-orange-dark" . "#f85149") ("bg-yellow-min" . "#693e00") ("bg-yellow" . "#4b2900") ("bg-yellow-max" . "#341a00") ("fg-yellow-bright" . "#e3b341") ("fg-yellow" . "#d29922") ("fg-yellow-dark" . "#bb8009") ("bg-green-min" . "#0d419d") ("bg-green" . "#0c2d6b") ("bg-green-max" . "#051d4d") ("fg-green-bright" . "#79c0ff") ("fg-green" . "#58a6ff") ("fg-green-dark" . "#388bfd") ("bg-blue-min" . "#0d419d") ("bg-blue" . "#0c2d6b") ("bg-blue-max" . "#051d4d") ("fg-blue-bright" . "#79c0ff") ("fg-blue" . "#58a6ff") ("fg-blue-dark" . "#388bfd"))
         )
        (colors-dark-dimmed
         ;; (my/get-primer-normalized-colors "dark_dimmed.json" t)
         '(("fg-rainbow-4" . "#ffb3d8") ("fg-rainbow-3" . "#96d0ff") ("fg-rainbow-2" . "#dcbdfb") ("fg-rainbow-1" . "#adbac7") ("fg-dir" . "#6cb6ff") ("fg-type" . "#adbac7") ("fg-variable" . "#f69d50") ("fg-keyword" . "#f47067") ("fg-function" . "#dcbdfb") ("fg-string" . "#96d0ff") ("fg-constant" . "#6cb6ff") ("fg-builtin" . "#6cb6ff") ("fg-error" . "#e5534b") ("fg-warning" . "#c69026") ("fg-success" . "#57ab5a") ("fg-link-visited" . "#b083f0") ("fg-link" . "#539bf5") ("bd-light" . "#373e47") ("bd" . "#444c56") ("fg-disabled" . "#545d68") ("fg-comment" . "#768390") ("bg-min" . "#373e47") ("bg-hl" . "#2d333b") ("bg" . "#22272e") ("fg" . "#adbac7") ("bg-max" . "#1c2128") ("fg-max" . "#cdd9e5") ("fg-white-bright" . "#adbac7") ("fg-white" . "#909dab") ("fg-black-bright" . "#2d333b") ("fg-black" . "#22272e") ("bg-cyan-max" . "#073235") ("bg-cyan" . "#0d4549") ("bg-cyan-min" . "#13555a") ("fg-cyan-bright" . "#56d4dd") ("fg-cyan" . "#39c5cf") ("bg-coral-min" . "#8d291b") ("bg-coral" . "#771d13") ("bg-coral-max" . "#5d1008") ("fg-coral-bright" . "#f79981") ("fg-coral" . "#ec775c") ("fg-coral-dark" . "#de5b41") ("bg-pink-min" . "#7e325a") ("bg-pink" . "#69264a") ("bg-pink-max" . "#551639") ("fg-pink-bright" . "#fc8dc7") ("fg-pink" . "#e275ad") ("fg-pink-dark" . "#c96198") ("bg-purple-min" . "#5936a2") ("bg-purple" . "#472c82") ("bg-purple-max" . "#352160") ("fg-purple-bright" . "#dcbdfb") ("fg-purple" . "#b083f0") ("fg-purple-dark" . "#986ee2") ("bg-red-min" . "#922323") ("bg-red" . "#78191b") ("bg-red-max" . "#5d0f12") ("fg-red-bright" . "#ff938a") ("fg-red" . "#f47067") ("fg-red-dark" . "#e5534b") ("bg-orange-min" . "#7f3913") ("bg-orange" . "#682d0f") ("bg-orange-max" . "#4d210c") ("fg-orange-bright" . "#f69d50") ("fg-orange" . "#e0823d") ("fg-orange-dark" . "#cc6b2c") ("bg-yellow-min" . "#6c4400") ("bg-yellow" . "#593600") ("bg-yellow-max" . "#452700") ("fg-yellow-bright" . "#daaa3f") ("fg-yellow" . "#c69026") ("fg-yellow-dark" . "#ae7c14") ("bg-green-min" . "#245829") ("bg-green" . "#1b4721") ("bg-green-max" . "#113417") ("fg-green-bright" . "#6bc46d") ("fg-green" . "#57ab5a") ("fg-green-dark" . "#46954a") ("bg-blue-min" . "#1b4b91") ("bg-blue" . "#143d79") ("bg-blue-max" . "#0f2d5c") ("fg-blue-bright" . "#6cb6ff") ("fg-blue" . "#539bf5") ("fg-blue-dark" . "#4184e4"))
         ))
    (pcase github-primer-color-theme
      ("light"
       (append colors-light github-primer-override-colors-alist))
      ("light-high-contrast"
       (append colors-light-hc github-primer-override-colors-alist))
      ("light-colorblind"
       (append colors-light-cb github-primer-override-colors-alist))
      ("light-tritanopia"
       (append colors-light-t github-primer-override-colors-alist))
      ("dark-dimmed"
       (append colors-dark-dimmed github-primer-override-colors-alist))
      ("dark-high-contrast"
       (append colors-dark-hc github-primer-override-colors-alist))
      ("dark-colorblind"
       (append colors-dark-cb github-primer-override-colors-alist))
      ("dark-tritanopia"
       (append colors-dark-t github-primer-override-colors-alist))
      (_ (append colors-dark github-primer-override-colors-alist)))))

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

;;;; Built-in
;;;;; default
  `(bold ((,class :weight bold)))
  `(italic ((,class :slant italic)))
  `(bold-italic ((,class :weight bold :slant italic)))
  `(default ((,class :foreground ,fg :background ,bg)))
  `(fringe ((,class :foreground ,fg :background ,bg)))
  `(cursor ((,class :background ,fg)))
  `(shadow ((,class :foreground ,fg-comment)))
  `(file-name-shadow ((,class :inherit 'shadow)))
  `(link ((,class :foreground ,fg-link :underline t)))
  `(link-visited ((,class :foreground ,fg-link-visited :underline t)))
  `(button ((,class :inherit 'link)))
  `(browse-url-button ((,class :inherit 'link)))
  `(success ((,class :foreground ,fg-success :weight bold)))
  `(warning ((,class :foreground ,fg-warning :weight bold)))
  `(error ((,class :foreground ,fg-error :weight bold)))
  `(bookmark-face ((,class :foreground ,fg-variable)))

;;;;; font-lock & outline
  `(font-lock-builtin-face ((,class :foreground ,fg-builtin)))
  `(font-lock-constant-face ((,class :foreground ,fg-constant)))
  `(font-lock-function-name-face ((,class :foreground ,fg-function)))
  `(font-lock-string-face ((,class :foreground ,fg-string)))
  `(font-lock-keyword-face ((,class :foreground ,fg-keyword)))
  `(font-lock-variable-name-face ((,class :foreground ,fg-variable)))
  `(font-lock-type-face ((,class :foreground ,fg-type :slant italic)))
  `(font-lock-warning-face ((,class :foreground ,fg-error :weight bold)))
  `(font-lock-comment-face ((,class :foreground ,fg-comment :slant italic)))
  `(font-lock-comment-delimiter-face ((,class :inherit 'font-lock-comment-face :italic nil)))
  `(font-lock-doc-face ((,class :inherit 'font-lock-string-face)))
  `(font-lock-doc-markup-face ((,class :inherit 'font-lock-constant-face)))
  `(font-lock-preprocessor-face ((,class :inherit 'font-lock-builtin-face)))
  `(font-lock-regexp-grouping-backslash ((,class :inherit 'bold)))
  `(font-lock-regexp-grouping-construct ((,class :inherit 'bold)))
  `(font-lock-regexp-grouping-backslash ((,class :foreground ,fg :weight bold)))
  `(font-lock-regexp-grouping-construct ((,class :foreground ,fg :weight bold)))
  `(font-lock-negation-char-face ((,class :foreground ,fg :weight bold)))
  `(elisp-shorthand-font-lock-face ((,class :inherit 'font-lock-keyword-face)))
  `(outline-1 ((,class :foreground ,fg-cyan))) ; font-lock-function-name-face
  `(outline-2 ((,class :foreground ,fg-blue))) ; font-lock-variable-name-face
  `(outline-3 ((,class :foreground ,fg-purple))) ; font-lock-keyword-face
  `(outline-4 ((,class :foreground ,fg-pink))) ; font-lock-comment-face
  `(outline-5 ((,class :foreground ,fg-red))) ; font-lock-type-face
  `(outline-6 ((,class :foreground ,fg-coral))) ; font-lock-constant-face
  `(outline-7 ((,class :foreground ,fg-orange))) ; font-lock-builtin-face
  `(outline-8 ((,class :foreground ,fg-yellow))) ; font-lock-string-face

;;;;; special characters
  `(trailing-whitespace ((,class :background ,bg-red-max)))
  `(escape-glyph ((,class :foreground ,fg-cyan)))
  `(nobreak-hyphen ((,class :inherit 'escape-glyph)))
  `(nobreak-space ((,class :inherit 'escape-glyph :underline t)))
  `(homoglyph ((,class :foreground ,fg-cyan)))
  `(glyphless-char ((,class :height 0.6)))

;;;;; borders & lines
  `(vertical-border ((,class :foreground ,bd-light)))
  `(border ((,class :background ,bg))) ; bd-light
  `(internal-border ((,class :inherit 'vertical-border)))
  `(window-divider ((,class :inherit 'vertical-border)))
  `(window-divider-first-pixel ((,class :inherit 'vertical-border)))
  `(window-divider-last-pixel ((,class :inherit 'vertical-border)))
  `(separator-line ((,class :background ,bd-light :height 0.1)))
  `(fill-column-indicator ((,class :foreground ,bd-light)))

;;;;; line numbers
  `(line-number ((,class :foreground ,fg-disabled :background ,bg)))
  `(line-number-current-line ((,class :foreground ,fg :background ,bg-min)))
  `(line-number-major-tick ((,class :foreground ,fg-comment :background ,bg-blue-max)))
  `(line-number-minor-tick ((,class :foreground ,fg-comment :background ,bg-purple-max)))
  `(linum ((,class :inherit 'line-number)))

;;;;; region & highlights
  `(hl-line ((,class :background ,bg-hl :extend t)))
  `(highlight ((,class :background ,bg-blue)))
  `(region ((,class :background ,bg-blue :extend t)))
  `(mouse-drag-and-drop-region ((,class :inherit 'region)))
  `(secondary-selection ((,class :background ,bg-purple :extend t)))
  `(next-error ((,class :inherit 'highlight)))
  `(next-error-message ((,class :inherit 'highlight :extend t)))

;;;;; search & replace
  `(isearch ((,class :foreground ,fg :background ,bg-blue)))
  `(isearch-fail ((,class :foreground ,fg :background ,bg-red)))
  `(isearch-group-1 ((,class :foreground ,fg :background ,bg-purple)))
  `(isearch-group-2 ((,class :foreground ,fg :background ,bg-purple-max)))
  `(lazy-highlight ((,class :foreground ,fg :background ,bg-purple-max)))
  `(match ((,class :foreground ,fg :background ,bg-green)))
  `(query-replace ((,class :inherit 'isearch)))

;;;;; minibuffer
  `(minibuffer-prompt ((,class :foreground ,fg-cyan :bold nil)))
  `(completions-annotations ((,class :inherit 'shadow :slant italic)))
  `(completions-common-part ((,class :foreground ,fg-string)))
  `(completions-first-difference ((,class :inherit 'bold)))
  `(completions-group-separator ((,class :inherit 'shadow :strike-through t)))
  `(completions-group-title ((,class :inherit 'completions-annotations)))
  `(completions-highlight ((,class :inherit 'highlight)))
  `(buffer-menu-buffer ((,class :weight bold)))
  `(ido-first-match ((,class :weight bold)))
  `(ido-incomplete-regexp ((,class :inherit 'font-lock-warning-face)))
  `(ido-indicator ((,class :foreground ,fg-yellow :background ,bg-red-max)))
  `(ido-only-match ((,class :foreground ,fg-green)))
  `(ido-subdir ((,class :foreground ,fg-constant)))
  `(ido-virtual ((,class :foreground ,fg-coral)))

;;;;; help & doc
  `(eldoc-highlight-function-argument ((,class :inherit 'bold)))
  `(help-argument-name ((,class :inherit 'italic)))
  `(help-for-help-header ((,class :height 1.26)))
  `(help-key-binding ((,class :foreground ,fg-keyword :background ,bg-max :box (:line-width 1 :color ,bd-light :style nil))))
  `(woman-bold ((,class :inherit 'bold)))
  `(woman-italic ((,class :inherit 'italic)))
  `(woman-addition ((,class :inherit 'font-lock-builtin-face)))
  `(woman-unknown ((,class :inherit 'font-lock-warning-face)))
  ;; defaults for man are fine

;;;;; dired
  `(dired-broken-symlink ((,class :foreground ,fg-warning :background ,bg-red)))
  `(dired-directory ((,class :foreground ,fg-dir))) ; fg-constant
  `(dired-header ((,class :foreground ,fg-string)))
  `(dired-flagged ((,class :inherit 'error)))
  `(dired-ignored ((,class :inherit 'shadow)))
  `(dired-mark ((,class :foreground ,fg-cyan :weight bold))) ; font-lock-constant-face
  `(dired-marked ((,class :inherit 'warning)))
  `(dired-perm-write ((,class :foreground ,fg-red :underline t))) ; font-lock-comment-delimiter-face
  `(dired-symlink ((,class :foreground ,fg-purple))) ; font-lock-keyword-face
  `(dired-warning ((,class :inherit 'font-lock-warning-face)))
  `(dired-set-id ((,class :inherit 'font-lock-warning-face)))
  `(dired-special ((,class :inherit 'font-lock-variable-name-face)))

;;;;; whitespace-mode
  `(whitespace-space ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-space-after-tab ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-space-before-tab ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-hspace ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-tab ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-newline ((,class :foreground ,fg-disabled :background unspecified)))
  `(whitespace-trailing ((,class :foreground ,fg-comment :background ,bg-red-max)))
  `(whitespace-line ((,class :foreground ,fg-comment :background ,bg-red-max)))
  `(whitespace-missing-newline-at-eof ((,class :foreground ,fg-comment :background ,bg-yellow-max)))
  `(whitespace-empty ((,class :foreground unspecified :background unspecified)))
  `(whitespace-indentation ((,class :foreground ,fg-comment :background ,bg-yellow-max)))
  `(whitespace-big-indent ((,class :foreground ,fg-comment :background ,bg-red-max)))

;;;;; show-paren
  `(show-paren-match ((,class :foreground ,fg :background ,bg-blue-max)))
  `(show-paren-match-expression ((,class :inherit 'show-paren-match)))
  `(show-paren-mismatch ((,class :foreground ,fg :background ,bg-red-max)))

;;;;; Flymake & Flyspell
  `(flymake-error ((,class :underline (:style wave :color ,fg-error))))
  `(flymake-note ((,class :underline (:style wave :color ,fg-success))))
  `(flymake-warning ((,class :underline (:style wave :color ,fg-warning))))
  `(flyspell-incorrect ((,class :underline (:style wave :color ,fg-error))))
  `(flyspell-duplicate ((,class :underline (:style wave :color ,fg-warning))))
  `(confusingly-reordered ((,class :underline (:style wave :color ,fg-error))))

;;;;; Compilation
  `(compilation-error ((,class :inherit 'error)))
  `(compilation-warning ((,class :inherit 'warning)))
  `(compilation-info ((,class :inherit 'success)))
  `(compilation-line-number ((,class :foreground ,fg-cyan)))
  `(compilation-column-number ((,class :foreground ,fg-string)))
  `(compilation-mode-line-exit ((,class :inherit 'success :weight bold)))
  `(compilation-mode-line-fail ((,class :inherit 'error)))
  `(compilation-mode-line-run ((,class :inherit 'warning :weight bold)))

;;;;; diff
  `(diff-added ((,class :foreground ,fg-success :background ,bg-green-max :extend t)))
  `(diff-indicator-added ((,class :inherit 'diff-added)))
  `(diff-changed ((,class :foreground ,fg-constant :background ,bg-blue-max)))
  `(diff-indicator-changed ((,class :inherit 'diff-changed :extend t)))
  `(diff-removed ((,class :foreground ,fg-error :background ,bg-red-max :extend t)))
  `(diff-indicator-removed ((,class :inherit 'diff-removed)))
  `(diff-changed-unspecified ((,class :background ,bg-hl :extend t)))
  `(diff-error ((,class :foreground ,fg-error :weight bold)))
  `(diff-file-header ((,class :background ,bg-hl :weight bold :extend t)))
  `(diff-function ((,class :inherit 'diff-file-header)))
  `(diff-index ((,class :inherit 'diff-file-header)))
  `(diff-header ((,class :background ,bg-hl :extend t)))
  `(diff-hunk-header ((,class :inherit 'diff-header)))
  `(diff-refine-added ((,class :background ,bg-green :extend t)))
  `(diff-refine-changed ((,class :background ,bg-green-max :extend t)))
  `(diff-refine-removed ((,class :background ,bg-red :extend t)))
  `(diff-nonexistent ((,class :background ,bg-min)))
  `(diff-indicator-changed ((,class :inherit 'diff-changed)))
  `(diff-indicator-added ((,class :inherit 'diff-added)))
  `(diff-indicator-removed ((,class :inherit 'diff-removed)))
  `(ediff-current-diff-Ancestor ((,class :background ,bg-green-min :extend t)))
  `(ediff-current-diff-A ((,class :background ,bg-red-max :extend t)))
  `(ediff-current-diff-B ((,class :background ,bg-green-max :extend t)))
  `(ediff-current-diff-C ((,class :background ,bg-yellow-max :extend t)))
  `(ediff-even-diff-Ancestor ((,class :distant-foreground ,fg :background ,bg-hl :extend t)))
  `(ediff-even-diff-A ((,class :distant-foreground ,fg :background ,bg-hl :extend t)))
  `(ediff-even-diff-B ((,class :distant-foreground ,fg :background ,bg-min :extend t)))
  `(ediff-even-diff-C ((,class :distant-foreground ,fg :background ,bg-hl :extend t)))
  `(ediff-fine-diff-Ancestor ((,class :background ,bg-cyan)))
  `(ediff-fine-diff-A ((,class :background ,bg-red :extend t)))
  `(ediff-fine-diff-B ((,class :background ,bg-green :extend t)))
  `(ediff-fine-diff-C ((,class :background ,bg-yellow :extend t)))
  `(ediff-odd-diff-Ancestor ((,class :background ,bg-min :extend t)))
  `(ediff-odd-diff-A ((,class :distant-foreground ,fg :background ,bg-min :extend t)))
  `(ediff-odd-diff-B ((,class :distant-foreground ,fg :background ,bg-hl :extend t)))
  `(ediff-odd-diff-C ((,class :distant-foreground ,fg :background ,bg-min :extend t)))

;;;;; hi-lock
  `(hi-aquamarine ((,class :background ,bg-cyan)))
  `(hi-black-b ((,class :foreground ,fg :bold t)))
  `(hi-black-hb ((,class :inherit 'fixed-pitch :height 1.0 :foreground ,fg :background ,bg-max :bold t)))
  `(hi-blue ((,class :background ,bg-blue)))
  `(hi-blue-b ((,class :foreground ,fg-blue :bold t)))
  `(hi-green ((,class :background ,bg-green)))
  `(hi-green-b ((,class :foreground ,fg-green :bold t)))
  `(hi-pink ((,class :background ,bg-pink)))
  `(hi-red-b ((,class :foreground ,fg-red :bold t)))
  `(hi-salmon ((,class :background ,bg-coral)))
  `(hi-yellow ((,class :background ,bg-yellow)))

;;;;; mode-line & ui
  `(mode-line ((,class :foreground ,fg :background ,bg-min :box (:line-width 1 :color ,bd-light :style nil))))
  `(mode-line-active ((,class :inherit 'mode-line)))
  `(mode-line-inactive ((,class :inherit 'mode-line :foreground ,fg-disabled :background ,bg-hl)))
  `(mode-line-highlight ((,class :foreground ,fg :background ,bg-blue :box nil)))
  `(mode-line-buffer-id ((,class :weight bold)))
  `(mode-line-emphasis  ((,class :weight bold)))
  `(header-line ((,class :inherit 'mode-line)))
  `(header-line-highlight ((,class :inherit 'mode-line-highlight)))
  `(tooltip ((,class :background ,bg-max :box (:line-width 1 :color ,bd-light :style nil))))
  `(tool-bar ((,class :foreground ,fg :background ,bg-max)))
  `(widget-field ((,class :foreground ,fg :background ,bg-min :box (:line-width 1 :color ,bd-light :style nil))))
  `(widget-button ((,class :foreground ,fg :weight bold)))
  `(widget-button-pressed ((,class :foreground ,fg-red)))
  `(widget-documentation ((,class :foreground ,fg-green)))
  `(widget-inactive ((,class :inherit 'shadow)))
  `(widget-single-line-field ((,class :foreground ,fg :background ,bg-min)))
  `(custom-button ((,class :foreground ,fg :background ,bg)))
  `(custom-button-mouse ((,class :foreground ,fg-blue :background ,bg)))
  `(custom-button-pressed ((,class :foreground ,fg-disabled :background ,bg)))
  `(custom-button-unraised ((,class :foreground ,fg :background ,bg)))
  `(custom-button-pressed-unraised ((,class :inherit 'custom-button-unraised)))
  `(custom-changed ((,class :foreground ,fg-coral :background ,bg)))
  `(custom-comment ((,class :foreground ,fg :background ,bg-hl)))
  `(custom-comment-tag ((,class :foreground ,fg-comment)))
  `(custom-documentation ((,class :foreground ,fg)))
  `(custom-variable-tag ((,class :foreground ,fg-cyan :weight bold)))
  `(custom-face-tag ((,class :inherit 'custom-variable-tag)))
  `(custom-group-subtitle ((,class :weight bold)))
  `(custom-group-tag ((,class :inherit 'fixed-pitch :foreground ,fg-blue :weight bold :height 1.0)))
  `(custom-group-tag-1 ((,class :foreground ,fg-purple)))
  `(custom-invalid ((,class :foreground ,fg-yellow :background ,bg-red-max)))
  `(custom-link ((,class :inherit 'link)))
  `(custom-modified ((,class :foreground ,fg-coral)))
  `(custom-rogue ((,class :foreground ,fg-pink :background ,bg-max)))
  `(custom-saved ((,class :foreground ,fg-green :weight bold)))
  `(custom-set ((,class :foreground ,fg-blue :background ,bg-max)))
  `(custom-state ((,class :foreground ,fg-green)))
  `(custom-themed ((,class :foreground ,fg-blue-bright :background ,bg-blue-max)))
  `(custom-variable-button ((,class :foreground ,fg :underline t)))
  `(custom-variable-obsolete ((,class :foreground ,fg-blue-bright)))
  `(custom-visibility ((,class :foreground ,fg-blue :height 0.8 :underline t)))
  `(tab-bar ((,class :foreground ,fg :background ,bg-min)))
  `(tab-bar-tab ((,class :inherit 'tab-bar :background ,bg :box (:line-width 4 :color ,bg :style nil))))
  `(tab-bar-tab-inactive ((,class :inherit 'tab-bar :foreground ,fg-comment :background ,bg-min :box (:line-width 4 :color ,bd-light :style nil))))
  `(tab-bar-tab-group-current ((,class :inherit 'tab-bar-tab)))
  `(tab-bar-tab-group-inactive ((,class :inherit 'tab-bar-tab-inactive)))
  `(tab-bar-tab-ungrouped ((,class :inherit 'tab-bar-tab-inactive)))
  `(tab-line ((,class :inherit 'tab-bar :background ,bg-hl :height 0.9)))

;;;;; shell
  `(tty-menu-disabled-face ((,class :foreground ,fg-disabled :background ,bg-blue-max)))
  `(tty-menu-enabled-face ((,class :foreground ,fg :background ,bg-blue-max)))
  `(tty-menu-selected-face ((,class :foreground ,fg-warning :background ,bg-red-max)))
  `(eshell-ls-archive ((,class :foreground ,fg-pink)))
  `(eshell-ls-backup ((,class :foreground ,fg-coral)))
  `(eshell-ls-clutter ((,class :foreground ,fg-orange)))
  `(eshell-ls-directory ((,class :foreground ,fg-blue)))
  `(eshell-ls-executable ((,class :foreground ,fg-green)))
  `(eshell-ls-missing ((,class :foreground ,fg-red)))
  `(eshell-ls-product ((,class :foreground ,fg-coral-bright)))
  `(eshell-ls-readonly ((,class :foreground ,fg-pink-bright)))
  `(eshell-ls-special ((,class :foreground ,fg-purple)))
  `(eshell-ls-symlink ((,class :foreground ,fg-purple)))
  `(eshell-ls-unreadable ((,class :foreground ,fg-disabled)))
  `(eshell-prompt ((,class :foreground ,fg-cyan :weight bold)))
  ;; term will inherit ansi-colors and vterm inherit term

;;;; message
  `(message-cited-text-1 ((,class :foreground ,fg-pink-bright)))
  `(message-cited-text-2 ((,class :foreground ,fg-purple-bright)))
  `(message-cited-text-3 ((,class :foreground ,fg-string)))
  `(message-cited-text-3 ((,class :foreground ,fg-cyan-bright)))
  `(message-header-cc ((,class :foreground ,fg-string :weight bold)))
  `(message-header-name ((,class :foreground ,fg-green-dark)))
  `(message-header-newsgroups ((,class :foreground ,fg-yellow)))
  `(message-header-other ((,class :foreground ,fg-purple)))
  `(message-header-subject ((,class :foreground ,fg-cyan-bright)))
  `(message-header-to ((,class :foreground ,fg-string :weight bold)))
  `(message-header-xheader ((,class :foreground ,fg-comment :slant italic)))
  `(message-mml ((,class :foreground ,fg-green :slant italic)))
  `(message-separator ((,class :foreground ,fg-comment)))

;;;; erc
  `(erc-action-face  ((,class :weight bold)))
  `(erc-button ((,class :weight bold :underline t)))
  `(erc-command-indicator-face ((,class :weight bold)))
  `(erc-current-nick-face ((,class :foreground ,fg-green :weight bold)))
  `(erc-default-face ((,class :background ,bg :foreground ,fg)))
  `(erc-direct-msg-face ((,class :foreground ,fg-coral)))
  `(erc-error-face ((,class :foreground ,fg-red)))
  `(erc-header-line ((,class :background ,bg-hl :foreground ,fg)))
  `(erc-input-face ((,class :foreground ,fg-string)))
  `(erc-my-nick-face ((,class :foreground ,fg-green :weight bold)))
  `(erc-my-nick-prefix-face ((,class :foreground ,fg-green :weight bold)))
  `(erc-nick-default-face ((,class :weight bold)))
  `(erc-nick-msg-face ((,class :foreground ,fg-coral)))
  `(erc-nick-prefix-face ((,class :background ,bg :foreground ,fg)))
  `(erc-notice-face ((,class :foreground ,fg-purple)))
  `(erc-prompt-face ((,class :foreground ,fg-blue-bright :background ,bg-blue-max :weight bold)))
  `(erc-timestamp-face ((,class :foreground ,fg-cyan :weight bold)))

;;;;; ansi
  `(ansi-color-black ((,class :foreground ,fg-black :background ,fg-black)))
  `(ansi-color-white ((,class :foreground ,fg-white :background ,fg-white)))
  `(ansi-color-red ((,class :foreground ,fg-red :background ,fg-red)))
  `(ansi-color-green ((,class :foreground ,fg-green :background ,fg-green)))
  `(ansi-color-yellow ((,class :foreground ,fg-yellow :background ,fg-yellow)))
  `(ansi-color-blue ((,class :foreground ,fg-blue :background ,fg-blue)))
  `(ansi-color-magenta ((,class :foreground ,fg-purple :background ,fg-purple)))
  `(ansi-color-cyan ((,class :foreground ,fg-cyan :background ,fg-cyan)))
  `(ansi-color-bright-black ((,class :foreground ,fg-black-bright :background ,fg-black-bright)))
  `(ansi-color-bright-white ((,class :foreground ,fg-white-bright :background ,fg-white-bright)))
  `(ansi-color-bright-red ((,class :foreground ,fg-red-bright :background ,fg-red-bright)))
  `(ansi-color-bright-green ((,class :foreground ,fg-green-bright :background ,fg-green-bright)))
  `(ansi-color-bright-yellow ((,class :foreground ,fg-yellow-bright :background ,fg-yellow-bright)))
  `(ansi-color-bright-blue ((,class :foreground ,fg-blue-bright :background ,fg-blue-bright)))
  `(ansi-color-bright-magenta ((,class :foreground ,fg-purple-bright :background ,fg-purple-bright)))
  `(ansi-color-bright-cyan ((,class :foreground ,fg-cyan-bright :background ,fg-cyan-bright)))

;;;; External

;;;;; artbollocks
  `(artbollocks-face ((,class :foreground unspecified :background unspecified :underline (:style wave :color ,fg-purple))))
  `(artbollocks-lexical-illusions-face ((,class :foreground unspecified :background unspecified :underline (:style wave :color ,fg-pink))))
  `(artbollocks-passive-voice-face ((,class :foreground unspecified :background unspecified :underline (:style wave :color ,fg-comment))))
  `(artbollocks-weasel-words-face ((,class :foreground unspecified :background unspecified :underline (:style wave :color ,fg-coral))))

;;;;; auctex
  `(TeX-error-description-error ((,class :foreground ,fg-coral :weight bold)))
  `(TeX-error-description-tex-said ((,class :foreground ,fg-string :weight bold)))
  `(TeX-error-description-warning ((,class :foreground ,fg-warning :weight bold)))
  `(font-latex-bold-face ((,class :inherit 'bold :foreground unspecified)))
  `(font-latex-italic-face ((,class :inherit 'italic :foreground unspecified)))
  `(font-latex-math-face ((,class :foreground ,fg-string)))
  `(font-latex-script-char-face ((,class :foreground ,fg-builtin)))
  `(font-latex-sedate-face ((,class :foreground ,fg-comment)))
  `(font-latex-sectioning-0-face ((,class :foreground ,fg-variable :weight ultra-bold :height unspecified)))
  `(font-latex-sectioning-1-face ((,class :foreground ,fg-cyan :weight semi-bold :height unspecified)))
  `(font-latex-sectioning-2-face ((,class :foreground ,fg-purple :weight semi-bold :height unspecified)))
  `(font-latex-sectioning-3-face ((,class :foreground ,fg-variable :weight semi-bold :height unspecified)))
  `(font-latex-sectioning-4-face ((,class :foreground ,fg-cyan :weight semi-bold :height unspecified)))
  `(font-latex-sectioning-5-face ((,class :foreground ,fg-purple :weight semi-bold :height unspecified)))
  `(font-latex-string-face ((,class :inherit 'font-lock-string-face)))
  `(font-latex-verbatim-face ((,class :foreground ,fg-cyan :slant italic)))
  `(font-latex-warning-face ((,class :inherit 'font-lock-warning-face)))

;;;;; avy
  `(avy-background-face ((,class :foreground ,fg-disabled)))
  `(avy-lead-face ((,class :background ,bg-coral :foreground ,fg)))
  `(avy-lead-face-0 ((,class :background ,bg-purple :foreground ,fg)))
  `(avy-lead-face-1 ((,class :background ,bg-orange :foreground ,fg)))
  `(avy-lead-face-2 ((,class :background ,bg-pink :foreground ,fg)))

;;;; consult
  `(consult-file ((,class :foreground ,fg-constant)))

;;;;; corfu
  `(corfu-default ((,class :background ,bg-max)))
  `(corfu-border ((,class :inherit 'vertical-border)))
  `(corfu-current ((,class :inherit 'highlight)))
  `(corfu-bar ((,class :background ,bg-min)))
  `(corfu-annotations ((,class :inherit 'completions-annotations)))
  `(corfu-deprecated ((,class :inherit 'shadow :strike-through t)))
  `(corfu-echo ((,class :inherit 'completions-annotations)))

;;;;; diff-hl
  `(diff-hl-change ((,class :foreground ,bg-blue :background ,bg-blue)))
  `(diff-hl-delete ((,class :foreground ,bg-red :background ,bg-red)))
  `(diff-hl-insert ((,class :foreground ,bg-green :background ,bg-green)))

;;;;; elfeed
  `(elfeed-log-debug-level-face ((,class :foreground ,fg-pink)))
  `(elfeed-log-error-level-face ((,class :foreground ,fg-error)))
  `(elfeed-log-info-level-face ((,class :foreground ,fg-constant)))
  `(elfeed-log-warn-level-face ((,class :foreground ,fg-warning)))
  `(elfeed-search-date-face ((,class :foreground ,fg-function)))
  `(elfeed-search-feed-face ((,class :foreground ,fg-cyan)))
  `(elfeed-search-tag-face ((,class :foreground ,fg-green)))
  `(elfeed-search-title-face ((,class :foreground ,fg)))
  `(elfeed-search-unread-count-face ((,class :foreground ,fg)))
  `(elfeed-search-unread-title-face ((,class :inherit 'bold)))

;;;;; elpaca
  `(elpaca-blocked ((,class :foreground ,fg-warning :weight bold)))
  `(elpaca-busy ((,class :foreground ,fg-orange :weight bold)))
  `(elpaca-failed ((,class :foreground ,fg-error :weight bold)))
  `(elpaca-finished ((,class :foreground ,fg-success :weight bold)))

;;;;; embark (defaults are fine)

;;;;; helpful
  `(helpful-heading ((,class :foreground ,fg :weight bold :height 1.2)))

;;;;; highlight numbers
  `(highlight-numbers-number ((,class :foreground ,fg-string :bold nil)))

;;;;; hl-todo
  `(hl-todo ((,class :foreground ,fg-coral :weight bold)))

;;;;; marginalia (defaults are fine, just fix wrong italic)
  `(marginalia-file-priv-read ((,class :inherit 'font-lock-type-face :italic nil)))

;;;;; Markdown
  `(markdown-bold-face ((,class :inherit 'bold)))
  `(markdown-italic-face ((,class :inherit 'italic)))
  `(markdown-strike-through-face ((,class :strike-through t)))
  `(markdown-highlight-face ((,class :inherit 'highlight)))
  `(markdown-markup-face ((,class :inherit 'shadow)))
  `(markdown-hr-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-list-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-reference-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-comment-face ((,class :inherit 'font-lock-comment-face)))
  `(markdown-url-face ((,class :inherit 'font-lock-string-face)))
  `(markdown-link-face ((,class :inherit 'link)))
  `(markdown-plain-url-face ((,class :inherit 'markdown-link-face)))
  `(markdown-link-title-face ((,class :inherit 'font-lock-comment-face :italic nil)))
  `(markdown-missing-link-face ((,class :foreground ,fg-error)))
  `(markdown-blockquote-face ((,class :inherit 'font-lock-doc-face)))
  `(markdown-code-face ((,class :inherit 'fixed-pitch :background ,bg-max :extend t)))
  `(markdown-inline-code-face ((,class :inherit 'fixed-pitch)))
  `(markdown-pre-face ((,class :inherit 'fixed-pitch)))
  `(markdown-html-tag-delimiter-face ((,class :inherit 'fixed-pitch :foreground ,fg-green)))
  `(markdown-html-tag-name-face ((,class :inherit 'fixed-pitch :foreground ,fg-green)))
  `(markdown-html-attr-name-face ((,class :inherit 'fixed-pitch :foreground ,fg-constant)))
  `(markdown-html-attr-value-face ((,class :inherit 'fixed-pitch :foreground ,fg-string)))
  `(markdown-html-entity-face ((,class :inherit 'fixed-pitch :slant italic)))
  `(markdown-metadata-key-face ((,class :inherit 'font-lock-constant-face)))
  `(markdown-metadata-value-face ((,class :inherit 'font-lock-string-face)))
  `(markdown-table-face ((,class :inherit 'fixed-pitch)))
  `(markdown-language-info-face ((,class :inherit 'fixed-pitch :foreground ,fg-string)))
  `(markdown-language-keyword-face ((,class :inherit 'fixed-pitch)))
  `(markdown-header-delimiter-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-header-rule-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-math-face ((,class :inherit 'font-lock-string-face)))
  `(markdown-line-break-face ((,class :inherit 'font-lock-constant-face :underline t)))
  `(markdown-gfm-checkbox-face ((,class :inherit 'font-lock-builtin-face)))
  `(markdown-footnote-marker-face ((,class :inherit 'markdown-markup-face)))
  `(markdown-footnote-text-face ((,class :inherit 'font-lock-comment-face)))

;;;;; multiple-cursor
  `(mc/cursor-bar-face ((,class :background ,fg-coral :height 1)))
  `(mc/cursor-face ((,class :inverse-video t)))
  `(mc/region-face ((,class :inherit 'region)))

;;;;; orderless
  `(orderless-match-face-0 ((,class :foreground ,fg-constant :weight bold)))
  `(orderless-match-face-1 ((,class :foreground ,fg-pink-bright :weight bold)))
  `(orderless-match-face-2 ((,class :foreground ,fg-success :weight bold)))
  `(orderless-match-face-3 ((,class :foreground ,fg-warning :weight bold)))

;;;;; rainbow-delimiters
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
  `(rainbow-delimiters-base-error-face ((,class :foreground ,fg-error :background unspecified)))
  `(rainbow-delimiters-unmatched-face ((,class :inherit 'rainbow-delimiters-base-error-face)))
  `(rainbow-delimiters-mismatched-face ((,class :inherit 'rainbow-delimiters-base-error-face)))

;;;;; Web Mode
  `(web-mode-bold-face ((,class :inherit 'bold)))
  `(web-mode-italic-face ((,class :inherit 'italic)))
  `(web-mode-underline-face ((,class :inherit 'underline)))
  `(web-mode-comment-face ((,class :inherit 'font-lock-comment-face :italic nil)))
  `(web-mode-comment-keyword-face ((,class :weight bold :box nil)))
  `(web-mode-doctype-face ((,class :inherit 'web-mode-comment-face)))
  `(web-mode-string-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-keyword-face ((,class :inherit 'font-lock-keyword-face)))
  `(web-mode-constant-face ((,class :inherit 'font-lock-constant-face)))
  `(web-mode-type-face ((,class :inherit 'font-lock-type-face)))
  `(web-mode-variable-name-face ((,class :inherit 'font-lock-variable-name-face)))
  `(web-mode-builtin-face ((,class :inherit 'font-lock-builtin-face)))
  `(web-mode-function-name-face ((,class :inherit 'font-lock-function-name-face)))
  `(web-mode-function-call-face ((,class :inherit 'font-lock-function-name-face)))
  `(web-mode-filter-face ((,class :inherit 'font-lock-function-name-face)))
  `(web-mode-preprocessor-face ((,class :inherit 'font-lock-preprocessor-face)))
  `(web-mode-param-name-face ((,class :foreground ,fg)))
  `(web-mode-symbol-face ((,class :inherit 'font-lock-variable-name-face)))
  `(web-mode-warning-face ((,class :inherit 'warning)))
  `(web-mode-error-face ((,class :inherit 'error)))
  `(web-mode-html-tag-face ((,class :foreground ,fg-green)))
  `(web-mode-html-tag-custom-face ((,class :inherit 'web-mode-html-tag-face)))
  `(web-mode-html-tag-namespaced-face ((,class :inherit 'web-mode-html-tag-face)))
  `(web-mode-html-tag-unclosed-face ((,class :inherit 'web-mode-html-tag-face :underline t)))
  `(web-mode-html-tag-bracket-face ((,class :foreground ,fg-green)))
  `(web-mode-html-attr-name-face ((,class :inherit 'font-lock-constant-face)))
  `(web-mode-html-attr-value-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-html-entity-face ((,class :foreground ,fg :slant italic)))
  `(web-mode-html-attr-custom-face ((,class :inherit 'web-mode-html-attr-name-face)))
  `(web-mode-html-attr-engine-face ((,class :inherit 'web-mode-html-attr-name-face)))
  `(web-mode-html-attr-equal-face ((,class :inherit 'web-mode-html-attr-name-face)))
  `(web-mode-annotation-face ((,class :inherit 'web-mode-comment-face)))
  `(web-mode-annotation-html-face ((,class :inherit 'web-mode-annotation-face :slant italic)))
  `(web-mode-annotation-tag-face ((,class :inherit 'web-mode-annotation-face :underline t)))
  `(web-mode-annotation-type-face ((,class :inherit 'web-mode-annotation-face :weight bold)))
  `(web-mode-annotation-value-face ((,class :inherit 'web-mode-annotation-html-face)))
  `(web-mode-block-face ((,class :background ,bg-max)))
  `(web-mode-block-comment-face ((,class :inherit 'web-mode-comment-face)))
  `(web-mode-block-control-face ((,class :inherit 'font-lock-preprocessor-face)))
  `(web-mode-block-delimiter-face ((,class :inherit 'font-lock-preprocessor-face)))
  `(web-mode-block-string-face ((,class :inherit 'web-mode-string-face)))
  `(web-mode-block-attr-name-face ((,class :inherit 'web-mode-html-attr-name-face)))
  `(web-mode-block-attr-value-face ((,class :inherit 'web-mode-html-attr-value-face)))
  `(web-mode-part-face ((,class :inherit 'default)))
  `(web-mode-part-comment-face ((,class :inherit 'font-lock-comment-face)))
  `(web-mode-part-string-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-style-face ((,class :inherit 'web-mode-part-face)))
  `(web-mode-script-face ((,class :inherit 'web-mode-part-face)))
  `(web-mode-javascript-comment-face ((,class :inherit 'font-lock-comment-face)))
  `(web-mode-javascript-string-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-css-property-name-face ((,class :inherit 'font-lock-keyword-face)))
  `(web-mode-css-selector-face ((,class :inherit 'font-lock-function-name-face)))
  `(web-mode-css-selector-class-face ((,class :inherit 'web-mode-css-selector-face)))
  `(web-mode-css-selector-tag-face ((,class :inherit 'web-mode-css-selector-face)))
  `(web-mode-css-comment-face ((,class :inherit 'font-lock-comment-face)))
  `(web-mode-css-string-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-css-variable-face ((,class :inherit 'font-lock-variable-name-face)))
  `(web-mode-css-color-face ((,class :inherit 'font-lock-builtin-face)))
  `(web-mode-css-function-face ((,class :inherit 'font-lock-builtin-face)))
  `(web-mode-css-priority-face ((,class :inherit 'font-lock-builtin-face)))
  `(web-mode-css-pseudo-class-face ((,class :inherit 'font-lock-builtin-face)))
  `(web-mode-css-at-rule-face ((,class :inherit 'font-lock-constant-face)))
  `(web-mode-json-context-face ((,class :foreground ,fg-pink)))
  `(web-mode-json-key-face ((,class :foreground ,fg-pink-bright)))
  `(web-mode-json-comment-face ((,class :inherit 'font-lock-comment-face)))
  `(web-mode-json-string-face ((,class :inherit 'font-lock-string-face)))
  `(web-mode-sql-keyword-face ((,class :weight bold :slant italic)))
  `(web-mode-whitespace-face ((,class :inherit 'trailing-whitespace)))
  `(web-mode-folded-face ((,class :underline t)))
  `(web-mode-inlay-face ((,class :background ,bg-max)))
  `(web-mode-current-column-highlight-face ((,class :background ,bg-min)))
  `(web-mode-current-element-highlight-face ((,class :foreground ,fg :background ,bg-max)))
  `(web-mode-interpolate-color1-face ((,class :inherit 'web-mode-string-face)))
  `(web-mode-interpolate-color2-face ((,class :inherit 'web-mode-string-face)))
  `(web-mode-interpolate-color3-face ((,class :inherit 'web-mode-string-face)))
  `(web-mode-interpolate-color4-face ((,class :inherit 'web-mode-string-face)))

  ;; Documentation on these faces are not clear. Keeping the defaults.
  ;; web-mode-jsx-depth-1-face
  ;; web-mode-jsx-depth-2-face
  ;; web-mode-jsx-depth-3-face
  ;; web-mode-jsx-depth-4-face
  ;; web-mode-jsx-depth-5-face

;;;;; which func
  `(which-func ((,class :foreground ,fg-function)))

;;;;; which key (defaults are fine)

;;;; WIP
  ;; ORG
  ;; org-default - inherits default
  ;; org-link - inherits link
  ;; org-level-1 - inherits outline
  ;; org-level-2 - inherits outline
  ;; org-level-3 - inherits outline
  ;; org-level-4 - inherits outline
  ;; org-level-5 - inherits outline
  ;; org-level-6 - inherits outline
  ;; org-level-7 - inherits outline
  ;; org-level-8 - inherits outline

  `(org-block ((,class :inherit 'fixed-pitch :foreground ,fg :background ,bg-max :extend t)))
  `(org-quote ((,class :foreground ,fg-comment :background ,bg-max :extend t)))
  `(org-verse ((,class :inherit 'org-quote :extend t)))
  `(org-block-begin-line ((,class :inherit 'fixed-pitch :foreground ,fg-comment :background ,bg-hl)))
  `(org-block-end-line ((,class :inherit 'fixed-pitch :foreground ,fg-comment :background ,bg-hl)))
  `(org-ellipsis ((,class :foreground ,fg-comment :underline nil)))

  `(org-hide ((,class :foreground ,bg)))
  `(org-date ((,class :foreground ,fg-cyan :underline t)))
  `(org-done ((,class :foreground ,fg-success :weight bold)))
  `(org-todo ((,class :foreground ,fg-error :weight bold)))
  `(org-latex-and-related ((,class :foreground ,fg-warning)))

  `(org-code ((,class :inherit 'fixed-pitch :foreground ,fg-comment))) ; shadow
  `(org-meta-line ((,class :inherit 'fixed-pitch :foreground ,fg-comment))) ; comment-face
  `(org-document-info-keyword ((,class :inherit 'fixed-pitch :foreground ,fg-comment))) ; shadow
  `(org-table ((,class :inherit 'fixed-pitch :foreground ,fg-string)))
  `(org-table-header ((t (:inherit 'org-table :foreground ,bg :background ,fg))))

  `(org-verbatim ((,class :inherit 'fixed-pitch :foreground ,fg-comment))) ; shadow
  `(org-formula ((,class :inherit 'fixed-pitch :foreground ,fg-variable)))
  `(org-special-keyword ((,class :inherit 'fixed-pitch :foreground ,fg-keyword)))
  `(org-checkbox ((,class :inherit 'fixed-pitch :bold t))) ; bold
  ;; org-checkbox-statistics-done - inherits org-done
  ;; org-checkbox-statistics-todo - inherits org-todo

  ;; org-document-info
  ;; org-document-title

  ;; org-warning - inherits font-lock-warning-face (deadlines and todo)
  ;; org-archived - inherits shadow
  ;; org-cite - inherits link
  ;; org-cite-key - inherits link

  ;; org-clock-overlay
  ;; org-column
  ;; org-column-title
  ;; org-date-selected
  ;; org-dispatcher-highlight

  ;; org-drawer
  ;; org-footnote
  ;; org-headline-done
  ;; org-headline-todo
  ;; org-imminent-deadline
  ;; org-inline-src-block
  ;; org-list-dt
  ;; org-macro
  ;; org-mode-line-clock
  ;; org-mode-line-clock-overrun
  ;; org-priority
  ;; org-property-value
  ;; org-scheduled
  ;; org-scheduled-previously
  ;; org-scheduled-today
  ;; org-sexp-date
  ;; org-tag
  ;; org-tag-group
  ;; org-target
  ;; org-time-grid
  ;; org-upcoming-deadline
  ;; org-upcoming-distant-deadline

  ;; org-roam-dim
  ;; org-roam-header-line
  ;; org-roam-olp
  ;; org-roam-preview-heading
  ;; org-roam-preview-heading-highlight
  ;; org-roam-preview-heading-selection
  ;; org-roam-preview-region
  ;; org-roam-shielded
  ;; org-roam-title

  ;; org-modern-block-name
  ;; org-modern-date-active
  ;; org-modern-date-inactive
  ;; org-modern-done
  ;; org-modern-horizontal-rule
  ;; org-modern-internal-target
  ;; org-modern-label
  ;; org-modern-priority
  ;; org-modern-radio-target
  ;; org-modern-statistics
  ;; org-modern-symbol
  ;; org-modern-tag
  ;; org-modern-time-active
  ;; org-modern-time-inactive
  ;; org-modern-todo

  ;; org-agenda-calendar-event
  ;; org-agenda-calendar-sexp
  ;; org-agenda-clocking
  ;; org-agenda-column-dateline
  ;; org-agenda-current-time
  ;; org-agenda-date
  ;; org-agenda-date-today
  ;; org-agenda-date-weekend
  ;; org-agenda-date-weekend-today
  ;; org-agenda-diary
  ;; org-agenda-dimmed-todo-face
  ;; org-agenda-done
  ;; org-agenda-filter-category
  ;; org-agenda-filter-effort
  ;; org-agenda-filter-regexp
  ;; org-agenda-filter-tags
  ;; org-agenda-restriction-lock
  ;; org-agenda-structure
  ;; org-agenda-structure-filter
  ;; org-agenda-structure-secondary


  ;; This theme is still incomplete
  ;; TODO:
  ;; - Info-mode
  ;; - eww
  ;; - grep
  ;; - c-annotation-face
  ;; - newsticker
  ;; - org (org-ref)
  ;; - magit
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
                             ,fg-purple
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
