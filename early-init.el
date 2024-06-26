;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold 67108864
         gc-cons-percentage 0.1)
   (garbage-collect)
   (message
    "Emacs loaded in %s with %d garbage collections."
    (format
     "%.2f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

(setq package-quickstart nil
      package-enable-at-startup nil
      load-prefer-newer t)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setopt native-comp-async-report-warnings-errors 'silent
          native-comp-jit-compilation t)
  (let ((cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
    (add-to-list 'native-comp-eln-load-path cache)
    (startup-redirect-eln-cache cache)))

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(setopt inhibit-startup-message t
        initial-scratch-message ""
        auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-charset-priority 'unicode)
