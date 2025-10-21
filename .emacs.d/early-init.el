;;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(setq inhibit-startup-message t)
(setq gc-cons-threshold (* 128 1024 1024))

(setq default-frame-alist
      (append
       '((width . 400)
         (height . 100)
         (vertical-scroll-bar . nil)
         ) default-frame-alist))
(setq initial-frame-alist default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(setq frame-title-format (format (if (buffer-file-name) "%%f" "%%b")()))

(global-hl-line-mode t)

(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

(setq byte-compile-warnings '(cl-functions))
;;; early-init.el ends here
