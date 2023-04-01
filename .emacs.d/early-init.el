;;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(setq inhibit-startup-message t)
(setq gc-cons-threshold (* 16 1024 1024))

(setq default-frame-alist
      (append
       '((top . 22)
         (left . 55)
         (width . 400)
         (height . 130)
         (vertical-scroll-bar . nil)
         ) default-frame-alist))
(setq initial-frame-alist default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(setq frame-title-format (format (if (buffer-file-name) "%%f" "%%b")()))

(column-number-mode t)
(global-hl-line-mode t)
(line-number-mode t)

(global-display-line-numbers-mode t)

(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

(setq byte-compile-warnings '(cl-functions))
;;; early-init.el ends here
