;;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(setq inhibit-startup-message t)
(setq gc-cons-threshold (* 16 1024 1024))

(setq initial-frame-alist
      (append
       '((top . 22)
         (left . 55)
         (width . 400)
         (height . 130)
         (vertical-scroll-bar . nil)
         ) initial-frame-alist))
(setq frame-title-format (format (if (buffer-file-name) "%%f" "%%b")()))

(set-face-attribute 'default nil :font "HackGen Console" :height 120)

(column-number-mode t)
(global-hl-line-mode t)
(line-number-mode t)

(setq byte-compile-warnings '(cl-functions))
;;; early-init.el ends here
