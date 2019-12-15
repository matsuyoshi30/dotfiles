;;; init.el --- init.el
;; Author: matsuyoshi30
;; Version:
;;; Commentary:
;; This program is Emacs init.el
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (locate-user-emacs-file "~/.emacs.d/elisp/"))
(add-to-list 'load-path (locate-user-emacs-file "~/.emacs.d/elpa/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-to-load-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-to-load-path 定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path)))))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path)))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-loader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 設定ファイルはinits以下に置いて、init-loaderで読み込む
(require 'init-loader)
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load (locate-user-emacs-file "inits"))
(init-loader-load "~/.emacs.d/inits")

(provide 'init)
;;; init.el ends here
