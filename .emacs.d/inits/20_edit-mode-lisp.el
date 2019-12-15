;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'lisp-mode "lisp-mode" nil t)
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

(autoload 'paredit "paredit" nil t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; lispxmp
(autoload 'lispxmp "lispxmp" nil t)

;; edit-list
(autoload 'edit-list "edit-list" nil t)

;;; 自動byte compile
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|/myprojects/\\|/inits/\\|setup.el")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; eldoc
(autoload 'eldoc "eldoc" nil t)

(autoload 'eldoc-extension "eldoc-extension" nil t)
(setq eldoc-idle-delay 0.2)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-minor-mode-string "")



;;; 強制キーバインドマイナーモード
(define-minor-mode overriding-minor-mode
  "強制的にC-tを割り当てる"             ;説明文字列
  t                                     ;デフォルトで有効にする
  ""                                    ;モードラインに表示しない
  `((,(kbd "C-t") . other-window-or-split)))
