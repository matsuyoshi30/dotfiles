;; default mode is text mode
(setq major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-mode設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . nxml-mode))
;; </を入力すると自動的にタグを閉じる
(setq nxml-slash-auto-complete-flag t)
;; M-TABでタグ補完
(setq nxml-bind-meta-tab-to-complete-flag t)
;; nxml-modeでauto-complete-modeを利用
(add-to-list 'ac-modes 'nxml-mode)
;; 子要素のインデント幅を設定
(setq nxml-child-indent 2)
;; 属性値のインデント幅を設定
(setq nxml-attribute-indent 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cssモード設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'css-mode "css-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lessモード設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'less-css-mode "less-css-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pythonモード設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;メジャーモードの自動起動設定
(autoload 'python-mode "python-mode" nil t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

;;インデントをスペースに
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kotlin mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'kotlin-mode "kotlin-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js[x]?$". web-mode))
(defvar web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?")))
(add-hook 'web-mode-hook
          '(lambda ()
             (add-to-list 'web-mode-comment-formats '("jsx" . "//"))))
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             ))
(local-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.sh$" . sh-mode))

;;; crontab mode
(setq crontab-mode-map nil)
(autoload 'crontab-mode "crontab-mode" nil t)
(add-to-list 'auto-mode-alist '("crontab$" . crontab-mode))
