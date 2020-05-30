;; color-theme
(load-theme 'monokai t)

;; ターミナル以外はツールバー、スクロールバーを非表示
(when window-system
  (tool-bar-mode 0)    ;; tool-bar
  (scroll-bar-mode 0)) ;; scroll-bar

;; CocoaEmacs 以外はメニューバーを非表示
(unless (eq window-system 'ns)
  (menu-bar-mode 0))

;; C-t でウインドウ切り替え
(define-key global-map (kbd "C-t") 'other-window)

;; 少しずつスクロール(C-s-n, C-s-p)
(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;; 行列数表示
(column-number-mode t)
(line-number-mode t)

;; スタートアップ画面表示
(setq inhibit-startup-screen 0)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; 選択部分のハイライト
(transient-mark-mode t)

;; キーストロークをエコーエリアに早く表示
(setq echo-keystrokes 0.1)

;; ファイルサイズ表示
(size-indication-mode t)

;; 時計表示
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)

;; バッテリー情報表示
(display-battery-mode t)

;; window のフレーム設定
(if window-system (progn
  (cond
   ((eq window-system 'ns) ; for Macintosh
    (setq initial-frame-alist
          (append
           '((top . 22)
             (left . 55)
             (width . 120)
             (height . 65)
             (cursor-height . 0)
             (vertical-scroll-bar . nil)
             ) initial-frame-alist))))))
(if window-system (progn
  (setq default-frame-alist initial-frame-alist)))

;; markdown-modeの時のみ、行末の空白を無視
(defvar delete-trailing-whitespace-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespace-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'my-face-spc-at-eol) nil)
             (set (make-local-variable 'delete-trailing-whitespace-before-save) nil)))

;; 全角スペースを目立させる
(require 'whitespace)
(setq whitespace-style '(face
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please comment the line below.
;;      (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
        ))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(global-whitespace-mode 1)

;; フォント設定
(when window-system
    (create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
    (set-fontset-font "fontset-ricty"
                      'unicode
                      (font-spec :family "Ricty")
                      nil
                      'append)
    (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
    (defvar my:font-size 14))

;; window の透過
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 90)))

;; smart-mode-line
(setq sml/active-background-color "gray60")
(setq sml/modified-char "*")
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(setq sml/shorten-directory -1)
(sml/setup)

(require 'diminish)
;; Hide jiggle-mode lighter from mode line
(diminish 'jiggle-mode)
;; Replace abbrev-mode lighter with "Abv"
(diminish 'abbrev-mode "Abv")

(column-number-mode t)
(line-number-mode t)

;; display-buffer時に変に分割されるのを防ぐ
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; elscreen
;; http://emacs.rubikitch.com/elscreen/
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))
