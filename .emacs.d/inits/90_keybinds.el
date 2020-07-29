;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'smartchr)
;; (require 'smartrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 通常操作
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-j" 'newline) ; 改行
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)
(global-set-key (kbd "C-t") 'other-window-or-split)
;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;; フルスクリーン
(global-set-key (kbd "C-x F") 'toggle-frame-maximized)
(global-set-key (kbd "C-x ?") 'help-command)

;;; 少しずつスクロール
(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;; C-t でウインドウ切り替え
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-;") 'anything-custom-filelist) ;;自分の定義
(global-set-key (kbd "C-:") 'anything);;anything
(global-set-key (kbd "C-x C-z") 'anything-resume)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map [(control ?:)] 'anything-migemo)
(global-set-key (kbd "C-c g") 'anything-git-grep-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C-g") 'git-grep)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "C-x v") 'magit-status)

(global-set-key (kbd "<f5>") 'slime-js-reload)

(global-set-key (kbd "C-M-g") 'ack)
(global-set-key (kbd "C-M-f") 'find-dired)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;;; abbrev
(global-set-key (kbd "M-SPC") 'expand-abbrev)

;; point-undo
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)

;; redo
(global-set-key "\M-/" 'redo)

;;; quickrun
(global-set-key "\C-cc" 'quickrun-with-arg)

;;; auto-complete
(define-key global-map (kbd "<C-tab>") 'ac-fuzzy-complete)

;;; org-mode用
(define-key global-map (kbd "C-c l") 'org-store-link)
;;; lisp mode用
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)
;; (define-key emacs-lisp-mode-map (kbd "L") (smartchr '("L" " ; => ")))

;;; for dired
(define-key dired-mode-map "\C-m" 'dired-my-advertised-find-file)
(define-key dired-mode-map "^" 'dired-my-up-directory)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; isearch-mode
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; replace
(global-set-key (kbd "C-c r") 'anzu-query-replace)
(global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;; multiple-cursors
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)

;;; highlight-symbol
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(global-set-key (kbd "C-x C-d") 'open-default-directory-on-iterm)

;;; goto-last-change
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;;; auto-programming
(global-set-key (kbd "M-l") 'auto-programming)

;; google-translate
(global-set-key (kbd "C-M-t") 'google-translate-enja-or-jaen)

;; vs code
(define-key global-map (kbd "C-c C-v") 'open-by-vscode)
