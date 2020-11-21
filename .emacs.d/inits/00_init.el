;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キー設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; システムが Mac のとき、 Ctrl を Mac ではなく Emacs に渡す
(when (eq system-type 'darwin)
  (setq mac-pass-control-to-system t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 本体設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 言語環境
(set-language-environment "Japanese")

;; 文字コード
(prefer-coding-system 'utf-8)

;; ファイル名設定
;; MacOS
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq system-type 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; ファイル名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; yes-no の選択を y-n にする
(fset 'yes-or-no-p 'y-or-n-p)

;; 行先頭で C-k を一回押すことで行全体を消去
(setq kill-whole-line t)

;; TAB ではなく半角スペース
(setq-default tab-width 2 indent-tabs-mode nil)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; ロックファイルを作らない
(setq create-lockfiles nil)

;; Symbolic linkを避ける
(setq vc-follow-symlinks t)

;; 終了時は確認する
(setq confirm-kill-emacs 'y-or-n-p)

(provide '00_init)
;;; 00_init.el ends here
