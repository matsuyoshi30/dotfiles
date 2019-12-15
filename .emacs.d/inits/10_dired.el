(ffap-bindings)

;; dired文字コードの設定
(setq dired-default-file-coding-system 'utf-8-unix)

;; Xキーでの拡張機能の追加
(require 'dired-x)
(setq dired-guess-shell-gnutar "tar")
;; 各ファイルの設定
(setq dired-guess-shell-alist-user
      '(("\\.tar\\.gz\\'" "tar ztvf")
        ("\\.tar\\'" "tar ztvf")
        ("\\.tar\\.bz2\\'" "tar Itvf")
        ("\\.zip\\'" "unzip -l")
        ("\\.\\(g\\|\\ z\\'" "zcat")
        ("\\.\\(jpg\\|JPG\\|git\\|GIF\\)\\'"
         (if (eq system-type 'window-nt)
             "fiber" "xv"))
        ("\\.ps\\'"
         (if (eq system-type 'window-nt)
             "fiber" "ghostview"))
        ))

;; スペースでマークする（FD like）
(define-key dired-mode-map " " 'dired-toggle-mark)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    (dired-previous-line 1)))

;; diredで新しいバッファを作成しない
(defun dired-my-advertised-find-file ()
  (interactive)
  (let ((kill-target (current-buffer))
        (check-file (dired-get-filename)))
    (funcall 'dired-advertised-find-file)
    (if (file-directory-p check-file)
        (kill-buffer kill-target))))

(defun dired-my-up-directory (&optional other-window)
  "Run dired on parent directory of current directory.
   Find the parent directory either in this buffer or another buffer.
   Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (if other-window
              (dired-other-window up)
            (progn
              (kill-buffer (current-buffer))
              (dired up))
            (dired-goto-file dir))))))

;; 別のディレクトリに移動したときもソート方法を保存する
(defadvice dired-advertised-find-file
    (around dired-sort activate)
  (let ((sw dired-actual-switches))
    ad-do-it
    (if (string= major-mode 'dired-mode)
        (progn
          (setq dired-actual-switches sw)
          (dired-sort-other dired-actual-switches)))
    ))

(defadvice dired-my-up-directory
    (around dired-sort activate)
  (let ((sw dired-actual-switches))
    ad-do-it
    (if (string= major-mode 'dired-mode)
        (progn
          (setq dired-actual-switches sw)
          (dired-sort-other dired-actual-switches)))
    ))

;; ディレクトリを最初に表示する
(setq insert-directory-program "gls")
(setq dired-listing-switches "-AFl --group-directories-first")

;; wdiredの設定
(require 'wdired)
