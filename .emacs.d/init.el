;;; init.el --- init.el
;; Author: matsuyoshi30
;; Version:
;;; Commentary:
;; This program is Emacs init.el
;;; Code:

;;; Measurement

(defconst my-before-load-init-time (current-time))

(defun my-load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my-before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my-before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))

(add-hook 'after-init-hook #'my-load-init-time t)

(defvar my-tick-previous-time my-before-load-init-time)
(defun my-tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my-loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime))))

(defun my-emacs-init-time ()
  "Emacs booting time in msec."
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my-emacs-init-time) ;; M-x emacs-init-time

;;; General

(add-to-list 'load-path (locate-user-emacs-file "~/.emacs.d/elisp/"))
(add-to-list 'load-path (locate-user-emacs-file "~/.emacs.d/elpa/"))

;; add-to-load-path 定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path)))))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path)))))
(add-to-load-path "elisp" "elpa")

;; package
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Ctrl to Emacs
(when (eq system-type 'darwin)
  (setq mac-pass-control-to-system t))

(set-language-environment "Japanese")

(prefer-coding-system 'utf-8)

;; filename
(when (eq system-type 'darwin) ;; macOS
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
(when (eq system-type 'w32) ;; windows
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

(setq completion-ignore-case t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-whole-line t)

(setq-default tab-width 2 indent-tabs-mode nil)

(global-auto-revert-mode 1) ;; read buffer automatically

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq vc-follow-symlinks t) ;; avoid symbolic link

(setq confirm-kill-emacs 'y-or-n-p)

(setq ac-comphist-file "~/.emacs.d/cache/auto-complete/ac-comphist.dat")
(setq eshell-directory-name "~/.emacs.d/cache/eshell/")
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.saves-")

;;; Path

(autoload 'exec-path-from-shell "exec-path-from-shell" nil t)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))

;;; Utility

;; chomp 定義
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

;; git
(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
         (t
          "")))

(defun git-cdup ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-cdup")))
        (t
         "")))

;; iterm
(defun execute-on-iterm (command)
  (interactive "MCommand: ")
  (do-applescript
   (format "tell application \"iTerm\"
              active
              tell current session of current terminal
                write text \"%s\"
              end tell
            end tell"
           command)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; vs code
(defun open-by-vscode ()
  (interactive)
  (shell-command
   (format "code -r -g %s:%d:%d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column))))

;;; Display

(load-theme 'monokai t)

(when window-system
  (tool-bar-mode 0)    ;; tool-bar
  (scroll-bar-mode 0)) ;; scroll-bar

(unless (eq window-system 'ns)
  (menu-bar-mode 0))

(column-number-mode t)
(line-number-mode t)

(setq inhibit-startup-screen 0)

(show-paren-mode t)

(transient-mark-mode t)

(size-indication-mode t)

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)

(display-battery-mode t)

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

(defvar delete-trailing-whitespace-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespace-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'my-face-spc-at-eol) nil)
             (set (make-local-variable 'delete-trailing-whitespace-before-save) nil)))

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

(when window-system
    (create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
    (set-fontset-font "fontset-ricty"
                      'unicode
                      (font-spec :family "Ricty")
                      nil
                      'append)
    (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
    (defvar my:font-size 14))
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 100)))

(setq sml/active-background-color "gray60")
(setq sml/modified-char "*")
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(setq sml/shorten-directory -1)
(sml/setup)

(require 'diminish)
(diminish 'jiggle-mode) ;; Hide jiggle-mode lighter from mode line
(diminish 'abbrev-mode "Abv")

(column-number-mode t)
(line-number-mode t)

;; avoid wired split when display-buffer
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; elscreen
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(setq elscreen-display-tab nil)
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
        ("^irchat-" . "IRChat")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")))

;;; Dired

(ffap-bindings)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".DS_Store")))

(setq dired-default-file-coding-system 'utf-8-unix)

(require 'dired-x)
(setq dired-guess-shell-gnutar "tar")
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

;; mark by <space>
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

;; do not make new buffer for dired
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

;; save sort result when go to another directory
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

;; output directory first
(setq insert-directory-program "gls")
(setq dired-listing-switches "-AFl --group-directories-first")

(require 'wdired)

;;; auto-complete

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;; company

(require 'company)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)

(set-face-attribute 'company-tooltip nil
                    :foreground "black"
                    :background "lightgray")
(set-face-attribute 'company-preview-common nil
                    :foreground "dark gray"
                    :background "black"
                    :underline t)
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "white"
                    :background "steelblue")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black"
                    :underline t)
(set-face-attribute 'company-tooltip-common-selection nil
                    :background "steelblue"
                    :foreground "white"
                    :underline t)
(set-face-attribute 'company-tooltip-annotation nil
                    :foreground "red")

;;; flycheck

(require 'flycheck)
(setq flycheck-highlighting-mode 'lines)

;;; Edit mode

;; default mode is text mode
(setq major-mode 'text-mode)

;; Go mode
(autoload 'go-mode "go-mode" nil t)
(add-hook 'go-mode-hook
          '(lambda()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)))
(add-hook 'before-save-hook 'gofmt-before-save)

;; web mode
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jsx?$". web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$". web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$". web-mode))
(add-to-list 'auto-mode-alist '("\\.html$". web-mode))
(defun setup-tide-mode ()
	(interactive)
	(tide-setup)
	(flycheck-mode +1)
	(setq flycheck-check-syntax-automatically '(save mode-enabled))
	(eldoc-mode +1)
	(tide-hl-identifier-mode +1)
	(company-mode +1))
(add-hook 'web-mode-hook
          '(lambda ()
             (when (string-equal "js" (file-name-extension buffer-file-name))
               (setup-tide-mode))
             (when (string-equal "ts" (file-name-extension buffer-file-name))
               (setup-tide-mode))
             (when (string-equal "jsx" (file-name-extension buffer-file-name))
               (setup-tide-mode))
             (when (string-equal "tsx" (file-name-extension buffer-file-name))
               (setup-tide-mode))))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq web-mode-script-padding 0)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             ))
(local-set-key (kbd "RET") 'newline-and-indent)

;; json mode
(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))

;; css mode
(autoload 'css-mode "css-mode" nil t)

;; lisp mode
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


;; python mode
(autoload 'python-mode "python-mode" nil t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
;; indent to space
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil))))

;; c mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)))

;; kotlin mode
(autoload 'kotlin-mode "kotlin-mode" nil t)

;; rust mode
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)))

;; dockerfile mode
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; docker-compose mode
(autoload 'docker-compose-mode "docker-compose-mode" nil t)
(add-to-list 'auto-mode-alist '("docker-compose\\'" . docker-compose-mode))

;; yaml mode
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; terraform mode
(autoload 'terraform-mode "terraform-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tf" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; sh mode
(add-to-list 'auto-mode-alist '("\\.sh$" . sh-mode))

;; crontab mode
(setq crontab-mode-map nil)
(autoload 'crontab-mode "crontab-mode" nil t)
(add-to-list 'auto-mode-alist '("crontab$" . crontab-mode))

;;; anything

(when (require 'anything nil t)
  (setq
   ;; 候補を表示する時間
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数
   anything-candidate-number-limit 100
   ;; 候補が多いとき、体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nit t))
  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;;; flymake
(require 'flymake)

(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; output error on mini-buffer
;; ref: http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; output error message by flymake on popup-tip
;; ref: http://d.hatena.ne.jp/khiker/20100203/popup_flymake
(require 'popup)
(defun popup-flymake-display-error ()
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info
                                                           line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count)
                                                  line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count)
                                                      line-err-info-list)))
               (text (flymake-ler-text (nth (1- count)
                                                 line-err-info-list)))
               (line (flymake-ler-line (nth (1- count)
                                                 line-err-info-list))))
          (popup-tip (format "[%s] %s" line text))))
      (setq count (1- count)))))

(add-hook
 'flymake-mode-hook
 '(lambda ()
    (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
    (local-set-key (kbd "C-c e") 'popup-flymake-display-error)))

;;; Snippet

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"))
(define-key yas-minor-mode-map (kbd "C-x C-i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x C-i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x C-i v") 'yas-visit-snippet-file)
(yas-global-mode 1)

;;; Org

(require 'org)
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-remember
(setq org-default-notes-file (concat org-directory "remember.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")))
;; TODO status
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; log when done
(setq org-log-done 'time)

;;; Region

;; ref: http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ;; echo area
    (count-lines-region (region-beginning) (region-end))
    ""
    ))

;;; Key bindings

;; General
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-j" 'newline) ; 改行
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)
(global-set-key (kbd "C-t") 'other-window-or-split)

;; multi line move
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;; full screen
(global-set-key (kbd "C-x F") 'toggle-frame-maximized)
(global-set-key (kbd "C-x ?") 'help-command)

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'other-window)

;; for anything
(global-set-key (kbd "C-;") 'anything-custom-filelist) ;;自分の定義
(global-set-key (kbd "C-:") 'anything);;anything
(global-set-key (kbd "C-x C-z") 'anything-resume)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map [(control ?:)] 'anything-migemo)
(global-set-key (kbd "C-c g") 'anything-git-grep-all)

;; others

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

(provide 'init)
;;; init.el ends here
