;;; init.el --- init.el -*- lexical-binding: t -*-

;; Author: matsuyoshi30

;;; Commentary:

;; This program is Emacs init.el

;;; Code:

;;; General

;;; Bootstrap elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                   ,@(when-let* ((depth (plist-get order :depth)))
                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
                                                   ,(plist-get order :repo) ,repo))))
                   ((zerop (call-process "git" nil buffer t "checkout"
                                         (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package を elpaca 経由で有効化
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; diminish for :diminish support
(elpaca diminish (require 'diminish))

;; elpaca-use-package-mode が有効になるまで同期的に待つ
;; これがないと use-package :ensure t が評価される時点で
;; elpaca-use-package-mode が未有効のため、ビルトインパッケージがロードされる
(elpaca-wait)

;; (require 'profiler)
;; (profiler-start 'cpu)

(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defconst my/before-load-init-time (current-time))

;;;###autoload
(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defvar my/tick-previous-time my/before-load-init-time)

;;;###autoload
(defun my/tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my/loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my/tick-previous-time)))
               msg)
      (setq my/tick-previous-time ctime))))

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my/emacs-init-time)

(use-package macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; Builtin settings: cus-edit, cus-start, simple merged
(use-package emacs
  :ensure nil
  :custom
  (custom-file (locate-user-emacs-file "custom.el"))
  (user-full-name "Masaya Watanabe")
  (user-mail-address "sfbgwm30@gmail.com")
  (user-login-name "matsuyoshi30")
  (create-lockfiles nil)
  (debug-on-error t)
  (init-file-debug t)
  (frame-resize-pixelwise t)
  (enable-recursive-minibuffers t)
  (history-length 1000)
  (history-delete-duplicates t)
  (scroll-preserve-screen-position t)
  (scroll-conservatively 100)
  (mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (ring-bell-function 'ignore)
  (text-quoting-style 'straight)
  (truncate-lines t)
  (scroll-bar-mode nil)
  (indent-tabs-mode nil)
  (kill-ring-max 100)
  (kill-read-only-ok t)
  (kill-whole-line t)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  :bind
  (("C-h" . delete-backward-char)
   ("C-S-j" . eval-print-last-sexp)
   ("C-j" . newline)
   ("C-c a" . align)
   ("C-c M-a" . align-regexp)
   ("C-x F" . toggle-frame-maximized)
   ("C-x ?" . help-command)
   ("C-c C-j" . rg)
   ("C-c C-q" . quickrun-with-arg)
   ("C-t" . other-window-or-split)
   ("C-c '" . google-this)
   ("C-c t" . toggle-truncate-lines)
   ("C-x C-m" . counsel-mark-ring)
   ("C-c g" . affe-grep)
   ("C-c f" . affe-find)
   ("C-]" . consult-ghq-find)
   ("M-SPC" . expand-abbrev)
   ("<f5>" . ef-themes-select)
   ("<f6>" . neotree-toggle)
   ("<f7>" . global-display-line-numbers-mode)
   ("<f8>" . display-line-numbers-mode)
   ("C-x M-g" . germanium-buffer-to-png)
   ("C-x M-q" . germanium-region-to-png)
   ("M-n" . (lambda () (interactive) (scroll-up 1)))
   ("M-p" . (lambda () (interactive) (scroll-down 1)))
   ("C-x C-j" . skk-mode)
   ("C-c w" . copy-word-at-point)
   ("C-\\" . nil))
  :bind (:map read-expression-map
         ("<tab>" . completion-at-point))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(use-package paren
  :ensure nil
  :custom (show-paren-delay 0.1)
  :init (show-paren-mode 1))

(use-package delsel
  :ensure nil
  :init (delete-selection-mode 1))

(use-package autorevert
  :ensure nil
  :custom (auto-revert-interval 5)
  :init (global-auto-revert-mode 1))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 2000)
  (recentf-exclude '("\\.elc$" "\\.o$" "~$" "\\.undo-tree/" "PATH" "^/[^/:]+:"))
  (recentf-auto-cleanup 'never)
  :init
  (recentf-mode t))

(when (eq system-type 'darwin)
  (defvar mac-pass-control-to-system t) ;; Ctrl to Emacs
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq confirm-kill-emacs 'y-or-n-p)

(setq vc-follow-symlinks t) ;; avoid symbolic link

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(defvar ac-comphist-file "~/.emacs.d/cache/auto-complete/ac-comphist.dat")
(defvar eshell-directory-name "~/.emacs.d/cache/eshell/")
(defvar auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.saves-")

(use-package dash :ensure t)

(setq gc-cons-percentage 0.2
      gc-cons-threshold (* 128 1024 1024))
(add-hook 'focus-out-hook #'garbage-collect)
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; (use-package gcmh
;;   :ensure t
;;   :diminish gcmh
;;   :custom (gcmh-verbose t)
;;   :config
;;   (gcmh-mode 1))

(use-package midnight
  :ensure nil
  :demand t
  :hook (emacs-startup . midnight-mode))

(use-package expand-region
  :ensure t
  :bind (("C-@" . er/expand-region)))

(setq auto-mode-case-fold nil)

(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq ffap-machine-p-known 'reject)

(setq idle-update-delay 1.0)

(setq redisplay-skip-fontification-on-input t)

;;; Path

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;; Display

(set-face-attribute 'default nil :family "HackGen Console" :height 160)
(set-face-attribute 'fixed-pitch nil :family "HackGen Console" :height 160)
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 160)

(use-package ef-themes
  :ensure (:tag "1.10.0") ;; before modus
  :functions my-ef-themes-default-font-face
  :custom
  (ef-themes-headings '((0 . (variable-pitch light 1.9))
                        (1 . (variable-pitch light 1.8))
                        (2 . (variable-pitch regular 1.7))
                        (3 . (variable-pitch regular 1.6))
                        (4 . (variable-pitch regular 1.5))
                        (5 . (variable-pitch 1.4))
                        (6 . (variable-pitch 1.3))
                        (7 . (variable-pitch 1.2))
                        (t . (variable-pitch 1.1))))
  (ef-themes-mixed-font t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-to-toggle '(ef-summer ef-winter))
  (ef-themes-region '(intense no-extend neutral))
  :config
  (defun my-ef-themes-default-font-face ()
     (ef-themes-with-colors
       `(default ((,c :height 140)))))
  (mapc #'disable-theme custom-enabled-themes)
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-default-font-face)
  ;; テーマ読み込み前にフレーム位置を保存し、読み込み後に復元する
  ;; ef-themes-post-load-hook は load-theme の後に実行されるため、
  ;; hook 内で frame-position を取得しても既に変わった後の値になる
  (defvar my--frame-position-before-theme nil
    "テーマ切り替え前のフレーム位置を保存する変数")
  (advice-add 'ef-themes--load-theme :before
              (lambda (&rest _)
                (setq my--frame-position-before-theme
                      (frame-position (selected-frame)))))
  (add-hook 'ef-themes-post-load-hook
            (lambda ()
              (set-frame-size (selected-frame) (frame-width) (frame-height))
              (when my--frame-position-before-theme
                (set-frame-position (selected-frame)
                                    (car my--frame-position-before-theme)
                                    (cdr my--frame-position-before-theme))))
            nil t)
  (ef-themes-select 'ef-elea-dark))

(use-package neotree
  :ensure t
  :functions neo-global--window-exists-p
  :custom
  (neo-show-hidden-files t)
  (neo-theme 'icons)
  :config
  (defun neotree-text-scale ()
    "Text scale for neotree."
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 0.2)
    (message nil))
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (call-interactively 'neotree-text-scale))))

(use-package tab-bar
  :ensure nil
  :init (tab-bar-mode 1)
  :custom
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-name-truncated-max 12))

(setq display-time-day-and-date t)
(defvar display-time-string-forms
  '(month "/" day " " dayname " "
          24-hours ":" minutes " "))
(display-time-mode t)
(display-battery-mode t)

(use-package simple
  :ensure nil
  :demand t
  :config
  (global-visual-line-mode t)
  (with-eval-after-load 'diminish
    (diminish 'visual-line-mode nil)))

(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(defvar delete-trailing-whitespace-before-save t)
(defun my-delete-trailing-whitespace ()
  "Delete trailing whitespaces before save file."
  (if delete-trailing-whitespace-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
(add-hook 'markdown-mode-hook
          #'(lambda ()
             (set (make-local-variable 'my-face-spc-at-eol) nil)
             (set (make-local-variable 'delete-trailing-whitespace-before-save) nil)))

(use-package nyan-mode
  :ensure t
  :init
  (defvar nyan-bar-length 16)
  :config
  (nyan-mode t))

(use-package smartparens
  :ensure t
  :demand t
  :diminish smartparens-mode
  :functions sp-pair
  :custom (sp-escape-quotes-after-insert nil)
  :bind (:map smartparens-mode-map
         ("C-(" . sp-backward-slurp-sexp)
         ("C-)" . sp-slurp-hybrid-sexp)
         ("M-(" . sp-backward-barf-sexp)
         ("M-)" . sp-forward-barf-sexp)
         ("M-k" . sp-splice-sexp)
         ("C-M-k" . sp-raise-sexp)
         ("C-M-u" . sp-split-sexp)
         ([remap backward-kill-sexp] . sp-backward-kill-sexp)
         ([remap backward-list] . sp-backward-symbol)
         ([remap backward-sexp] . sp-backward-sexp)
         ([remap beginning-of-defun] . sp-backward-down-sexp)
         ([remap end-of-defun] . sp-down-sexp)
         ([remap forward-list] . sp-forward-symbol)
         ([remap forward-sexp] . sp-forward-sexp)
         ([remap kill-sexp] . sp-kill-sexp)
         ([remap mark-sexp] . sp-mark-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (sp-pair "｢" "｣" :actions '(insert wrap autoskip navigate))
  (sp-pair "「" "」" :actions '(insert wrap autoskip navigate))
  (sp-pair "『" "』" :actions '(insert wrap autoskip navigate))
  ;; based on https://github.com/Fuco1/smartparens/wiki/Permissions
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(use-package rainbow-mode
  :ensure t
  :hook
  (css-mode . rainbow-mode)
  (sass-mode . rainbow-mode)
  (scss-mode . rainbow-mode)
  (web-mode . rainbow-mode))


;; all-the-icons

(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 0.9)
  (all-the-icons-default-adjust 0.0))

(use-package all-the-icons-ivy
  :ensure t
  :after all-the-icons
  :demand t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :demand t
  :diminish all-the-icons-dired-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-lsp t))

(use-package hide-mode-line
  :ensure t
  :hook (neotree-mode . hide-mode-line-mode))

(transient-mark-mode t)
(size-indication-mode t)
(setq-default tab-width 2 indent-tabs-mode nil)

(use-package moom
  :ensure t
  :defines moom-mode-map
  :config
  (setq moom-use-font-module nil)
  (moom-mode t)
  (define-key moom-mode-map (kbd "M-1") 'moom-move-frame-left)
  (define-key moom-mode-map (kbd "M-2") 'moom-move-frame-to-center)
  (define-key moom-mode-map (kbd "M-3") 'moom-move-frame-right)
  ;; Center frame on startup
  (add-hook 'window-setup-hook #'moom-move-frame-to-center))

(use-package darkroom
  :ensure t
  :bind (("<f10>" . my:darkroom-mode-in)
         ("<f12>" . my:full-darkroom-mode-in))
  :config
  (defun my:darkroom-mode-in ()
    (interactive)
    (darkroom-mode 1)
    (bind-key "<f10>" 'my:darkroom-mode-out darkroom-mode-map))
  (defun my:darkroom-mode-out ()
    (interactive)
    (darkroom-mode 0))
  (defun my:full-darkroom-mode-in ()
    (interactive)
    (toggle-frame-fullscreen)
    (darkroom-mode 1)
    (bind-key "<f12>" 'my:full-darkroom-mode-out darkroom-mode-map))
  (defun my:full-darkroom-mode-out ()
    (interactive)
    (darkroom-mode 0)
    (toggle-frame-fullscreen)))

(use-package perfect-margin
  :ensure t
  :demand t
  :custom
  (perfect-margin-visible-width 200)
  :config
  (setq perfect-margin-ignore-filters nil)
  (setq perfect-margin-ignore-regexps nil)
  (perfect-margin-mode 1))

(use-package spacious-padding
  :ensure t
  :demand t
  :config
  (setopt spacious-padding-widths
          '( :internal-border-width 14
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 30
             :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package breadcrumb
  :ensure t
  :demand t
  :config (breadcrumb-mode))

;;; IME

;; reference
;; https://qiita.com/takaxp/items/a86ee2aacb27c7c3a902
;; https://masutaka.net/chalow/2015-01-04-1.html

(defconst my-cur-color-ime '(:on "#FF9300" :off "#91C3FF"))
(defconst my-cur-type-ime '(:on box :off box :invisible nil))
(defvar my-ime-last nil)
(defvar mac-win-last-ime-status 'off)
(defun mac-win-save-last-ime-status ()
  "Save IME status by input source."
  (setq mac-win-last-ime-status
        (if (string-match "\\.\\(Roman\\|US\\)$" (mac-input-source))
            'off 'on)))
(defun advice:mac-auto-ascii-setup-input-source (&optional _prompt)
  "Extension to store IME status."
  (mac-win-save-last-ime-status))

(if (fboundp 'mac-ime-active-p)
    (defalias 'my-ime-active-p 'mac-ime-active-p)
  (defun my-ime-active-p () current-input-method))

(defun my-ime-on-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :on))
  (set-cursor-color (plist-get my-cur-color-ime :on)))

(defun my-ime-off-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :off))
  (set-cursor-color (plist-get my-cur-color-ime :off)))

(defun my-ime-invisible-cursor ()
  (interactive)
  (setq cursor-type (plist-get my-cur-type-ime :invisible)))

(defvar my--graphic-display-p (display-graphic-p))
(defun my-apply-cursor-config ()
  (interactive)
  (when my--graphic-display-p
    (if (my-ime-active-p) (my-ime-on-cursor) (my-ime-off-cursor))))

;; for init setup
(setq-default cursor-type (plist-get my-cur-type-ime :on))
(unless noninteractive
  (add-hook 'buffer-list-update-hook #'my-apply-cursor-config)
  (my-apply-cursor-config))
(add-hook 'input-method-activate-hook #'my-ime-on-cursor)
(add-hook 'input-method-deactivate-hook #'my-ime-off-cursor)

(when (eq window-system 'mac) ;; for EMP
  (when (fboundp 'mac-input-source)
    (defun my-mac-keyboard-input-source ()
	    (if (string-match "\\.US$" (mac-input-source))
	        (progn
	          (setq cursor-type (plist-get my-cur-type-ime :off))
	          (add-to-list 'default-frame-alist
			                   `(cursor-type . ,(plist-get my-cur-type-ime :off)))
	          (set-cursor-color (plist-get my-cur-color-ime :off)))
	      (progn
	        (setq cursor-type (plist-get my-cur-type-ime :on))
	        (add-to-list 'default-frame-alist
			                 `(cursor-type . ,(plist-get my-cur-type-ime :on)))
	        (set-cursor-color (plist-get my-cur-color-ime :on)))))

	  (mac-auto-ascii-mode 1)
	  ;; IME ON/OFF でカーソルの種別や色を替える
	  (add-hook 'mac-selected-keyboard-input-source-change-hook
		          #'my-mac-keyboard-input-source)
	  ;; IME ON の英語入力＋決定後でもカーソルの種別や色を替える
	  (add-hook 'mac-enabled-keyboard-input-sources-change-hook
	            #'my-mac-keyboard-input-source)
	  (declare-function my-mac-keyboard-input-source "init" nil)
	  (my-mac-keyboard-input-source)

    (defun mac-win-restore-ime ()
      (when (and mac-auto-ascii-mode (eq mac-win-last-ime-status 'on))
        (mac-select-input-source
         "com.google.inputmethod.Japanese.base")))

    (advice-add 'mac-auto-ascii-setup-input-source :before
                #'advice:mac-auto-ascii-setup-input-source)

    (defun mac-win-restore-ime-target-commands ()
      (when (and mac-auto-ascii-mode
                 (eq mac-win-last-ime-status 'on)
                 (memq this-command mac-win-target-commands))
        (mac-select-input-source
         "com.google.inputmethod.Japanese.base")))
    (add-hook 'pre-command-hook 'mac-win-restore-ime-target-commands)

    ;; M-x でのコマンド選択でもIMEを戻せる．
    ;; ただし，移動先で q が効かないことがある（要改善）
    (add-hook 'minibuffer-setup-hook 'mac-win-save-last-ime-status)
    (add-hook 'minibuffer-exit-hook 'mac-win-restore-ime)

    ;; 自動で ASCII入力から日本語入力に引き戻したい関数（デフォルト設定）
    (defvar mac-win-target-commands
      '(find-file save-buffer other-window delete-window split-window))))

(use-package ccc
  :ensure (:host github :repo "skk-dev/ddskk"
           :files ("ccc.el")
           :pre-build (("perl" "-pi" "-e"
                        "print \";; Version: 1.43\\n\" if /^;; Keywords:/ && !$done++"
                        "ccc.el")))
  :demand t)

(use-package ddskk
  :ensure t
  :custom
  (skk-egg-like-newline t)
  (skk-use-look t)
  (skk-sticky-key ";"))

;;; dired

(use-package dired
  :ensure nil
  :init
  (defun dired-toggle-mark (arg)
    "Toggle the current (or next ARG) files."
    (interactive "P")
    (let ((dired-marker-char
           (if (save-excursion (beginning-of-line)
                               (looking-at " "))
               dired-marker-char ?\040)))
      (dired-mark arg)
      (dired-previous-line 1)))
  (defun dired-my-advertised-find-file ()
    "Do not make new buffer for dired."
    (interactive)
    (let ((kill-target (current-buffer))
          (check-file (dired-get-filename)))
      (funcall 'dired-find-file)
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
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (dired-guess-shell-gnutar "tar")
  (dired-guess-shell-alist-user '(("\\.tar\\.gz\\'" "tar ztvf")
                                  ("\\.tar\\'" "tar ztvf")
                                  ("\\.tar\\.bz2\\'" "tar Itvf")
                                  ("\\.zip\\'" "unzip -l")
                                  ("\\.\\(g\\|\\ z\\'" "zcat")
                                  ("\\.\\(jpg\\|JPG\\|git\\|GIF\\)\\'"
                                   (if (eq system-type 'window-nt)
                                       "fiber" "xv"))
                                  ("\\.ps\\'"
                                   (if (eq system-type 'window-nt)
                                       "fiber" "ghostview"))))
  :bind (:map dired-mode-map
         (" " . dired-toggle-mark)
         ("\C-m" . dired-my-advertised-find-file)
         ("^" . dired-my-up-directory)
         ("r" . wdired-change-to-wdired-mode)))

(use-package wdired :ensure nil :demand t)

;; output directory first
(setq insert-directory-program "gls")
(setq dired-listing-switches "-AFl --group-directories-first")

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".DS_Store")))

;;; completion

(use-package orderless :ensure t)

(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :custom
  (vertico-count 40)
  (completion-styles '(orderless))
  :hook
  (after-init . savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-S-s" . consult-imenu)
   ("C-x C-r" . consult-recentf-file)
   ("M-s M-b" . consult-buffer))
  :defines vertico-map
  :custom
  (consult-find-command "fd --color=never --full-path ARG OPTS")
  :config
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-r") 'vertico-previous)
    (define-key vertico-map (kbd "C-s") 'vertico-next))
  (setq xref-show-xrefs-function #'consult-xref))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; magit

(use-package magit
  :ensure t
  :bind (("C-x m" . magit-status)
         ("C-c l" . magit-blame-addition))
  :init
  (setq-default magit-auto-revert-mode nil))

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package diff-hl
  :ensure t
  :commands diff-hl-magit-pre-refresh diff-hl-magit-post-refresh diff-hl-dired-mode
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1)
  :config
  ;; diff-hl-dired の非同期 vc プロセスがバッファ kill 時に確認を求めないようにする
  (advice-add 'diff-hl-dired-update :after
              (lambda ()
                (dolist (buf (buffer-list))
                  (when (string-match-p "\\*diff-hl-dired tmp status\\*" (buffer-name buf))
                    (let ((proc (get-buffer-process buf)))
                      (when proc
                        (set-process-query-on-exit-flag proc nil))))))))

(use-package difftastic
  :ensure t
  :demand t
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff
      '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))

;;; libvterm

(use-package vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :bind
  ("<f2>" . vterm-toggle)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-keymap-exceptions '("<f1>" "<f2>" "<f8>" "C-c" "C-x" "C-g" "C-l" "M-x" "C-v" "M-v" "C-y" "C-t" "C-z"))
  :config
  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm--self-insert))

(use-package vterm-toggle
  :ensure (:host github :repo "jixiuf/vterm-toggle"
           :pre-build (("perl" "-pi" "-e"
                        "s/:type 'symbolp/:type 'hook/g"
                        "vterm-toggle.el")))
  :custom
  (vterm-toggle-scope 'project)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname (eq major-mode ' vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  (defun my/vterm-new-buffer-in-current-window ()
    (interactive)
    (let ((display-buffer-alist nil))
      (vterm))))

;;; Projectile

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :defines projectile-mode-map
  :custom
  (projectile-project-search-path '("~/ghq"))
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode t))

;; completion

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-popupinfo-delay nil)
  :bind (:map corfu-map
         ("C-s" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; flymake

(use-package package-lint-flymake :ensure t)
(use-package flymake-diagnostic-at-point
  :ensure t
  :config
  ;; Emacs 30+ renamed flymake--diag-text to flymake-diagnostic-text
  (unless (fboundp 'flymake--diag-text)
    (defalias 'flymake--diag-text #'flymake-diagnostic-text)))
(use-package popup :ensure t)
(use-package posframe :ensure t)

(use-package flymake
  :ensure nil
  :bind (("M-N" . flymake-goto-next-error)
         ("M-P" . flymake-goto-prev-error))
  :config
  (advice-add 'flymake-proc-legacy-flymake :override #'ignore)

  (with-eval-after-load 'flymake
    (with-eval-after-load 'flymake-diagnostic-at-point
      (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
    (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
    (with-eval-after-load 'popup
      (set-face-attribute 'popup-tip-face nil
                          :background "dark slate gray"
                          :foreground "white"
                          :underline nil)))

  ;; flymake-posframe
   (with-eval-after-load 'flymake-diagnostic-at-point
     (defvar flymake-posframe-hide-posframe-hooks
       '(pre-command-hook post-command-hook focus-out-hook)
       "The hooks which should trigger automatic removal of the posframe.")
     (defun flymake-posframe-hide-posframe ()
       "Hide messages currently being shown if any."
       (posframe-hide " *flymake-posframe-buffer*")
       (dolist (hook flymake-posframe-hide-posframe-hooks)
         (remove-hook hook #'flymake-posframe-hide-posframe t)))
     (defun my/flymake-diagnostic-at-point-display-popup (text)
       "Display the flymake diagnostic TEXT inside a posframe."
       (posframe-show " *flymake-posframe-buffer*"
                      :string (concat flymake-diagnostic-at-point-error-prefix text)
                      :position (point)
                      :foreground-color "cyan"
                      :internal-border-width 2
                      :internal-border-color "red"
                      :poshandler 'posframe-poshandler-window-bottom-left-corner)
       (dolist (hook flymake-posframe-hide-posframe-hooks)
         (add-hook hook #'flymake-posframe-hide-posframe nil t)))
     (advice-add 'flymake-diagnostic-at-point-display-popup :override 'my/flymake-diagnostic-at-point-display-popup)))

;;; Snippet

(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs '("~/.emacs.d/mysnippets"))
  :bind (:map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("C-c C-y" . company-yasnippet)
         ("C-x C-i i" . yas-insert-snippet)
         ("C-x C-i n" . yas-new-snippet)
         ("C-x C-i v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)

;;; Edit

(use-package quickrun
  :ensure t
  :defer t)

(use-package smartrep :ensure t)

(use-package multiple-cursors
  :ensure t
  :after smartrep
  :bind
  ("C-M-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))

(use-package undo-tree
  :ensure t
  :demand t
  :diminish "UT"
  :defines undo-tree-visualizer-mode-map
  :custom
  (global-undo-tree-mode t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("" . ,(concat user-emacs-directory "undo-tree/"))))
  (undo-tree-visualizer-timestamps t)
  :config
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit))

(use-package rg
  :ensure t
  :defer t)

(use-package eglot
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'scala-mode-hook 'eglot-ensure))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :init (eglot-booster-mode 1))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-ts-mode . tree-sitter-hl-mode)
         (tsx-ts-mode . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(use-package jsonrpc
  :ensure nil
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))

;;; Variouts mode

(setq major-mode 'text-mode) ;; default mode is text mode

;; Go
(use-package go-mode
  :ensure t
  :defines c-basic-offset
  :init
  (add-hook 'go-mode-hook #'(lambda ()
                             (setq c-basic-offset 4)
                             (setq indent-tabs-mode t)
                             (local-set-key (kbd "M-.") 'godef-jump)
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                             (local-set-key (kbd "C-c i") 'go-goto-imports)
                             (local-set-key (kbd "C-c d") 'godoc)))
  :custom
  (gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'tree-sitter-mode))

; https://go.googlesource.com/tools/+/refs/heads/master/gopls/doc/emacs.md
(defun project-find-go-module (dir)
  "Find go.mod file and append DIR to project root."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  "Get PROJECT root directory."
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)

;; web
(use-package web-mode
  :ensure t
  :functions sp-local-pair
  :defines web-mode-map
  :mode
  ("\\.erb\\'"
   "\\.html?\\'"
   "\\.tpl\\'"
   "\\.tmpl\\'"
   "\\.vue\\'")
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'web-mode "<" ">" :actions nil))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (define-key web-mode-map (kbd "C-c C-b b") nil)
  (define-key web-mode-map (kbd "C-c C-b c") nil)
  (define-key web-mode-map (kbd "C-c C-b e") nil)
  (define-key web-mode-map (kbd "C-c C-b k") nil)
  (define-key web-mode-map (kbd "C-c C-b n") nil)
  (define-key web-mode-map (kbd "C-c C-b p") nil)
  (define-key web-mode-map (kbd "C-c C-b s") nil)
  (define-key web-mode-map (kbd "C-c i b") 'web-mode-block-beginning)
  (define-key web-mode-map (kbd "C-c i c") 'web-mode-block-close)
  (define-key web-mode-map (kbd "C-c i e") 'web-mode-block-end)
  (define-key web-mode-map (kbd "C-c i k") 'web-mode-block-kill)
  (define-key web-mode-map (kbd "C-c i n") 'web-mode-block-next)
  (define-key web-mode-map (kbd "C-c i p") 'web-mode-block-previous)
  (define-key web-mode-map (kbd "C-c i s") 'web-mode-block-select))

;; typescript
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts?\\'" . tsx-ts-mode)
         ("\\.tsx?\\'" . tsx-ts-mode)))

(use-package tide
  :ensure t
  :commands setup-tide-mode
  :hook (tsx-ts-mode . setup-tide-mode)
  :config
  (with-eval-after-load 'tide
    (defun setup-tide-mode nil
      (interactive)
      (tide-setup)
      (flycheck-mode 1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode 1)
      (tide-hl-identifier-mode 1)
      (company-mode 1))

    (setq company-tooltip-align-annotations t)))

;; json
(use-package json-mode
  :ensure t
  :init (add-hook 'json-mode-hook #'(lambda () (make-local-variable 'js-indent-level) (setq js-indent-level 2))))

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :custom
  (markdown-hide-urls nil)
  :defines markdown-mode-map
  :hook
  (markdown-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (custom-set-variables
   '(markdown-code-lang-modes
     (append
      '(("conf" . conf-mode)
        ("diff" . diff-mode)
        ("go" . go-mode)
        ("hs" . haskell-mode)
        ("html" . web-mode)
        ("ini" . conf-mode)
        ("js" . web-mode)
        ("json" . json-mode)
        ("jsx" . rjsx-mode)
        ("md" . markdown-mode)
        ("py" . python-mode)
        ("rb" . ruby-mode)
        ("rs" . rustic-mode)
        ("sql" . sql-mode)
        ("ts" . web-mode)
        ("tsx" . rjsx-mode)
        ("yaml". yaml-mode)
        ("zsh" . sh-mode))
      markdown-code-lang-modes))))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode)

;; scheme
(use-package geiser-gauche :ensure t)

;; elisp
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
         ("C-M-q" . nil)
         ("C-c C-e" . macrostep-expand))
  :config
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode))

(use-package elisp-slime-nav
  :ensure t
  :after elisp-mode
  :diminish elisp-slime-nav-mode
  :bind (:map elisp-slime-nav-mode-map ("C-c C-d" . helpful-at-point))
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (help-mode . elisp-slime-nav-mode)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

(use-package ielm
  :ensure nil
  :bind (:map ielm-map
         ("C-c C-d" . helpful-at-point)))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hindent
  :ensure t
  :after haskell-mode)

;; python
(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq indent-tabs-mode nil)))))

;; c/c++
(use-package cc-mode
  :ensure nil
  :hook
  ((c-mode . (lambda () (local-unset-key (kbd "C-c C-b"))))
   (c-mode . (lambda () (setq comment-start "//"
                               comment-end   "")))
   (c-mode . (lambda () (setq-default c-basic-offset 2)))
   (c++-mode . (lambda () (local-unset-key (kbd "C-c C-b"))))
   (c++-mode . (lambda () (setq comment-start "//"
                                 comment-end   "")))
   (c++-mode . (lambda () (setq-default c-basic-offset 2)))))

(use-package clang-format
  :ensure t
  :after cc-mode
  :init
  (defun set-hook-after-save-clang-format ()
    (add-hook 'after-save-hook 'clang-format-buffer t t))
  :hook ((c-mode . set-hook-after-save-clang-format)
         (c++-mode . set-hook-after-save-clang-format))
  :bind (:map c-mode-map ([remap indent-whole-buffer] . clang-format-buffer))
  :bind (:map c++-mode-map ([remap indent-whole-buffer] . clang-format-buffer)))

;; rust
(use-package rustic
  :ensure t
  :custom
  (rustic-format-display-method 'ignore)
  (rustic-lsp-client 'eglot)
  (rustic-babel-display-error-popup nil)
  :config
  (defun my/find-rust-project-root (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (list 'vc 'Git root)))
  (defun my/rust-mode-hook ()
    (setq-local project-find-functions (list #'my/find-rust-project-root)))
  (add-hook 'rust-mode-hook #'my/rust-mode-hook))

;; ocaml
(use-package tuareg
  :ensure t)

;; lua
(use-package lua-ts-mode
  :ensure nil
  :mode ("\\.lua$"))

;; scala
(use-package scala-mode
  :ensure t
  :interpreter ("scala"))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  (with-eval-after-load 'sbt-mode
    (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)
    (setq sbt:program-options '("-Dsbt.supershell=false"))))

(use-package css-mode :ensure nil)
(use-package csv-mode :ensure t)
(use-package toml-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)
(use-package dotenv-mode :ensure t :mode "\\.env\\..*\\'")
(use-package envrc :ensure t)
(use-package kotlin-mode :ensure t)
(use-package nginx-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package yaml-mode :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'")
  :config
  (defun my-protobuf-indent-line ()
    (let ((indent-size (string-to-number (gethash 'indent_size editorconfig-properties-hash "2"))))
      (setq c-basic-offset indent-size)
      (c-indent-line)))
  (add-hook 'protobuf-mode-hook
          (lambda ()
            (setq indent-line-function 'my-protobuf-indent-line))))

(use-package sh-script
  :ensure nil
  :custom (sh-basic-offset 2)
  :defines sh-shell
  :mode ("\\.zsh\\'" . sh-mode))

(use-package rfc-mode
  :ensure t
  :demand t
  :custom
  (rfc-mode-directory "~/Documents/rfc"))

(use-package pdf-tools
  :ensure t
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward))
  :custom
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 1.1)
  :config
  (pdf-tools-install))

;; copilot

(use-package copilot
  :ensure (:host github :repo "zerolfx/copilot.el")
  :bind (:map copilot-mode-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

;; claude code

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-cli-path "~/.claude/local/claude")
  (claude-code-ide-window-width 120)
  :config
  (claude-code-ide-emacs-tools-setup))

;; editorconfig

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; ellama

(use-package llm :ensure t)

(use-package ellama
  :ensure t
  :after llm
  :config
  (require 'llm-ollama)
  (setopt ellama-language "Japanese")
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-provider (make-llm-ollama
                           :chat-model "gemma2"
                           :embedding-model "gemma2"))
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "gemma2:9b"
                                       :embedding-model "gemma2:9b"))
  (with-eval-after-load 'embark
    (define-key embark-region-map (kbd "T") 'ellama-translate)))

;;; elfeed

(use-package elfeed
  :ensure t
  :bind
  ("C-x w" . elfeed)
  :defines (elfeed-feeds elfeed-search-filter elfeed-show-mode-hook elfeed-show-entry-switch)
  :functions my-show-elfeed
  :config
  (setq elfeed-feeds '(("https://planet.emacslife.com/atom.xml" emacs)
                        ("https://sachachua.com/blog/feed/" emacs)
                        ("https://protesilaos.com/codelog.xml" emacs)
                        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" youtube awesomekling)))
  (setq elfeed-search-filter "@3-days-ago +unread")
  (defun my-show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer))
  (setq elfeed-show-mode-hook
   (lambda ()
	   (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Iosevka" :size 14))
	   (setq fill-column 120)
	   (setq elfeed-show-entry-switch #'my-show-elfeed))))

;;; shortdoc

(use-package shortdoc
  :ensure nil
  :config
  (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Iosevka" :size 14)))

;;; Org

(use-package org-superstar :ensure t :defer t)
(use-package org-appear :ensure t :defer t)

(use-package org
  :ensure t
  :defines (org-default-notes-file org-agenda-files)
  :functions (yank-with-indent copy-region-unindented)
  :bind
  (:map org-mode-map
   ("C-h" . delete-backward-char)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c y" . yank-with-indent)
   ("C-c M-w" . copy-region-unindented))
  :hook
  (org-mode . org-appear-mode)
  (org-mode . (lambda () (org-superstar-mode 1)))
  (org-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (setq org-return-follows-link t)
  (setq org-startup-folded t)
  (setq org-startup-truncated nil)
  (setq org-log-done 'time)
  (setq org-hide-leading-stars t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation nil)
  (setq org-todo-keywords '((sequence "TODO(t)" "FOCUS(f)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
  (setq org-todo-keyword-faces '(("FOCUS" :foreground "#FF0000" :background "#FFCC66")
                                  ("WAIT" :foreground "#CCCCCC" :background "#666666")))
  (setq org-appear-autolinks t)
  (setq org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
  (setq org-default-notes-file (concat (getenv "ORGSYNCROOT") "/org/journal.org"))
  (setq org-agenda-files (list (concat (getenv "ORGSYNCROOT") "/org/")))
  ;; ox (org-export) の設定 — org ロード後に評価する必要がある
  (setq org-export-with-timestamps nil)
  (require 'ox-md)
  ;; https://emacs.stackexchange.com/questions/31646/how-to-paste-with-indent
  (defun yank-with-indent ()
    "Yank with indentation."
    (interactive)
    (let ((indent
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (message indent)
      (yank)
      (narrow-to-region (mark t) (point))
      (pop-to-mark-command)
      (while (search-forward "\n" nil t) (replace-match (concat "\n" indent) nil t))
      (widen)))
  ;; https://emacs.stackexchange.com/questions/34966/copy-region-without-leading-indentation
  (defun copy-region-unindented (pad beginning end)
    "Copy the region, un-indented by the length of its minimum indent calculated
by PAD, BEGINNING and END."
    (interactive "P\nr")
    (let ((buf (current-buffer))
          (itm indent-tabs-mode)
          (tw tab-width)
          (st (syntax-table))
          (indent nil))
      (with-temp-buffer
        (setq indent-tabs-mode itm
              tab-width tw)
        (set-syntax-table st)
        (insert-buffer-substring buf beginning end)
        ;; Establish the minimum level of indentation.
        (goto-char (point-min))
        (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                    (not (eobp)))
          (let ((length (current-column)))
            (when (or (not indent) (< length indent))
              (setq indent length)))
          (forward-line 1))
        (if (not indent)
            (error "Region is entirely whitespace")
          ;; Un-indent the buffer contents by the length of the minimum
          ;; indent level, and copy to the kill ring.
          (when pad
            (setq indent (- indent (prefix-numeric-value pad))))
          (indent-rigidly (point-min) (point-max) (- indent))
          (copy-region-as-kill (point-min) (point-max))))))
  )

(use-package org-capture
  :ensure nil
  :defines (task-file nippou-file idea-file tweet-file org-capture-templates)
  :bind ("C-c r" . org-capture)
  :config
  ;; https://ladicle.com/post/20200625_123915/
  (defvar org-code-capture--store-file "")
  (defvar org-code-capture--store-header "")
  (defun org-code-capture--store-here ()
    "Register current subtree as a capture point."
    (interactive)
    (setq org-code-capture--store-file (buffer-file-name))
    (setq org-code-capture--store-header (nth 4 (org-heading-components))))
  (defun org-code-capture--find-store-point ()
    "Find registered capture point and move the cursor to it."
    (let ((filename org-code-capture--store-file))
      (set-buffer (org-capture-target-buffer filename)))
    (goto-char (point-min))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for org-code-capture--find-store-file should be in Org mode"
       (current-buffer))
      (current-buffer))
    (if (re-search-forward org-code-capture--store-header nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* Capture\n")
      (beginning-of-line 0))
    (org-end-of-subtree))
  (setq code-file (concat (getenv "ORGSYNCROOT") "/org/code.org"))
  (setq memo-file (concat (getenv "ORGSYNCROOT") "/org/memo.org"))
  (setq compp-file (concat (getenv "ORGSYNCROOT") "/org/compp.org"))
  (setq scribble-file (concat (getenv "ORGSYNCROOT") "/org/scribble.org"))
  (setq org-capture-templates
      `(("c" "Code" plain
         (function org-code-capture--find-store-point)
         "%^{Summary}\n%(with-current-buffer (org-capture-get :original-buffer) (browse-at-remote-get-url))\n# %(with-current-buffer (org-capture-get :original-buffer) (file-full-path))\n\n%i\n"
         :immediate-finish 1)
        ("m" "Memo" entry
         (file memo-file)
         "* %? %U %^g\n"
         :empty-lines 1)
        ("p" "Compe" entry
         (file memo-file)
         "* %? %U :competitive:\n"
         :empty-lines 1)
        ("s" "Scribble" entry
         (file scribble-file)
         "* %? %U %^g\n\n"
         :prepend t
         :empty-lines 1))))

;;; Utility

(use-package google-this :ensure t)
(use-package which-key :ensure t :custom (which-key-mode 1))
(use-package germanium :ensure t :custom (germanium-check-options-each-execute-command nil))

(use-package affe
  :ensure t
  :custom
  (affe-find-command "fd --color=never --full-path --hidden --exclude .git")
  (affe-highlight-function 'orderless-highlight-matches)
  (affe-regexp-function 'orderless-pattern-compiler))

(use-package consult-ghq
  :ensure t)

(use-package browse-at-remote
  :ensure t
  :demand t
  :functions browse-at-remote-get-url
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :bind
  ("C-C b" . browse-at-remote)
  ("C-c C-b" . bar-to-clipboard))

(defun other-window-or-split ()
  "Move to other window or split window."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun open-by-vscode ()
  "Open current buffer by vscode."
  (interactive)
  (shell-command
   (format "code -r -g %s:%d:%d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column))))

(defun to-clipboard (x)
  "Paste X to clipboard."
  (when x
    (with-temp-buffer
      (insert x)
      (clipboard-kill-region (point-min) (point-max)))
    (message x)))

(defun file-full-path ()
  "Return current file absolute path and column number."
  (if (equal major-mode 'dired-mode)
      default-directory
    (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))

(defun file-full-path-org-link-to-clipboard ()
  "Copy org link about current file to clipboard."
  (interactive)
  (to-clipboard (concat "[[" (file-full-path) "][" (file-name-nondirectory buffer-file-name) "]]")))

(defun copy-word-at-point ()
  "Copy the word at point to kill ring."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (kill-new word)
      (message "Copied: %s" word))))

(setq comment-empty-lines t)

;;; Key bindings

(setq file-name-handler-alist my/saved-file-name-handler-alist)

;; (profiler-report)
;; (profiler-stop)

(provide 'init)
;;; init.el ends here
