;;; init.el -*- lexical-binding: t -*-

;; Author: matsuyoshi30

;;; Commentary:

;; This program is Emacs init.el

;;; Code:

;;; Measurement

(defconst my-before-load-init-time (current-time))
(defconst my-loading-profile-p nil
  "If non-nil, show tick while booting. Do not use `my-profiler-p' with this.")

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

;; (eval-when-compile
;;   (setq byte-compile-warnings '(cl-functions)))

(eval-and-compile
  (prog1 "initialize leaf.el"
    (customize-set-variable
     'package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf))
    (leaf leaf-keywords
      :ensure t
      :init
      (leaf el-get :ensure t)
      :config
      (leaf-keywords-init))))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(when (eq system-type 'darwin)
  (defvar mac-pass-control-to-system t)) ;; Ctrl to Emacs

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

(defalias 'yes-or-no-p 'y-or-n-p)

(setq kill-whole-line t)

(setq-default tab-width 2 indent-tabs-mode nil)

(global-auto-revert-mode 1) ;; read buffer automatically

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq vc-follow-symlinks t) ;; avoid symbolic link

(setq confirm-kill-emacs 'y-or-n-p)

(defvar ac-comphist-file "~/.emacs.d/cache/auto-complete/ac-comphist.dat")
(defvar eshell-directory-name "~/.emacs.d/cache/eshell/")
(defvar auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.saves-")

;;; Path

(autoload 'exec-path-from-shell "exec-path-from-shell" nil t)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))

;;; Utility

;; vs code
(defun open-by-vscode ()
  (interactive)
  (shell-command
   (format "code -r -g %s:%d:%d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column))))
(define-key global-map (kbd "C-c C-v") 'open-by-vscode)

;;; Display

(require 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region 'bg-only)
(modus-themes-load-themes)
;;(modus-themes-load-operandi)
(modus-themes-load-vivendi)
(global-set-key (kbd "<f5>") 'modus-themes-toggle)

(setq inhibit-startup-screen 0)

(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
    (set-fontset-font "fontset-ricty"
                      'unicode
                      (font-spec :family "Ricty")
                      nil
                      'append)
    (add-to-list 'default-frame-alist '(font . "fontset-ricty"))
    (defvar my:font-size 14)
    (progn
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
                 ) initial-frame-alist)))))
    (progn
      (setq default-frame-alist initial-frame-alist)))

(column-number-mode t)
(line-number-mode t)

(show-paren-mode t)
(transient-mark-mode t)
(size-indication-mode t)

(setq display-time-day-and-date t)
(defvar display-time-string-forms
      '(month "/" day " " dayname " "
              24-hours ":" minutes " "))
(display-time-mode t)
(display-battery-mode t)

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

;; smart mode line
(defvar sml/active-background-color "gray60")
(defvar sml/modified-char "*")
(defvar sml/no-confirm-load-theme t)
(defvar sml/theme 'dark)
(defvar sml/shorten-directory -1)
(sml/setup)

(leaf diminish :ensure t)

;; avoid wired split when display-buffer
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; elscreen
(defvar elscreen-prefix-key (kbd "C-z"))
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
(leaf flycheck :ensure t)

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
(setq gofmt-command "goimports")
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

(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; python mode
(autoload 'python-mode "python-mode" nil t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil)))) ;; indent to space

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

(leaf dockerfile-mode :ensure t)
(leaf docker-compose-mode :ensure t)
(leaf yaml-mode :ensure t)
(leaf terraform-mode :ensure t)
(leaf protobuf-mode :ensure t)

;; sh mode
(add-to-list 'auto-mode-alist '("\\.sh$" . sh-mode))

;; rfc mode
(defvar rfc-mode-directory (expand-file-name "~/Documents/rfc/"))

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

(define-key global-map (kbd "C-c l") 'org-store-link)

;;; Key bindings

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)

;; multi line move
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;; full screen
(global-set-key (kbd "C-x F") 'toggle-fGrame-maximized)
(global-set-key (kbd "C-x ?") 'help-command)

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "M-SPC") 'expand-abbrev)

;; redo
(global-set-key "\M-/" 'redo)

;;; quickrun
(global-set-key "\C-cc" 'quickrun-with-arg)

;;; auto-complete
(define-key global-map (kbd "<C-tab>") 'ac-fuzzy-complete)

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

(provide 'init)
;;; init.el ends here
