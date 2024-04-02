;;; init.el --- init.el -*- lexical-binding: t -*-

;; Author: matsuyoshi30

;;; Commentary:

;; This program is Emacs init.el

;;; Code:

;;; General

(eval-when-compile (setq byte-compile-warnings '(cl-functions)))

(eval-and-compile
  (prog1 "initialize leaf.el"
    (customize-set-variable
     'package-archives '(;("org"   . "https://orgmode.org/elpa/")
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
      (leaf diminish :ensure t)
      (leaf-keywords-init))))

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

(leaf leaf
  :config
  (leaf leaf-convert
    :ensure t
    :config
    (leaf use-package :ensure t))
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom
  `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  :custom '((user-full-name . "Masaya Watanabe")
            (user-mail-address . "sfbgwm30@gmail.com")
            (user-login-name . "matsuyoshi30")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            )
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf recentf
  :custom
  ((recentf-max-saved-items . 2000)
   (recentf-exclude . '("\\.elc$" "\\.o$" "~$" "\\.undo-tree/" "PATH" "^/[^/:]+:"))
   (recentf-auto-cleanup . 'never))
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

(leaf dash :ensure t)

(leaf gcmh
  :ensure t
  :diminish gcmh
  :custom (gcmh-verbose . t)
  :config
  (gcmh-mode 1))

(leaf midnight
  :require t
  :hook
  (emacs-startup-hook . midnight-mode))

(leaf expand-region
  :ensure t
  :bind
  (("C-@" . er/expand-region)))

;;; Path

(autoload 'exec-path-from-shell "exec-path-from-shell" nil t)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;; Display

(set-face-attribute 'default nil :family "HackGen Console" :height 130)
(set-face-attribute 'fixed-pitch nil :family "HackGen Console" :height 120)
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 120)

(leaf ef-themes
  :ensure t
  :defun my-ef-themes-default-font-face
  :custom
  (ef-themes-headings . '((0 . (variable-pitch light 1.9))
                          (1 . (variable-pitch light 1.8))
                          (2 . (variable-pitch regular 1.7))
                          (3 . (variable-pitch regular 1.6))
                          (4 . (variable-pitch regular 1.5))
                          (5 . (variable-pitch 1.4))
                          (6 . (variable-pitch 1.3))
                          (7 . (variable-pitch 1.2))
                          (t . (variable-pitch 1.1))))
  (ef-themes-mixed-font . t)
  (ef-themes-variable-pitch-ui . t)
  (ef-themes-to-toggle . '(ef-summer ef-winter))
  (ef-themes-region . '(intense no-extend neutral))
  :config
  (defun my-ef-themes-default-font-face ()
    (ef-themes-with-colors
      `(default ((,c :height 130)))))
  (mapc #'disable-theme custom-enabled-themes)
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-default-font-face))
(ef-themes-select 'ef-cyprus)

(leaf neotree
  :ensure t
  :defun neo-global--window-exists-p
  :custom
  (neo-show-hidden-files . t)
  (neo-theme . 'icons))
(defun neotree-text-scale ()
  "Text scale for neotree."
  (interactive)
  (text-scale-adjust 0)
  (text-scale-decrease 0.2)
  (message nil))
(add-hook 'neo-after-create-hook
      (lambda (_)
        (call-interactively 'neotree-text-scale)))

(leaf tab-bar-mode
  :init
  (tab-bar-mode 1)
  :custom
  ((tab-bar-new-tab-choice         . "*scratch*")
   (tab-bar-tab-name-truncated-max . 12)))

(setq display-time-day-and-date t)
(defvar display-time-string-forms
  '(month "/" day " " dayname " "
          24-hours ":" minutes " "))
(display-time-mode t)
(display-battery-mode t)

(leaf visual-line-mode
  :require simple
  :config
  (global-visual-line-mode t)
  (diminish 'visual-line-mode nil))

(leaf volatile-highlights
  :ensure t
  :require t
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

(leaf nyan-mode
  :ensure t
  :init
  (defvar nyan-bar-length 16)
  :config
  (nyan-mode t))

(leaf smartparens
  :ensure t
  :require smartparens-config
  :diminish smartparens-mode
  :defun sp-pair
  :custom (sp-escape-quotes-after-insert . nil)
  :bind (:smartparens-mode-map
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

(leaf rainbow-mode
  :ensure t
  :hook
  css-mode-hook
  sass-mode-hook
  scss-mode-hook
  web-mode-hook)


;; all-the-icons

(leaf all-the-icons
  :ensure t
  :custom
  ((all-the-icons-scale-factor . 0.9)
   (all-the-icons-default-adjust . 0.0))
  :config
  (leaf all-the-icons-ivy
    :require t
    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
  (leaf all-the-icons-dired
    :require t
    :diminish all-the-icons-dired-mode))

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  ((doom-modeline-lsp . t)))

(transient-mark-mode t)
(size-indication-mode t)
(setq-default tab-width 2 indent-tabs-mode nil)

(leaf moom
  :ensure t
  :defvar moom-mode-map
  :config
  (moom-mode t)
  ;;(define-key moom-mode-map (kbd "M-0") 'moom-move-frame)
  (define-key moom-mode-map (kbd "M-1") 'moom-move-frame-left)
  (define-key moom-mode-map (kbd "M-2") 'moom-move-frame-to-center)
  (define-key moom-mode-map (kbd "M-3") 'moom-move-frame-right))

(leaf darkroom
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

(defun my-apply-cursor-config ()
  (interactive)
  (when (display-graphic-p)
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
                 (eq mac-win-last-ime-status 'on))
        (mapc (lambda (command)
                (when (string-match
                       (format "^%s" command) (format "%s" this-command))
                  (mac-select-input-source
                   "com.google.inputmethod.Japanese.base")))
              mac-win-target-commands)))
    (add-hook 'pre-command-hook 'mac-win-restore-ime-target-commands)

    ;; M-x でのコマンド選択でもIMEを戻せる．
    ;; ただし，移動先で q が効かないことがある（要改善）
    (add-hook 'minibuffer-setup-hook 'mac-win-save-last-ime-status)
    (add-hook 'minibuffer-exit-hook 'mac-win-restore-ime)

    ;; 自動で ASCII入力から日本語入力に引き戻したい関数（デフォルト設定）
    (defvar mac-win-target-commands
      '(find-file save-buffer other-window delete-window split-window))))

(leaf ddskk
  :ensure t
  :custom
  (skk-egg-like-newline . t)
  (skk-use-look . t)
  (skk-sticky-key . ";"))

;;; dired

(leaf dired
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
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :custom
  (dired-guess-shell-gnutar . "tar")
  (dired-guess-shell-alist-user . '(("\\.tar\\.gz\\'" "tar ztvf")
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
  :bind (:dired-mode-map
         (" " . dired-toggle-mark)
         ("\C-m" . dired-my-advertised-find-file)
         ("^" . dired-my-up-directory)
         ("r" . wdired-change-to-wdired-mode)
         ))
(leaf wdired :require t)

;; output directory first
(setq insert-directory-program "gls")
(setq dired-listing-switches "-AFl --group-directories-first")

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".DS_Store")))

;;; completion

(leaf vertico
  :ensure t
  :global-minor-mode t
  :custom
  (vertico-count . 20)
  (completion-styles . '(orderless))
  :hook
  ((after-init-hook . savehist-mode))
  :config
  (leaf marginalia
    :ensure t
    :global-minor-mode t)
  (leaf consult
    :ensure t
    :bind
    (("C-s" . consult-line)
     ("C-S-s" . consult-imenu)
     ("C-x C-r" . consult-recentf-file))
    :defvar vertico-map
    :custom
    (consult-find-command . "fd --color=never --full-path ARG OPTS")
    :config
    (define-key vertico-map (kbd "C-r") 'vertico-previous)
    (define-key vertico-map (kbd "C-s") 'vertico-next))
  (leaf embark
    :ensure t
    :bind (("C-." . embark-act)))
  (leaf embark-consult
    :ensure t
    :after (embark consult)
    :leaf-defer nil
    :hook (embark-collect-mode . consult-preview-at-point-mode)))

(setq xref-show-xrefs-function #'consult-xref)

;;; magit

(leaf magit
  :ensure t
  :bind (("C-x m" . magit-status)
         ("C-c l" . magit-blame-addition))
  :init
  (setq-default magit-auto-revert-mode nil))

(leaf magit-delta
  :ensure t
  :after magit
  :hook (magit-mode-hook))

;;; libvterm

(leaf vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :bind
  ("<f2>" . vterm-toggle)
  :custom
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  (vterm-keymap-exceptions . '("<f1>" "<f2>" "<f8>" "C-c" "C-x" "C-g" "C-l" "M-x" "C-v" "M-v" "C-y" "C-t" "C-z")))

(leaf vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-scope . 'project)
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

(leaf projectile
  :ensure t
  :diminish projectile-mode
  :defvar projectile-mode-map
  :custom
  (projectile-project-search-path . '("~/ghq"))
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (leaf counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode t)))

;;; company

(leaf company
  :doc "Modular text completion framework"
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-h" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection)
          ("M-d" . company-show-doc-buffer))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-selection-wrap-arount . t)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode
  :config
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
  (leaf company-quickhelp
    :ensure t
    :custom
    (company-quickhelp-max-lines . 5))
  (company-quickhelp-mode))


;;; flymake

(leaf flymake
  :bind (("M-N" . flymake-goto-next-error)
         ("M-P" . flymake-goto-prev-error)))

(require 'flymake-diagnostic-at-point)
(with-eval-after-load 'flymake
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  (set-face-attribute 'popup-tip-face nil
		      :background "dark slate gray" :foreground "white" :underline nil))
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; flymake-posframe
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
		 :string (concat flymake-diagnostic-at-point-error-prefix
				 (flymake--diag-text
				  (get-char-property (point) 'flymake-diagnostic)))
		 :position (point)
		 :foreground-color "cyan"
		 :internal-border-width 2
		 :internal-border-color "red"
		 :poshandler 'posframe-poshandler-window-bottom-left-corner)
  (dolist (hook flymake-posframe-hide-posframe-hooks)
    (add-hook hook #'flymake-posframe-hide-posframe nil t)))

(advice-add 'flymake-diagnostic-at-point-display-popup :override 'my/flymake-diagnostic-at-point-display-popup)

;;; Snippet

(leaf yasnippet
  :ensure t
  :require t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/mysnippets"))
  :bind (:yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("C-c C-y" . company-yasnippet)
         ("C-x C-i i" . yas-insert-snippet)
         ("C-x C-i n" . yas-new-snippet)
         ("C-x C-i v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1)
  (leaf yasnippet-snippets :ensure t))

;;; Edit

(leaf quickrun
  :ensure t
  :after t)

(leaf anzu
  :doc "replace"
  :ensure t
  :init
  (global-unset-key (kbd "C-t"))
  :custom (globa-anzu-mode . t)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp))

(leaf multiple-cursors
  :ensure t
  :require smartrep
  :bind
  ("C-M-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(leaf mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))

(leaf undo-tree
  :ensure t
  :require t
  :diminish "UT"
  :defvar undo-tree-visualizer-mode-map
  :custom
  (global-undo-tree-mode . t)
  (undo-tree-enable-undo-in-region . nil)
  (undo-tree-history-directory-alist . `(("" . ,(concat user-emacs-directory "undo-tree/"))))
  (undo-tree-visualizer-timestamps . t)
  :config
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit))

(leaf rg
  :ensure t
  :after t)

(leaf eglot
  :ensure t
  :config
 (add-hook 'go-mode-hook 'eglot-ensure)
 (add-hook 'web-mode-hook 'eglot-ensure)
 (add-hook 'rust-mode-hook 'eglot-ensure))

(leaf tree-sitter :ensure t)
(leaf tree-sitter-langs :ensure t)

;;; Variouts mode

(setq major-mode 'text-mode) ;; default mode is text mode

;; Go
(leaf go-mode
  :ensure t
  :defvar c-basic-offset
  :init
  (add-hook 'go-mode-hook #'(lambda ()
                             (setq c-basic-offset 4)
                             (setq indent-tabs-mode t)
                             (local-set-key (kbd "M-.") 'godef-jump)
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                             (local-set-key (kbd "C-c i") 'go-goto-imports)
                             (local-set-key (kbd "C-c d") 'godoc)))
  :custom
  (gofmt-command . "goimports")
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
(leaf web-mode
  :ensure t
  ;; :defvar lsp-enabled-clients
  :defun (sp-local-pair)
  :defvar web-mode-map
  :mode
  "\\.erb\\'"
  "\\.html?\\'"
  "\\.js\\'"
  "\\.jsx\\'"
  "\\.ts\\'"
  "\\.tsx\\'"
  "\\.tpl\\'"
  "\\.tmpl\\'"
  "\\.vue\\'"
  :custom
  (web-mode-attr-indent-offset . nil)
  (web-mode-code-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  (web-mode-enable-auto-indentation . nil)
  (web-mode-enable-auto-quoting . nil)
  (web-mode-enable-current-column-highlight . t)
  (web-mode-enable-current-element-highlight . t)
  (web-mode-markup-indent-offset . 2)
  :config
  (leaf smartparens :config (sp-local-pair 'web-mode "<" ">" :actions nil))
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

;; json
(leaf json-mode
  :ensure t
  :init (add-hook 'json-mode-hook #'(lambda () (make-local-variable 'js-indent-level) (setq js-indent-level 2))))

;; Markdown
(leaf markdown-mode
  :ensure t
  :after t
  :custom
  (markdown-hide-urls . nil)
  :defvar markdown-mode-map
  :hook
  (markdown-mode-hook . (lambda () (display-line-numbers-mode 0)))
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
      markdown-code-lang-modes)))
  (leaf markdown-preview-mode
    :ensure t))

;; scheme
(leaf geiser-gauche :ensure t)

;; elisp
(leaf elisp-mode
  :bind (:emacs-lisp-mode-map
         ("C-M-q" . nil)
         ("C-c C-e" . macrostep-expand))
  :config
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
  (leaf elisp-slime-nav
    :ensure t
    :diminish elisp-slime-nav-mode
    :bind (:elisp-slime-nav-mode-map ("C-c C-d" . helpful-at-point))
    :hook emacs-lisp-mode-hook help-mode-hook)
  (leaf eldoc
    :diminish eldoc-mode
    :hook emacs-lisp-mode-hook ielm-mode-hook)
  (leaf ielm
    :bind (:ielm-map
           ("C-c C-d" . helpful-at-point)))
  (leaf macrostep :ensure t)
  (leaf simple
    :bind (:read-expression-map
           ("<tab>" . completion-at-point))))

;; Haskell
(leaf haskell-mode
  :config
  (leaf hindent
    :ensure t)
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; python
(leaf python-mode
  :config
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq indent-tabs-mode nil)))))

;; c/c++
(leaf cc-mode
  :hook
  ((c-mode-hook . (lambda () (local-unset-key (kbd "C-c C-b"))))
   (c-mode-hook . (lambda () (setq comment-start "//"
                                   comment-end   "")))
   (c++-mode-hook . (lambda () (local-unset-key (kbd "C-c C-b"))))
   (c++-mode-hook . (lambda () (setq comment-start "//"
                                     comment-end   ""))))
  :config
  (leaf clang-format
    :ensure t
    :init
    (defun set-hook-after-save-clang-format ()
      (add-hook 'after-save-hook 'clang-format-buffer t t))
    :hook ((c-mode-hook . set-hook-after-save-clang-format)
           (c++-mode-hook . set-hook-after-save-clang-format))
    :bind ((:c-mode-map ([remap indent-whole-buffer] . clang-format-buffer))
           (:c++-mode-map ([remap indent-whole-buffer] . clang-format-buffer)))))

;; rust
(leaf rustic
  :ensure t
  :mode "\\.rs$"
  :custom
  (rustic-format-display-method . 'ignore)
  (rustic-lsp-client . 'eglot)
  :config
  (defun my/find-rust-project-root (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (list 'vc 'Git root)))
  (defun my/rust-mode-hook ()
    (setq-local project-find-functions (list #'my/find-rust-project-root)))
  (add-hook 'rust-mode-hook #'my/rust-mode-hook))

(leaf css-mode :ensure t)
(leaf csv-mode :ensure t)
(leaf toml-mode :ensure t)
(leaf dockerfile-mode :ensure t)
(leaf docker-compose-mode :ensure t)
(leaf dotenv-mode :ensure t :mode "\\.env\\..*\\'")
(leaf envrc :ensure t)
(leaf kotlin-mode :ensure t)
(leaf nginx-mode :ensure t)
(leaf protobuf-mode :ensure t)
(leaf terraform-mode :ensure t)
(leaf yaml-mode :ensure t)

(leaf sh-script
  :custom (sh-basic-offset . 2)
  :defvar sh-shell
  :config
  (leaf sh :mode "\\.zsh$"))

(leaf rfc-mode
  :require t
  :custom
  (rfc-mode-directory . "~/Documents/rfc"))

(leaf pdf-tools
  :ensure t
  :bind ((pdf-view-mode-map
          ("C-s" . isearch-forward)))
  :custom
  (pdf-annot-activate-created-annotations . t)
  (pdf-view-resize-factor . 1.1)
  :config
  (pdf-tools-install))

;; copilot

(leaf copilot
  :el-get (copilot
           :type github
           :pkgname "zerolfx/copilot.el"
           )
  :bind
  ((copilot-mode-map
    ("<tab>" .  copilot-accept-completion)
    ("TAB" .  copilot-accept-completion)))
  )

;; editorconfig

(leaf editorconfig
  :ensure t
  :custom
  (editorconfig-get-properties-function . 'editorconfig-core-get-properties-hash)
  :config
  (editorconfig-mode 1))

;;; elfeed

(leaf elfeed
  :ensure t
  :bind
  ("C-x w" . elfeed)
  :defvar '(elfeed-feeds elfeed-search-filter elfeed-show-mode-hook elfeed-show-entry-switch)
  :defun my-show-elfeed
  :setq
  (elfeed-feeds . '(("https://planet.emacslife.com/atom.xml" emacs)
                    ("https://sachachua.com/blog/feed/" emacs)
                    ("https://protesilaos.com/codelog.xml" emacs)
                    ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" youtube awesomekling)))
  (elfeed-search-filter . "@3-days-ago +unread")
  :config
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
	   (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Iosevka" :size 13))
	   (setq fill-column 120)
	   (setq elfeed-show-entry-switch #'my-show-elfeed))))

;;; shortdoc

(leaf shortdoc
  :config
  (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Iosevka" :size 13)))

;;; Org

(leaf org
  :ensure t
  :defvar '(org-default-notes-file org-agenda-files)
  :defun '(yank-with-indent copy-region-unindented)
  :custom ((org-return-follows-link . t)
           (org-startup-folded . t)
           (org-startup-truncated . nil)
           (org-log-done . 'time)
           (org-hide-leading-stars . t)
           (org-edit-src-content-indentation . 0)
           (org-src-preserve-indentation . nil)
           (org-todo-keywords . '((sequence "TODO(t)" "FOCUS(f)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
           (org-todo-keyword-faces . '(("FOCUS"    :foreground "#FF0000" :background "#FFCC66")
                                       ("WAIT"     :foreground "#CCCCCC" :background "#666666")))
           (org-appear-autolinks . t)
           (org-blank-before-new-entry . '((heading . always) (plain-list-item . nil)))
           (org-cycle-separator-lines . 1))
  :bind
  ((org-mode-map
    ("C-h" . delete-backward-char)
    ("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    ("C-c y" . yank-with-indent)
    ("C-c M-w" . copy-region-unindented)))
  :hook
  (org-mode-hook . org-appear-mode)
  (org-mode-hook . (lambda () (display-line-numbers-mode 0)))
  :config
  (setq org-default-notes-file (concat (getenv "ORGSYNCROOT") "/org/journal.org"))
  (setq org-agenda-files (list (concat (getenv "ORGSYNCROOT") "/org/")))
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

(leaf ox
  :ensure nil
  :config
  (setq org-export-with-timestamps nil)  ;; heading timestamp
  (leaf ox-md :ensure nil :require t))

(leaf org-capture
  :defvar '(task-file nippou-file idea-file tweet-file org-capture-templates)
  :config
  (defun org-get-target-headline (&optional targets prompt)
    (let ((org-refile-targets (or targets org-refile-targets))
          (prompt (or prompt "Capture Location")))
      (org-refile t nil nil prompt)))
  (setq code-file (concat (getenv "ORGSYNCROOT") "/org/code.org"))
  (setq idea-file (concat (getenv "ORGSYNCROOT") "/org/idea.org"))
  (setq memo-file (concat (getenv "ORGSYNCROOT") "/org/memo.org"))
  (setq tweet-file (concat (getenv "ORGSYNCROOT") "/org/tweet.org"))
  (setq watch-file (concat (getenv "ORGSYNCROOT") "/org/watch.org"))
  (setq compp-file (concat (getenv "ORGSYNCROOT") "/org/compp.org"))
  (setq org-capture-templates
      `(("c" "Code" plain
         (file+function code-file org-get-target-headline)
         "%?\n%(with-current-buffer (org-capture-get :original-buffer) (browse-at-remote-get-url))\n# %(with-current-buffer (org-capture-get :original-buffer) (file-full-path))\n\n%i\n"
         :empty-lines 1)
        ("i" "Idea" entry
         (file+headline idea-file "Project")
         "** %?\n"
         :empty-lines 1)
        ("m" "Memo" entry
         (file+headline memo-file "Memo")
         "** %?\n"
         :empty-lines 1)
        ("t" "Tweet" entry
         (file tweet-file)
         "* %? %U %^g"
         :empty-lines 1)
        ("p" "Compe P" entry
         (file compp-file)
         "* %? %U"
         :empty-lines 1))))

;;; Utility

(leaf google-this :ensure t)
(leaf which-key :ensure t :custom (which-key-mode . 1))
(leaf germanium :ensure t :custom (germanium-check-options-each-execute-command . nil))
(leaf consult-ghq
  :ensure t
  :config
  (leaf consult :ensure t)
  (leaf affe
    :ensure t
    :config
    (leaf orderless :ensure t)
    :custom
    (affe-find-command . "fd --color=never --full-path --hidden --exclude .git")
    (affe-highlight-function . 'orderless-highlight-matches)
    (affe-regexp-function . 'orderless-pattern-compiler)))

(leaf browse-at-remote
  :ensure t
  :require t
  :defun (browse-at-remote-get-url)
  :custom
  ((browse-at-remote-prefer-symbolic . nil))
  :bind
  ("C-C b" . browse-at-remote)
  ("C-c C-b" . bar-to-clipboard))

(leaf back-button-mode
  :config
  (back-button-mode 1))

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

(setq comment-empty-lines t)
(setq global-display-line-numbers-mode nil)

;;; Key bindings

(leaf *global-set-key
  :leaf-autoload nil
  :bind
  ("C-h" . delete-backward-char)
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
  ("C-c r" . org-capture)
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

  ("C-\\" . nil)
  :config
  (keyboard-translate ?\C-h ?\C-?))

(setq file-name-handler-alist my/saved-file-name-handler-alist)

;; (profiler-report)
;; (profiler-stop)

(provide 'init)
;;; init.el ends here
