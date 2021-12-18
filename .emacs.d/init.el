;;; init.el --- init.el -*- lexical-binding: t -*-

;; Author: matsuyoshi30

;;; Commentary:

;; This program is Emacs init.el

;;; Code:

;;; General

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
      (leaf diminish :ensure t)
      (leaf-keywords-init))))

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
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :bind (("M-ESC ESC" . c/redraw-frame))
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
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom
  `((inhibit-startup-screen . 0)))

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
   (recentf-exclude . '("\\.elc$" "\\.o$" "~$" "\\.undo-tree/" "PATH")))
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

;;; Path

(autoload 'exec-path-from-shell "exec-path-from-shell" nil t)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

;;; Display

(leaf modus-themes
  :ensure t
  :bind ("<f5>" . modus-themes-toggle)
  :custom
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . nil)
  (modus-themes-region . 'bg-only)
  (modus-themes-diffs . 'deuteranopia)
  (modus-themes-org-blocks . 'gray-background)
  (modus-themes-syntax . 'faint)
  (modus-themes-paren-match . 'intense-bold)
  (modus-themes-hl-line . 'accented)
  :config
  (modus-themes-load-themes))
; (modus-themes-load-operandi)
(modus-themes-load-vivendi)

(setq default-frame-alist
      (append (list
              '(font . "HackGenNerd-13"))
              default-frame-alist))

;;; frame

(when (eq window-system 'mac) ;; for EMP
  (setq initial-frame-alist
        (append
         '((top . 22)
           (left . 55)
           (width . 170)
           (height . 65)
           (vertical-scroll-bar . nil)
           ) initial-frame-alist))
  (setq frame-title-format (format (if (buffer-file-name) "%%f" "%%b"))))

(column-number-mode t)
(global-hl-line-mode t)
(line-number-mode t)

(setq display-time-day-and-date t)
(defvar display-time-string-forms
  '(month "/" day " " dayname " "
          24-hours ":" minutes " "))
(display-time-mode t)
(display-battery-mode t)

(defvar delete-trailing-whitespace-before-save t)
(defun my-delete-trailing-whitespace ()
  "Delete trailing whitespaces before save file."
  (if delete-trailing-whitespace-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
(add-hook 'markdown-mode-hook
          '(lambda ()
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
  (all-the-icons-scale-factor . 1.0)
  :config
  (leaf all-the-icons-ivy
    :require t
    :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
  (leaf all-the-icons-dired
    :require t
    :diminish all-the-icons-dired-mode))

;; smart mode line
(defvar sml/active-background-color "gray60")
(defvar sml/modified-char "*")
(defvar sml/no-confirm-load-theme t)
(defvar sml/theme 'dark)
(defvar sml/shorten-directory -1)
(sml/setup)

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

;; (leaf whitespace
;;   :require t
;;   :custom
;;   (global-whitespace-mode . 1)
;;   (whitespace-style . '(face tabs))
;;   :custom-face
;;   (whitespace-tab . '((t (:foreground "#0C2B33"))))
;;   )

(leaf elscreen
  :init
  (custom-set-variables
   '(elscreen-prefix-key (kbd "C-z"))
   '(elscreen-display-tab nil)
   '(elscreen-tab-display-kill-screen nil)
   '(elscreen-tab-display-control nil)
   '(elscreen-buffer-to-nickname-alist
     '(("^dired-mode$" .
        (lambda ()
          (format "Dired(%s)" dired-directory)))
       ("^Info-mode$" .
        (lambda ()
          (format "Info(%s)" (file-name-nondirectory Info-current-file))))
       ))
   '(elscreen-mode-to-nickname-alist
     '(("[Ss]hell" . "shell")
       ("compilation" . "compile")))
   )
  (elscreen-start))

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

(when (eq window-system 'mac) ; for EMP
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

;;; magit

(leaf magit
  :ensure t
  :bind (("C-x m" . magit-status)
         ("C-c l" . magit-blame-addition))
  :init
  (setq-default magit-auto-revert-mode nil))


;;; libvterm

(leaf vterm
  ;; requirements: brew install cmake libvterm libtool
  :ensure t
  :bind
  ("<f2>" . vterm-toggle)
  :custom
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  (vterm-keymap-exceptions . '("<f1>" "<f2>" "C-c" "C-x" "C-g" "C-l" "M-x" "C-v" "M-v" "C-y" "C-t" "C-z")))

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
  (projectile-project-search-path . '("~/projects"))
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (leaf counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode t)))

;;; flycheck

(leaf flycheck
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :url "http://www.flycheck.org"
  :ensure t
  :diminish flycheck-mode
  :bind (("M-N" . flycheck-next-error)
         ("M-P" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;;; company

(leaf company
  :doc "Modular text completion framework"
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
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
                      :foreground "red"))

(leaf auto-complete
  :ensure t
  :after t
  :require auto-complete-config
  :diminish auto-complete-mode
  :defun ac-config-default ac-set-trigger-key
  :custom
  (ac-auto-show-menu . 0.4)
  (ac-auto-start . nil)
  (ac-menu-height . 22)
  (ac-quick-help-delay . 0.4)
  (ac-use-quick-help . t)
  :bind
  (:ac-completing-map
   ("M-n" . ac-next)
   ("M-t" . ac-previous)
   ("RET" . nil))
  :config
  (ac-config-default)
  (ac-set-trigger-key "<C-tab>"))

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
  ("C-M-c" . mc/edit-lines))

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

(leaf lsp-mode
  :ensure t
  :after t
  :defvar lsp-command-map
  :init
  (defun lsp-format-before-save ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  :bind (:lsp-mode-map
         ("C-S-SPC" . nil))
  :config
  (define-key lsp-mode-map (kbd "M-z") lsp-command-map)
  (leaf lsp-ui
    :ensure t
    :custom
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-position . 'top)
    (lsp-ui-sideline-enable . nil)
    :bind (:lsp-ui-mode-map
           ("C-c C-d" . lsp-ui-doc-show)
           ("C->" . lsp-find-type-definition)
           ("C-c C-p" . lsp-ui-peek-find-implementation))))

;;; Variouts mode

(setq major-mode 'text-mode) ;; default mode is text mode

;; Go
(leaf go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook '(lambda ()
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
  (add-hook 'go-mode-hook 'lsp-deferred))

;; web
(leaf web-mode
  :ensure t
  ;; :defvar lsp-enabled-clients
  :defun sp-local-pair
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
  :init
  ;; (defun web-mode-setup ()
  ;;   (setq-local lsp-enabled-clients '(ts-ls eslint))
  ;;   (lsp))
  (defun setup-tide-mode ()
	  (interactive)
	  (tide-setup)
	  (flycheck-mode +1)
	  (defvar flycheck-check-syntax-automatically '(save mode-enabled))
	  (eldoc-mode +1)
	  (tide-hl-identifier-mode +1)
	  (company-mode +1))
  ;; :hook (web-mode-hook . web-mode-setup)
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
  (add-hook 'web-mode-hook
            '(lambda ()
               (when (string-equal "js" (file-name-extension buffer-file-name))
                 (setup-tide-mode))
               (when (string-equal "ts" (file-name-extension buffer-file-name))
                 (setup-tide-mode))
               ))
  (local-set-key (kbd "RET") 'newline-and-indent))

;; json
(leaf json-mode
  :ensure t
  :init (add-hook 'json-mode-hook '(lambda () (make-local-variable 'js-indent-level) (setq js-indent-level 2))))

;; Markdown
(leaf markdown-mode
  :ensure t
  :after t
  :custom
  (markdown-hide-urls . nil)
  :defvar markdown-mode-map
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

;; elisp
(leaf elisp-mode
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  :bind (:emacs-lisp-mode-map
         ("C-M-q" . nil)
         ("C-c C-e" . macrostep-expand))
  :config
  (leaf elisp-slime-nav
    :ensure t
    :diminish elisp-slime-nav-mode
    :bind (:elisp-slime-nav-mode-map ("C-c C-d" . helpful-at-point))
    :hook emacs-lisp-mode-hook help-mode-hook)
  (leaf eldoc
    :diminish eldoc-mode
    :hook emacs-lisp-mode-hook ielm-mode-hook)
  (leaf flycheck-package
    :ensure t
    :after flycheck
    :defun flycheck-package-setup
    :config (flycheck-package-setup))
  (leaf ielm
    :bind (:ielm-map
           ("C-c C-d" . helpful-at-point)))
  (leaf macrostep :ensure t)
  (leaf simple
    :bind (:read-expression-map
           ("<tab>" . completion-at-point))))

;; python
(leaf python-mode
  :config
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq indent-tabs-mode nil)))))

;; c/c++
(leaf cc-mode
  :hook
  ((c-mode-hook . lsp)
   (c++-mode-hook . lsp))
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
  (rustic-format-trigger . 'on-save)
  :after flycheck
  :defvar flycheck-checkers
  :config
  (push 'rustic-clippy flycheck-checkers))

(leaf css-mode :ensure t)
(leaf csv-mode :ensure t)
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
  :require t
  :custom
  (pdf-annot-activate-created-annotations . t)
  (pdf-view-resize-factor . 1.1)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook
            (lambda nil
              (linum-mode -1))))

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
	   (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "HackGenNerd" :size 13))
	   (setq fill-column 120)
	   (setq elfeed-show-entry-switch #'my-show-elfeed))))

;;; Org

(leaf org
  :ensure t
  :custom ((org-return-follows-link . t)
           (org-startup-folded . t)
           (org-startup-truncated . nil)
           (org-log-done . 'time)
           (org-hide-leading-stars . t)
           (org-edit-src-content-indentation . 0)
           (org-src-preserve-indentation . nil)
           (org-todo-keywords . '((sequence "TODO(t)" "FOCUS(f)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
           (org-todo-keyword-faces . '(("FOCUS"    :foreground "#FF0000" :background "#FFCC66")
                                       ("WAIT"     :foreground "#CCCCCC" :background "#666666"))))
  :bind
  ("C-c l" . org-store-link))

(leaf org2blog
  :ensure t
  :custom
  (org2blog/wp-blog-alist . '(("blog"
                               :url "https://diary.matsuyoshi30.net/xmlrpc.php"
                               :username "matsuyoshi30"))))

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
    (affe-find-command . "fd --color=never --full-path")
    (affe-highlight-function . 'orderless-highlight-matches)
    (affe-regexp-function . 'orderless-pattern-compiler)))

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

;;; Key bindings

(leaf *global-set-key
  :leaf-autoload nil
  :bind
  ("C-h" . delete-backward-char)
  ("C-j" . newline)
  ("C-c a" . align)
  ("C-c M-a" . align-regexp)
  ("C-x F" . toggle-frame-maximized)
  ("C-x ?" . help-command)
  ("C-c C-j" . rg)
  ("C-c c" . quickrun-with-arg)
  ("C-t" . other-window-or-split)
  ("C-c C-v" . open-by-vscode)
  ("C-c '" . google-this)
  ("C-c w" . org2blog-user-interface)
  ("C-c t" . toggle-truncate-lines)

  ("C-c g" . affe-grep)

  ("C-]" . consult-ghq-find)

  ("M-n" . "C-u 5 C-n")
  ("M-p" . "C-u 5 C-p")

  ("M-SPC" . expand-abbrev)
  ("<f3>" . highlight-symbol-at-point)
  ("M-<f3>" . highlight-symbol-remove-all)

  ("C-x M-g" . germanium-buffer-to-png)
  ("C-x M-q" . germanium-region-to-png))

(provide 'init)
;;; init.el ends here
