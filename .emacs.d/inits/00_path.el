;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec-pathの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます

(autoload 'exec-path-from-shell "exec-path-from-shell" nil t)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))
