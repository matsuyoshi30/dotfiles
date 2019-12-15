(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"))
(define-key yas-minor-mode-map (kbd "C-x C-i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x C-i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x C-i v") 'yas-visit-snippet-file)
(yas-global-mode 1)
