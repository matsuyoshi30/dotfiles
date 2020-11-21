(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
