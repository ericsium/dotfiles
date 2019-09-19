;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

;; Misc personal settings
(require 'init-settings)

(require 'init-theme)

;; nice shell-mode setup
(require 'init-shell)

;; Stuff set through emacs customize interface (see lisp/custom.el)
(require 'init-custom)
