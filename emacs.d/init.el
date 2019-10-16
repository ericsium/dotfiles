;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

;; Misc personal settings
(require 'init-settings)

(use-package elpy
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  ;; pyvenv config for pyenv
  (defun workon-auto()
    (setq-local pyvenv-workon (string-trim (shell-command-to-string "pyenv version-name")))
    (pyvenv-workon pyvenv-workon)
    )
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (add-hook 'python-mode-hook 'workon-auto)
  (add-hook 'hy-mode-hook 'workon-auto)
  ;; end pyvenv config
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  (pyvenv-tracking-mode 1) ;; pyvenv config
  )

(require 'init-theme)

;; nice shell-mode setup
(require 'init-shell)

;; Stuff set through emacs customize interface (see lisp/custom.el)
(require 'init-custom)
