;; Pull in ./lisp/*
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

;; Misc personal settings
(require 'init-settings)

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
;;     (if python-version-directory
;;         (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
;;                (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;           (pyenv-mode-set pyenv-current-version)
;;           (message (concat "Setting virtualenv to " pyenv-current-version))))))


;; (defvar pyenv-current-version nil nil)

;; (defun pyenv-init()
;;   "Initialize pyenv's current version to the global one."
;;   (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
;;     (message (concat "Setting pyenv version to " global-pyenv))
;;     (pyenv-mode-set global-pyenv)
;;     (setq pyenv-current-version global-pyenv)))

;; (add-hook 'after-init-hook 'pyenv-init)
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
;;  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (add-hook 'python-mode-hook 'workon-auto)
  (add-hook 'hy-mode-hook 'workon-auto)
  ;; end pyvenv config
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  (pyvenv-tracking-mode 1) ;; pyvenv config
  )

;; (use-package pyenv-mode
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode))
;;   ;; :bind
;;   ;; ("C-x p e" . pyenv-activate-current-project))

;; (use-package python
;;   :mode ("\\.py" . python-mode)
;;   :config
;;   (setq python-indent-offset 4)
;;   (elpy-enable))

;; (use-package company-quickhelp
;;   :ensure t
;;   :init
;;   (company-quickhelp-mode))

;; (use-package pyenv-mode-auto
;;   :ensure t)

(require 'init-theme)

;; nice shell-mode setup
(require 'init-shell)

;; Stuff set through emacs customize interface (see lisp/custom.el)
(require 'init-custom)
