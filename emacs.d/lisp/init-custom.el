;; Don't allow custom to stick stuff at the bottom of init.el
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file 'noerror)

(provide 'init-custom)
