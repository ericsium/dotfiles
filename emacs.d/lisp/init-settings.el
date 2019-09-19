
;; disable welcome screen
(setf inhibit-startup-screen t)

;; disable alarm bell
(setf ring-bell-function 'ignore)

;; Use visible bell
(setq visible-bell 1)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Smarter find-file - open file at point
(ffap-bindings)

;; Disable Toolbar
(tool-bar-mode -1)

;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; disable menu bar in CLI
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; When opening shell open in current buffer, not other window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; Keybindings
(global-set-key [(control ?c)(?r)] 'revert-buffer)
(global-set-key [(control ?c)(control ?r)] 'rename-buffer)
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "C-'") 'switch-to-other-buffer)
(global-set-key [(meta ?g)] 'goto-line)
(global-set-key [(meta ?s)(meta ?s)] 'shell)
;;(global-set-key [(meta mouse-2)] 'ffap-at-mouse)

;; By default truncate lines for buffers
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; cperl-mode is preferred to perl-mode                                        
;; "Brevity is the soul of wit" <foo at acm.org>                               
(defalias 'perl-mode 'cperl-mode)

;; Allow files upto 100MB without warning
(setq large-file-warning-threshold 100000000)

;; Don't fontify larger buffers
(setq font-lock-maximum-size 1000000)

;; Make _ a valid word character
(modify-syntax-entry ?_ "w")

;; improve scrolling
(setf scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(show-paren-mode 1) ; highlight matching parens

;;
;; XV coding guidelines
;;
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Minor tweaks to standard indentation 
;; Line up expression on new lines under opening paren
(c-set-offset 'arglist-cont '(c-lineup-arglist-operators 0))
(c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-operators c-lineup-arglist))
 
;; Line up close paren with open paren
(c-set-offset 'arglist-close '(c-lineup-arglist-close-under-paren))
(setq ruby-deep-indent-paren nil)

;; My additions - need to be added to XV coding guide
(defun my-c-setup ()
   (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'my-c-setup)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq auto-mode-alist (cons '("\\.tcl$" . tcl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zebu$" . tcl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.qel$" . tcl-mode) auto-mode-alist))
;; .itcl means itcl, so use tcl mode
(setq tcl-indent-level 2)


(when (<= emacs-major-version 25)
;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
;; Fix issue with lambda indent
;; emacs 25
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))                   ; default behavior
  )

(when (> emacs-major-version 26)
  ;;emacs 26
  (setq c-offsets-alist '(
                          (inlambda . 0) ; no extra indent for lambda
                          ))
  )

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(provide `init-settings)
