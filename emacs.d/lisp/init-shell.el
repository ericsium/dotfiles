;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Shell configurations.
;;

;;; Code:

(use-package shell
   :ensure nil
   :commands comint-send-string comint-simple-send comint-strip-ctrl-m
   :preface
  (defun n-shell-simple-send (proc command)
    "Various PROC COMMANDs pre-processing before sending to shell."
    (cond
     ;; Checking for clear command and execute it.
     ((string-match "^[ \t]*clear[ \t]*$" command)
      (comint-send-string proc "\n")
      (erase-buffer))
     ;; Checking for man command and execute it.
     ((string-match "^[ \t]*man[ \t]*" command)
      (comint-send-string proc "\n")
      (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
      (setq command (replace-regexp-in-string "[ \t]+$" "" command))
      ;;(message (format "command %s command" command))
      (funcall 'man command))
     ;; Send other commands to the default handler.
     (t (comint-simple-send proc command))))
  (defun n-shell-mode-hook ()
    "Shell mode customizations."
    (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
    (setq comint-input-sender 'n-shell-simple-send))
  :hook ((shell-mode . ansi-color-for-comint-mode-on)
         (shell-mode . n-shell-mode-hook))
  :config
  (setq system-uses-terminfo nil)       ; don't use system term info

  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

  ;; ANSI & XTERM 256 color support
  (use-package xterm-color
    :init
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))

    (add-hook 'shell-mode-hook
              (lambda ()
                ;; Disable font-locking in this buffer to improve performance
                (font-lock-mode -1)
                ;; Prevent font-locking from being re-enabled in this buffer
                (make-local-variable 'font-lock-function)
                (setq font-lock-function (lambda (_) nil))
                (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

  (use-package dirswitch
    :load-path "vendor/dirswitch"
    )

  (defun enable-dirswitch ()
    (dirtrack-mode 1)
    ;;    (setq dirtrack-list '("^[^:\\n]+@[^:\\n]+:\\(.+\\)[$#]" 1))
    ;; Use re-builder to test this out
    (setq dirtrack-list '("\\[.*\n\\(.*\\)\n[[:alpha:]]+@[^\]]+]\\$" 1))
    (shell-dirtrack-mode -1)
    (dirswitch-mode 1))
  
  (add-hook 'shell-mode-hook 'enable-dirswitch)
  )


(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
