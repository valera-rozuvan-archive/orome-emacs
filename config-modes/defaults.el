;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Turn indenting off on TAB and RETURN key press for all modes that we use.
;;

(defun no-indentation-for-tab ()
    (interactive)
    (local-set-key (kbd "<tab>") 'tab-to-tab-stop)
)
(defun set-newline-for-return ()
    (interactive)

    ;; (local-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
    (local-set-key (kbd "RET") 'newline)

    ;; (global-set-key "\em" 'newline) ;; for emacs 23
    ;; (global-set-key "\em" 'electric-newline-and-maybe-indent) ;; for emacs 24
)
(defun reset-indent-function ()
    (interactive)

    (setq indent-line-function (quote tab-to-tab-stop))
)
(dolist (
    hook '(
        emacs-lisp-mode-hook
        text-mode-hook
        fundamental-mode-hook

        python-mode-hook

        js-mode-hook
        javascript-mode-hook
        js2-mode-hook
        coffee-mode-hook

        sass-mode-hook
        less-css-mode-hook
        css-mode-hook

        yaml-mode-hook

        markdown-mode-hook

        shell-script-mode-hook
        sh-mode-hook
    ))
    (add-hook hook 'no-indentation-for-tab)
    (add-hook hook 'set-newline-for-return)
    (add-hook hook 'reset-indent-function)
)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
