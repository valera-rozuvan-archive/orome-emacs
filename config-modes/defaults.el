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
        c++-mode-hook
        c-mode-hook

        emacs-lisp-mode-hook
        text-mode-hook
        fundamental-mode-hook

        python-mode-hook

        js-mode-hook
        javascript-mode-hook
        js2-mode-hook
        coffee-mode-hook

        typescript-mode-hook

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Taken from:
;;
;;   How to get auto indent (not smart indent) in emacs in all modes
;;   http://stackoverflow.com/questions/18945380/
;;

(defun plain-tab ()
  (interactive)
  (insert (make-string tab-width ?\ )))
(defun plain-ret ()
  (interactive)
  (looking-back "^\\( +\\).*")
  (newline)
  (insert (match-string 1)))
(defun plain-del ()
  (interactive)
  (backward-delete-char
   (if (looking-back (format " \\{%d\\}" tab-width)) tab-width 1)))
(defvar all-the-mode-maps
  '(
    c++-mode-map
    c-mode-map

    emacs-lisp-mode-map
    text-mode-map
    ;; fundamental-mode-map

    ;; python-mode-map

    js-mode-map
    ;; javascript-mode-map
    js2-mode-map
    ;; coffee-mode-map

    typescript-mode-map

    ;; sass-mode-map
    less-css-mode-map
    css-mode-map

    yaml-mode-map

    ;; markdown-mode-map

    ;; shell-script-mode-map
    ;; sh-mode-map
  )
)
(eval `(mapc
        (lambda(map)
          (define-key map [tab] 'plain-tab)
          (define-key map [return] 'plain-ret)
          (define-key map [backspace] 'plain-del)
          (define-key map "{" (lambda()(interactive)(insert "{")))
          (define-key map "}" (lambda()(interactive)(insert "}"))))
        (list ,@all-the-mode-maps)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
