(add-to-list 'load-path "~/.emacs.d/vendor/load-directory-mu")
(require 'load-directory-mu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(desktop-save-mode t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-directory-mu "~/.emacs.d/config-editor")
(load-directory-mu "~/.emacs.d/config")
