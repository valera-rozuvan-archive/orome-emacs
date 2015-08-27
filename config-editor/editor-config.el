;; Configure the theme and the font size.
;; (load-theme 'deeper-blue t)
(load-theme 'adwaita t)

(add-to-list 'default-frame-alist '(height . 26))
(add-to-list 'default-frame-alist '(width . 121))


;; Font. Make sure that you have the "Inconsolata" font installed.
;; On Debian-like distros you can do:
;;
;;     sudo apt-get install ttf-inconsolata
;;
(set-default-font "Inconsolata-14")


;; Make sure that the current position (line and column number) of the
;; cursor is shown.
(setq line-number-mode t)
(setq column-number-mode t)


;; Disable the start-up splash screen.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Disabling the Menu Bar, and Toolbar
;;

(menu-bar-mode -1)
(tool-bar-mode -1)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Disable auto-save and auto-backup files.
;;

(setq auto-save-default nil)
(setq make-backup-files nil)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Store all backup and autosave files in the tmp dir.
;;

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory))
)
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t))
)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; On save, remove trailing whitespace from all lines.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Always replace tabs with spaces.
;;

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set tab width to 2 for all buffers.
;;

(setq-default tab-width 2)
(setq tab-width 2)

(setq-default c-basic-offset 2)
(setq c-basic-offset 2)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The default tab stops are 8 spaces appart.
;; Using some manual editing, we change this list.
;; Tabs will be 2, 4, 6, 8, 10, ..., 118, 120 spaces apart.

(setq tab-stop-list (number-sequence 2 120 2))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable the display of date and time in mode line.
;;

(setq
    display-time-day-and-date t
    display-time-24hr-format t
)
(display-time-mode t)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; http://www.emacswiki.org/emacs/DiredFindFileOtherFrame
;; Here’s a small function to open files in new frame from dired. This was
;; requested on the EmacsChannel.
;;

(defun dired-find-file-other-frame ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit))
)

(eval-after-load "dired"
  '(define-key dired-mode-map "F" 'dired-find-file-other-frame)
)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Open a new frame with a *scratch* buffer. To be used as a temporary scratch
;; pad.
;;

(defun new-frame-with-scratch ()
  "Open a new frame with scratch buffer selected"
  (interactive)
  (let ((frame (make-frame))
        (scratch-name "*temp*"))
    (select-frame-set-input-focus frame)
    (unless (get-buffer scratch-name)
      (with-current-buffer (get-buffer-create scratch-name)
        (text-mode)))
    (switch-to-buffer scratch-name 'norecord)))

(global-set-key (kbd "C-9") 'new-frame-with-scratch)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I want to disable electric indent mode for all major modes.
;;

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; find-func.el --- find the definition of the Emacs Lisp function near point.
;;

(require 'find-func)

;; Define some shortcut key bindings. Specifically `Ctrl-x + F` (note the
;; capital "F").
(find-function-setup-keys)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; http://stackoverflow.com/questions/1836925/emacs-find-name-dired-how-to-change-default-directory
;; Regarding case insensitivity, you can customize the variable
;; find-name-arg to be the case insensitive version:
;;

(setq find-name-arg "-iname")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage collector configuration.
;;
;; By default Emacs will initiate GC every 0.76 MB allocated
;; (gc-cons-threshold == 800000). If we increase this to 20 MB
;; (gc-cons-threshold == 20000000) we get a better result.
;;

(setq gc-cons-threshold 20000000)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
