;; ---------------------------------
;; packages
;; ---------------------------------
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("erlang-emacs" . "http://xn--5dbqm8ad.xn--9dbdkw.se/erlang-emacs/")))


(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun add-to-load (path)
  (add-to-list 'load-path (concat "~/.emacs.d/vendor/" path)))

(mapc 'install-if-needed install-packages)
(mapc 'add-to-load local-packages)


;; ---------------------------------
;; full path to opened file
;; ---------------------------------
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; ---------------------------------
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; right C-a button
;; ---------------------------------
(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;; ---------------------------------
;; create the autosave/backup dir if necessary, since emacs won't
;; and set autosave/backup directories
;; ---------------------------------
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)

(custom-set-variables
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))))


;; ---------------------------------
;; enable lower/upper-region
;; ---------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ---------------------------------
;; common hooks
;; ---------------------------------
;; delete whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; ---------------------------------
;; common settings
;; ---------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(auto-window-vscroll nil t)
 '(desktop-save-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(indent-tabs-mode nil)
 '(iswitchb-mode t)
 '(icomplete-mode t)
 '(show-trailing-whitespace t)
 '(ring-bell-function nil t)
 '(scroll-step 1)
 '(mouse-wheel-scroll-amount (quote (1)))
 '(require-final-newline t)
 '(scroll-conservatively 10000)
 '(tabbar-separator (quote (0.5))))

(provide 'common)
