;; packages settings
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun add-to-load (path)
  (add-to-list 'load-path (concat "~/.emacs.d/vendor/" path)))

;; load packages
(mapc 'install-if-needed '(haml-mode markdown-mode jinja2-mode
                           coffee-mode clojure-mode pony-mode
                           multiple-cursors workspaces monokai-theme
                           writegood-mode dash-at-point))
;; load local packages
(mapc 'add-to-load '("" "git-emacs" "mercurial" "clevercss-mode"
                     "unittest-mode"))

;; require packages
; markup
(require 'haml-mode)
(require 'markdown-mode)
(require 'jinja2-mode)
(require 'clevercss)
; langs
(require 'clojure-mode)
(require 'coffee-mode)
; python modes
(require 'pony-mode)
(require 'unittest)
; dvcs
(require 'git-emacs)
(require 'mercurial)
; theme
(require 'monokai-theme)
; etc
(require 'writegood-mode)
(require 'multiple-cursors)
(require 'dash-at-point)
; libraries
(load-library "workspaces.el")
(load "window-resize.el")

;; full path to opened file
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; font
(set-face-attribute 'default nil :font "Menlo Regular-11")

;; dash
(global-set-key (kbd "C-c d") 'dash-at-point)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; writegood-mode
(global-set-key (kbd "C-c C-g") 'writegood-mode)

;; workspaces
(global-set-key (kbd "C-x g") 'workspace-goto)

;; clevercss mode
(add-to-list 'auto-mode-alist '("\\.ccss$" . clevercss-mode))

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffeescript mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; coffee-mode-hook
(add-hook 'coffee-mode-hook
          (lambda ()
            (make-local-variable 'tab-width)
            (set 'tab-width 2)
            ;; If you don't want your compiled files to be wrapped
            (setq coffee-args-compile '("-c" "--bare"))
            ;; Emacs key binding
            (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
            ;; Compile '.coffee' files on every save
            (and (file-exists-p (buffer-file-name))
                       (file-exists-p (coffee-compiled-file-name))
                       (coffee-cos-mode t))))

;; html-mode-hook
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))

;; python-mode
(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")
(epy-setup-checker "~/.emacs.d/pycheker.sh %f")
(require 'ac-python)

;; keys
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(default-tab-width 4 t)
 '(desktop-save-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil)
 '(unittest-last-executed-module "tests")
 '(winner-mode t nil (winner))
 '(mouse-wheel-scroll-amount '(1))
 '(scroll-conservatively 10000)
 '(auto-window-vscroll nil)
 '(show-trailing-whitespace t)
 '(iswitchb-mode t)
 '(ring-bell-function nil))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; right C-a button
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
