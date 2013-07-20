;; start emacsserver
(server-start)

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
                           multiple-cursors workspaces twilight-theme
                           writegood-mode ahg))
;; load local packages
(mapc 'add-to-load '("" "git-emacs" "clevercss-mode" "unittest-mode"))

;; ahg
(defcustom ahg-hg-command "/usr/local/bin/hg"
  "Command to use for invoking Mercurial."
  :group 'ahg :type 'string)

;; require packages
; markup
(require 'haml-mode)
(require 'markdown-mode)
(require 'jinja2-mode)
(require 'clevercss)
; langs
(require 'clojure-mode)
(require 'coffee-mode)
; python's modes
(require 'pony-mode)
(require 'ac-python)
(require 'unittest)
; dvcs
(require 'git-emacs)
(require 'ahg)
; theme
(require 'twilight-theme)
; etc
(require 'writegood-mode)
(require 'multiple-cursors)
(load-library "workspaces.el")
(load "window-resize.el")

;; full path to opened file
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; font
(set-face-attribute 'default nil :font "Menlo Regular-11")

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; writegood-mode
(global-set-key "\C-c\C-g" 'writegood-mode)

;; workspaces
(global-set-key "\C-xg" 'workspace-goto)

;; clevercss mode
(add-to-list 'auto-mode-alist '("\\.ccss$" . clevercss-mode))

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffeescript mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  ;; CoffeeScript uses two spaces.
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  ;; If you don't want your compiled files to be wrapped
  (setq coffee-args-compile '("-c" "--bare"))
  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
  ;; Compile '.coffee' files on every save
  (and (file-exists-p (buffer-file-name))
       (file-exists-p (coffee-compiled-file-name))
       (coffee-cos-mode t)))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; erlang mode
;; http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html
;; set erlang directories
;; (setq load-path (cons  "/opt/local/lib/erlang/lib/tools-2.6.8/emacs"
;;       load-path))
;; (setq erlang-root-dir "/opt/local/lib/erlang")
;; (setq exec-path (cons "/opt/local/bin" exec-path))
;; (require 'erlang-start)

;; ;; This is needed for Distel setup
;; (let ((distel-dir "~/.emacs.d/vendor/distel/elisp"))
;; (unless (member distel-dir load-path)
;; ;; Add distel-dir to the end of load-path
;; (setq load-path (append load-path (list distel-dir)))))

;; (require 'distel)
;; (distel-setup)

;; ;; A number of the erlang-extended-mode key bindings are useful in the shell too
;; (defconst distel-shell-keys
;;  '(("TAB"      erl-complete)
;;    ("\M-."      erl-find-source-under-point)
;;    ("\M-,"      erl-find-source-unwind)
;;    ("\M-*"      erl-find-source-unwind)
;;   )
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-mode-hook
;; 	  (lambda ()
;; 	    ;; when starting an Erlang shell in Emacs, default in the node name
;; 	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
;; 	    ;; add Erlang functions to an imenu menu
;; 	    (imenu-add-to-menubar "imenu")))

;; ;; erlang-shell
;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda ()
;; 	    ;; add some Distel bindings to the Erlang shell
;; 	    (dolist (spec distel-shell-keys)
;; 	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; configs
(global-set-key [?\C-,] 'previous-buffer)
(global-set-key [?\C-.] 'next-buffer)
(global-set-key [?\C-w] 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-buffer-and-window)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [?\C-#] 'comment-or-uncomment-region)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(default-tab-width 4 t)
 '(desktop-save-mode 1)
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

;; python-mode
;; https://github.com/gabrielelanaro/emacs-for-python
(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")
(epy-setup-checker "~/.emacs.d/pycheker.sh %f")

;; html-mode-hook
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))

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
