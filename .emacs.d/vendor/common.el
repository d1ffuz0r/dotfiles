;; ---------------------------------
;; functions
;; ---------------------------------
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun add-to-load (path)
  (add-to-list 'load-path (concat "~/.emacs.d/vendor/" path)))

;; ---------------------------------
;; packages
;; ---------------------------------
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("erlang-emacs" . "http://xn--5dbqm8ad.xn--9dbdkw.se/erlang-emacs/")))

(setq base-packages '(dired+ multiple-cursors workspaces writegood-mode windresize))

(mapc 'install-if-needed (append base-packages install-packages))
(mapc 'add-to-load local-packages)


;; ---------------------------------
;; base packages. most useful and deletion is not allowed
;; ---------------------------------
(defun workspaces ()
  (load-library "workspaces.el")
  (global-set-key (kbd "C-x g") 'workspace-goto))

(defun dired+ ()
  (require 'dired+))

(defun multiple-cursors ()
  (require 'multiple-cursors)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(defun writegood-mode ()
  (require 'writegood-mode)
  (global-set-key (kbd "C-c C-g") 'writegood-mode))

(defun windresize ()
  (require 'windresize)
  (setq windresize-increment 1)
  (global-set-key [C-M-down] 'windresize-up-minus)
  (global-set-key [C-M-up] 'windresize-down-minus)
  (global-set-key [C-M-left] 'windresize-right-minus)
  (global-set-key [C-M-right] 'windresize-left-minus))

(mapcar 'funcall base-packages)

;; ---------------------------------
;; keybindings
;; ---------------------------------
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c g") 'rgrep)

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
;; ibuffer
;; ---------------------------------
(require 'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))


;; ---------------------------------
;; common settings
;; ---------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(auto-window-vscroll nil t)
 '(desktop-save-mode t)
 '(default-tab-width 4 t)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(global-hl-line-mode nil)
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
