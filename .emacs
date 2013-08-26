(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq local-packages '("git-emacs" "mercurial" "unittest-mode"
                       "emacs-for-python"))
(setq install-packages '(haml-mode markdown-mode jinja2-mode
                         coffee-mode clojure-mode pony-mode
                         multiple-cursors workspaces monokai-theme
                         writegood-mode dash-at-point dired+
                         zenburn-theme))

;; (package-refresh-contents)
;; base
(require 'common)
; theme
; (require 'monokai-theme)
(require 'zenburn-theme)
; markup
(require 'haml-mode)
(require 'markdown-mode)
(require 'jinja2-mode)
; languages
(require 'clojure-mode)
(require 'coffee-mode)
; python modes
(require 'epy-init)
(require 'unittest)
(require 'pony-mode)
(require 'ac-python)
; dvcs
(require 'git-emacs)
(require 'mercurial)
; dired
(require 'dired+)
; etc
(require 'writegood-mode)
(require 'multiple-cursors)
(require 'dash-at-point)
(require 'window-resize)
; libraries
(load-library "workspaces.el")

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

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffeescript-mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; coffee-mode-hook
(add-hook 'coffee-mode-hook
          (lambda ()
            (make-local-variable 'tab-width)
            (set 'tab-width 2)
            (setq coffee-args-compile '("-c" "--bare"))
            (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
            ;; Compile '.coffee' files on every save
            (and (file-exists-p (buffer-file-name))
                       (file-exists-p (coffee-compiled-file-name))
                       (coffee-cos-mode t))))

;; html-mode-hook
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))

;; pychecker
(epy-setup-checker "~/.emacs.d/pychecker.sh %f")

;; keys
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c g") 'rgrep)


(custom-set-variables
 '(default-tab-width 4 t)
 '(desktop-save-mode t)
 '(column-number-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode nil)
 '(iswitchb-mode t)
 '(winner-mode t nil (winner)))
