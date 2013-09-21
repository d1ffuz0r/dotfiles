;; Help
;; C->: mc/mark-next-like-this
;; C-<: mc/mark-previous-like-this
;; C-c C-<: mc/mark-all-like-this
;; C-w: backward-kill-word
;; C-x C-k: kill-region
;; C-c C-k: kill-buffer-and-window
;; C-c C-g: writegood-mode
;; C-x C-g: goto-line
;; C-c g: rgrep
;; C-x g: workspace-goto

(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq local-packages '("git-emacs" "mercurial" "unittest-mode" "emacs-for-python"))

(setq custom-packages '(haml-mode markdown-mode jinja2-mode coffee-mode clojure-mode
                        pony-mode jedi dash-at-point monokai-theme zenburn-theme
                        erlang distel rust-mode projectile))

(setq epy-packages '(autopair flymake-cursor python virtualenv nose auto-complete
                     dropdown-list yasnippet yasnippet-bundle))

(setq ac-modes '(coffee-mode sql-mode erlang-mode clojure-mode rust-mode html-mode))

(setq install-packages (append custom-packages epy-packages))

;; (package-refresh-contents)
;; common
(require 'common)
;; theme
; (require 'monokai-theme)
; (require 'twilight-theme)
(require 'obsidian-theme)
; (require 'zenburn-theme)
;; markup
(require 'haml-mode)
(require 'jinja2-mode)
(require 'markdown-mode)
;; languages
(require 'clojure-mode)
(require 'coffee-mode)
(require 'rust-mode)
;; erlang modes
(require 'erlang-start)
(require 'distel)
;; python modes
(require 'epy-init)
(require 'unittest)
(require 'pony-mode)
;; dvcs
(require 'git-emacs)
(require 'mercurial)
;; etc
(require 'dash-at-point)
(require 'projectile)

;; font
(set-face-attribute 'default nil :font "Menlo Regular-12")

;; jedi
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-show-doc (kbd "C-c C-d"))

;; python
; (epy-django-snippets)
(epy-setup-checker "~/.emacs.d/pychecker.sh %f")
(add-hook 'python-mode-hook 'jedi:setup)

;; dash
(global-set-key (kbd "C-c d") 'dash-at-point)

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffee-mode
(defun coffee-hook ()
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  (setq coffee-args-compile '("-c" "--bare"))
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook 'coffee-hook)

;; erlang-mode
(distel-setup)

(defconst distel-shell-keys
  '(([tab] erl-complete)
    ("\M-." erl-find-source-under-point)
    ("\M-," erl-find-source-unwind)
    ("\M-*" erl-find-source-unwind))
  "Additional keys to bind when in Erlang shell.")

(defun erlang-hook ()
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (imenu-add-to-menubar "imenu"))

(defun erlang-shell-hook ()
  (eldoc-mode t)
  (dolist (spec distel-shell-keys)
    (define-key erlang-shell-mode-map (car spec) (cadr spec))))

(add-hook 'erlang-mode-hook 'erlang-hook)
(add-hook 'erlang-shell-mode-hook 'erlang-shell-hook)

;; html-mode
(defun html-hook ()
  (setq sgml-basic-offset 4))

(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-hook 'html-mode-hook 'html-hook)

;; settings
(custom-set-variables
 '(global-auto-revert-mode t)
 '(global-linum-mode nil)
 '(winner-mode t nil (winner)))
