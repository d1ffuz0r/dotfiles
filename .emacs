(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq local-packages '("git-emacs" "mercurial" "unittest-mode" "emacs-for-python"))

(setq custom-packages '(haml-mode markdown-mode jinja2-mode coffee-mode clojure-mode
                        pony-mode jedi multiple-cursors workspaces dired+ windresize
                        writegood-mode dash-at-point monokai-theme zenburn-theme
                        erlang distel))

(setq epy-packages '(autopair flymake-cursor python virtualenv nose auto-complete
                     dropdown-list yasnippet yasnippet-bundle))

(setq install-packages (append custom-packages epy-packages))

;; (package-refresh-contents)
;; common
(require 'common)
;; theme
; (require 'monokai-theme)
; (require 'zenburn-theme)
; (require 'twilight-theme)
;; markup
(require 'haml-mode)
(require 'jinja2-mode)
(require 'markdown-mode)
;; languages
(require 'clojure-mode)
(require 'coffee-mode)
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
(require 'dired+)
(require 'multiple-cursors)
(require 'writegood-mode)
(require 'windresize)
;; libraries
(load-library "workspaces.el")

;; font
(set-face-attribute 'default nil :font "Menlo Regular-12")

;; python
; (epy-django-snippets)
(epy-setup-checker "~/.emacs.d/pychecker.sh %f")

;; jedi
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-show-doc (kbd "C-c C-d"))
(add-hook 'python-mode-hook 'jedi:setup)

;; dash
(global-set-key (kbd "C-c d") 'dash-at-point)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; windresize
(setq windresize-increment 1)
(global-set-key [C-M-down] 'windresize-up-minus)
(global-set-key [C-M-up] 'windresize-down-minus)
(global-set-key [C-M-left] 'windresize-right-minus)
(global-set-key [C-M-right] 'windresize-left-minus)

;; writegood-mode
(global-set-key (kbd "C-c C-g") 'writegood-mode)

;; workspaces
(global-set-key (kbd "C-x g") 'workspace-goto)

;; markdown-mode
;; (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffee-mode-hook
(add-hook 'coffee-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (make-local-variable 'tab-width)
            (set 'tab-width 2)
            (setq coffee-args-compile '("-c" "--bare"))
            (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
            ;; Compile '.coffee' files on every save
            (and (file-exists-p (buffer-file-name))
                 (file-exists-p (coffee-compiled-file-name))
                 (coffee-cos-mode t))))

;; erlang-mode
(distel-setup)

(defconst distel-shell-keys
  '(([tab] erl-complete)
    ("\M-." erl-find-source-under-point)
    ("\M-," erl-find-source-unwind)
    ("\M-*" erl-find-source-unwind))
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            (imenu-add-to-menubar "imenu")))

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (eldoc-mode t)
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; html-mode-hook
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))

;; keys
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c g") 'rgrep)

;; settings
(custom-set-variables
 '(default-tab-width 4 t)
 '(column-number-mode t)
 '(global-auto-revert-mode t)
 '(global-linum-mode nil)
 '(global-hl-line-mode nil)
 '(indicate-empty-lines t)
 '(winner-mode t nil (winner)))
