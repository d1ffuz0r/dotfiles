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

(setq custom-packages '(haml-mode markdown-mode jinja2-mode stylus-mode coffee-mode
                        pony-mode jedi erlang distel dash-at-point projectile flymake-coffee
                        json-mode puppet-mode js2-mode))

(setq epy-packages '(autopair flymake-cursor python virtualenv nose auto-complete
                     dropdown-list yasnippet yasnippet-bundle yas-jit))

(setq ac-modes '(coffee-mode sql-mode erlang-mode html-mode stylus-mode emacs-lisp-mode js2-mode
                 javascript-mode))

(setq install-packages (append custom-packages epy-packages))

;; (package-refresh-contents)
;; common
(require 'common)
;; markup
;;(require 'haml-mode)
(require 'jinja2-mode)
;;(require 'markdown-mode)
;;(require 'stylus-mode)
(require 'json-mode)
;; languages
(require 'js2-mode)
(require 'coffee-mode)
(require 'puppet-mode)
;; erlang modes
(require 'erlang-start)
(require 'distel)
;; python modes
(require 'epy-init)
;;(require 'unittest)
(require 'pony-mode)
;; dvcs
(require 'git-emacs)
;;(require 'mercurial)
;; etc
(require 'flymake-coffee)
(require 'dash-at-point)
(require 'projectile)
(require 'yas-jit)

;; yasnippets
(yas/jit-load)
(yas/load-directory "~/.emacs.d/vendor/snippets")

;; font
;; (set-face-attribute 'default nil :font "Menlo-12")
;; (setq-default line-spacing 0.05)
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
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)
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
  (imenu-add-to-menubar "imenu")
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist)))

(defun erlang-shell-hook ()
  (eldoc-mode t)
  (dolist (spec distel-shell-keys)
    (define-key erlang-shell-mode-map (car spec) (cadr spec))))

(add-to-list 'flymake-allowed-file-name-masks
             (list "\\.erl\\'" (apply-partially 'flymake-command-parse
                                                "~/.emacs.d/erlcheker.sh %f")))

(add-hook 'erlang-mode-hook 'erlang-hook)
(add-hook 'erlang-mode-hook 'flymake-find-file-hook)
(add-hook 'erlang-shell-mode-hook 'erlang-shell-hook)
(add-to-list 'auto-mode-alist '("\\.src$" . erlang-mode))

;; html-mode
(defun html-hook ()
  (setq sgml-basic-offset 2))

(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-hook 'html-mode-hook 'html-hook)

;; javascript-mode
(defun javascript-mode-hook()
  (make-local-variable 'tab-width)
  (setq tab-width 2))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'javascript-mode-hoo)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; settings
(custom-set-variables
 '(global-auto-revert-mode t)
 '(global-linum-mode nil)
 '(winner-mode t nil (winner))
 '(global-hl-line-mode t)
 ;'(cursor-type '(bar . 5))
 )
