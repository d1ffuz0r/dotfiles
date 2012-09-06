 (setq load-path (cons  "/opt/local/lib/erlang/lib/tools-2.6.7/emacs"
      load-path))
      (setq erlang-root-dir "/opt/local/lib/erlang")
      (setq exec-path (cons "/opt/local/bin" exec-path))
      (require 'erlang-start)
    

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")


(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("TAB"      erl-complete)	
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

;; This is needed for Distel setup
(let ((distel-dir "~/.emacs.d/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(desktop-save-mode 1)
(load-theme 'solarized-dark t)


(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(custom-set-variables
'(inhibit-startup-screen t)
'(tool-bar-mode nil)
'(visible-bell t)
'(winner-mode t nil (winner))
'(initial-scratch-message nil))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(desktop-save-mode 1)
(load-theme 'solarized-dark t)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Emacs nav
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-nav")
(require 'nav)
(global-set-key [f8] 'nav-toggle)

;; https://github.com/gabrielelanaro/emacs-for-python
(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")
