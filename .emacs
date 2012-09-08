(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

;; erlang mode
;; set erlang directories
(setq load-path (cons  "/opt/local/lib/erlang/lib/tools-2.6.8/emacs"
      load-path))
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/bin" exec-path))
(require 'erlang-start)

;; This is needed for Distel setup
(let ((distel-dir "~/.emacs.d/vendor/distel/elisp"))
(unless (member distel-dir load-path)
;; Add distel-dir to the end of load-path
(setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
 '(("TAB"      erl-complete)
   ("\M-."      erl-find-source-under-point)
   ("\M-,"      erl-find-source-unwind)
   ("\M-*"      erl-find-source-unwind)
  )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; erlang-shell
(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; configs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-variables
 '(scroll-bar-mode nil)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(desktop-save-mode 1)
 '(default-tab-width 4)
 '(winner-mode t nil (winner))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil))
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;--------------------tabbar------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/emhacks")
;; Tabbar
(require 'tabbar)

;; Tabbar hotkeys
(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key [(meta k)] 'tabbar-forward)

;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "#002b36"
 :foreground "#073642"
 :box '(:line-width 1 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "#002b36"
 :foreground "white"
 :box '(:line-width 5 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "#073642"
 :box '(:line-width 5 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :background "#002b36"
 :foreground "#073642"
 :box '(:line-width 1 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "#002b36"
 )

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)
;;-------------------END----------------------------


;; emacs-nav
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-nav")
(require 'nav)
(global-set-key [f8] 'nav-toggle)

;; python-mode
;; https://github.com/gabrielelanaro/emacs-for-python
(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")
