;; solarized theme
;; git://github.com/sellout/emacs-color-theme-solarized.git
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; jabber
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-jabber")
(require 'jabber)
(require 'jabber-autoloads)

;; workspaces
;; http://www.emacswiki.org/emacs/workspaces.el
;; http://filonenko-mikhail.blogspot.com/2012/01/emacs-workspaces.html
(add-to-list 'load-path "~/.emacs.d/vendor/workspaces")
(load-library "workspaces.el")
(global-set-key "\C-xg" 'workspace-goto)

;; pep8
(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
         (list "~/.emacs.d/pep8.py" (list local-file))))
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init)))

;; clevercss mode
;; git://github.com/jschaf/CleverCSS-Mode.git
(add-to-list 'load-path "~/.emacs.d/vendor/clevercss-mode")
(require 'clevercss)

(add-to-list 'auto-mode-alist '("\\.ccss$" . clevercss-mode))

;; haml mode
;; git://github.com/rradonic/haml-mode.git
(add-to-list 'load-path "~/.emacs.d/vendor/haml-mode")
(require 'haml-mode)

;; jinja2 mode
;; git://github.com/paradoxxxzero/jinja2-mode.git
(add-to-list 'load-path "~/.emacs.d/vendor/jinja2-mode")
(require 'jinja2-mode)

;; markdown mode
;; http://jblevins.org/projects/markdown-mode/
(add-to-list 'load-path "~/.emacs.d/vendor/markdown-mode")
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; coffeescript mode
;; git://github.com/defunkt/coffee-mode.git
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)

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

;; configs

(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
  (this-window-y-min (nth 1 win-edges))
  (this-window-y-max (nth 3 win-edges))
  (fr-height (frame-height)))
  (cond
    ((eq 0 this-window-y-min) "top")
    ((eq (- fr-height 1) this-window-y-max) "bot")
(  t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
  middle"
  (let* ((win-edges (window-edges))
  (this-window-x-min (nth 0 win-edges))
  (this-window-x-max (nth 2 win-edges))
  (fr-width (frame-width)))
  (cond
    ((eq 0 this-window-x-min) "left")
    ((eq (+ fr-width 4) this-window-x-max) "right")
    (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
    ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
    ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
    ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
    (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
    ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
    ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
    ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
    (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
    ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
    ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
    ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
    ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
    ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
    ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(global-set-key [C-M-down] 'win-resize-mi2nimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

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
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-step 1)
 '(global-hl-line-mode 1)
 '(inhibit-startup-screen t)
 '(desktop-save-mode 1)
 '(default-tab-width 4)
 '(winner-mode t nil (winner))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(require-final-newline t)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)

;; Tabbar
(add-to-list 'load-path "~/.emacs.d/vendor/emhacks")
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
 :background "white"
 :foreground "#073642"
 :box '(:line-width 5 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "black"
 :foreground "white"
 :box '(:line-width 5 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "gray75"
 :foreground "#073642"
 :underline nil
 :box '(:line-width 5 :color "#073642" :style nil))
(set-face-attribute
 'tabbar-button nil
 :background "#002b36"
 :foreground "#073642"
 :box '(:line-width 1 :color "#073642" :style nil))

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

;; emacs-nav
;; http://code.google.com/p/emacs-nav/
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-nav")
(require 'nav)
(global-set-key [f8] 'nav-toggle)

;; python-mode
;; https://github.com/gabrielelanaro/emacs-for-python
(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")
