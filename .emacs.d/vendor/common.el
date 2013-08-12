;; C-a remapping
;; full path to opened file in title
;; packages
;; autocreation backup directories
;; enable lower/upper-region


;; ---------------------------------
;; packages
;; ---------------------------------
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(mapc 'install-if-needed install-modes)


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
;; create the autosave dir if necessary, since emacs won't.
;; ---------------------------------
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)


;; ---------------------------------
;; enable lower/upper-region
;; ---------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ---------------------------------
;; common hooks
;; ---------------------------------
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ---------------------------------
;; common settings
;; ---------------------------------
(fset 'yes-or-no-p 'y-or-n-p)


(provide 'common)
