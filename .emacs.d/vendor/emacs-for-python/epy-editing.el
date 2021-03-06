;; ibuffer by default

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Ido mode with fuzzy matching
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Open Next Line
(require 'open-next-line)

;; Auto Completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat epy-install-dir "auto-complete/ac-dict"))
(ac-config-default)

;; Yasnippet - force the loading of the custom version of yasnippet
(require 'yasnippet)
(require 'yasnippet-bundle)
(load-file (concat epy-install-dir "extensions/snippet-helpers.el"))

;; this one is to activate django snippets
(defun epy-django-snippets ()
  "Load django snippets"
  (interactive)
  (yas/load-directory (concat epy-install-dir "snippets/django")))

(yas/initialize)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas/wrap-around-region 'cua)

;; code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
(beginning-of-line)
(push-mark)
(end-of-line)
(let ((str (buffer-substring (region-beginning) (region-end))))
  (when commentfirst
    (comment-region (region-beginning) (region-end)))
  (insert-string
   (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
  (forward-line -1)))

;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c c") (lambda ()
                                (interactive)
                                (djcb-duplicate-line t)))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(global-set-key (kbd "C-c l") 'mark-line)


; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Other useful stuff

; delete seleted text when typing
(delete-selection-mode 1)

; highlight brackets
(show-paren-mode t)

(provide 'epy-editing)
