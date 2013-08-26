;; epy-python.el - setup of python stuff

;; pymacs
;; (require 'pymacs (concat epy-install-dir "extensions/pymacs.el"))

;; Ipython integration with fgallina/python.el
(defun epy-setup-ipython ()
  "Setup ipython integration with python-mode"
  (interactive)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n")
  )

;;=========================================================
;; Flymake additions, I have to put this one somwhere else?
;;=========================================================

(defun flymake-create-copy-file ()
  "Create a copy local file"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace)))
    (file-relative-name
     temp-file
     (file-name-directory buffer-file-name))))

(defun flymake-command-parse (cmdline)
  "Parses the command line CMDLINE in a format compatible
       with flymake, as:(list cmd-name arg-list)

The CMDLINE should be something like:

 flymake %f python custom.py %f

%f will be substituted with a temporary copy of the file that is
 currently being checked.
"
  (let ((cmdline-subst (replace-regexp-in-string "%f" (flymake-create-copy-file) cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))


(when (load-file (concat epy-install-dir "extensions/flymake-patch.el"))
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

(defun epy-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline)))
  )


;; Python or python mode?
(eval-after-load 'python
  '(progn
     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)


     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)


     ;; when we swich on the command line, switch in Emacs
     (desktop-save-mode 1)
     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))
     )
  )

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

(provide 'epy-python)
