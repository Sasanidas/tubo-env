;;; 053-prog-scripts.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;; *************************** Python Settings ****************************

;; Utility to complete "import" statements with skeleton.

(defvar yc/system-py-modules nil "Python modules installed in system directory.")

(defun yc/get-python-files-of-dir (dir)
  "Return all python files in `DIR' without extension."
  (let ((fn-list nil)
        (yc/r-match-pyfile (rx (group (+? ascii)) ".py" (? (or "c" "o")))))
    (when (and dir
               (file-exists-p dir))
      (dolist (fn (directory-files dir nil yc/r-match-pyfile))
        (if (string-match yc/r-match-pyfile fn)
            (progn
              (setq fn-list (append fn-list (list (match-string 1 fn))))
              ))))
    fn-list))

(defun yc/get-python-modules ()
  "Return all python modules."
  (unless yc/system-py-modules
      (setq yc/system-py-modules
            (mapcar
             (lambda (f) (list f ))
             (apply
              'append
              (mapcar
               'yc/get-python-files-of-dir
               semantic-dependency-system-include-path)))))
  (append (yc/get-python-files-of-dir ".") (copy-sequence yc/system-py-modules)))

(define-skeleton skeleton-python-import
  "generate include<>" ""
  > "import "
  (completing-read
   "Import File:" (yc/get-python-modules)))


(use-package lsp-python-ms
  :commands (lsp-python-ms-setup)
  ;; :config
  ;; (progn
  ;;   (setq ;; lsp-python-executable-cmd "python3"


  ;;    ))

  :custom
  (lsp-python-ms-python-executable-cmd "python3")
  (lsp-python-ms-dir (concat yc/lsp-server-dir "/ms-python/"))
  (lsp-python-ms-executable (concat lsp-python-ms-dir
                                      "Microsoft.Python.LanguageServer"
                                      (and (eq system-type 'windows-nt) ".exe")))
  )

(use-package python
  :custom
  (python-shell-interpreter (if (executable-find "python3")
                                "python3" "python"))
  :hook (
         (python-mode
          .
          (lambda ()
            (unless (buffer-file-name)
              (flycheck-mode -1))

            (yc/lsp--setup (concat yc/lsp-server-dir "/ms-python/" "Microsoft.Python.LanguageServer")
                           "M-x: lsp-python-ms-setup"
                           (lambda ()
                             (unless (featurep 'lsp-python-ms)
                               (require 'lsp-python-ms))))

            ;; (yc/lsp--setup "pyls" "pip install 'python-language-server[yapf]'"
            ;;                (lambda ()
            ;;                  (unless (featurep 'lsp-pyls)
            ;;                    (require 'lsp-pyls)
            ;;                    (setq-default
            ;;                     lsp-clients-python-command '("pyls"  "-v" "--log-file"
            ;;                                                  "/tmp/pyls.log"))

            ;;                    (let ((pyls (expand-file-name (executable-find "pyls")))
            ;;                          (lsp-py-lib-dirs
            ;;                           '("/usr")))
            ;;                      (aif (file-name-directory (substring (file-name-directory pyls) 0 -1))
            ;;                          (push it
            ;;                                lsp-clients-python-library-directories))))))

            ))))

(defun yc/pyvenv-active ()
  "Enable pyvenv if necessary."
  (interactive)
  (unless (featurep 'pyvenv)
    (require 'pyvenv))
  (unless pyvenv-virtual-env
    (let ((directory (ivy-read "Active pyvenv in Directory: "
                               (directory-files  default-directory t )
                               :action (lambda (cand)
                                         (interactive)
                                         cand))))
      (pyvenv-activate directory))))

(use-package py-autopep8
  :commands (py-autopep8-buffer)
  :config
  (progn
    (setq py-autopep8-options '("--max-line-length=120"))))

(use-package pydoc
  :bind (:map python-mode-map
              (;; ,(kbd "<S-f1>")
               [S-f1]. pydoc-at-point)))

;; ***************** sh-mode *****************
(use-package sh-script
  :mode (((rx (or (: (+? ascii) "." (or "zsh" "ebuild" "eclass"))
                  (: (or ".bashrc.d" ".zshrc.d") "/" (+ nonl) "rc")
                  (: "/etc/" (or "init.d" "zsh" "profi") (+ nonl))
                  (: (+? nonl) "/zsh/" (+? nonl) "functions" (+ nonl))
                  )
              buffer-end)
          . sh-mode))
  :custom
  (sh-builtins
   (quote
    (
     (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait" "die"
            "edebug" "elog" "einfo" "ewarn" "ebegin" "eend")

     (bash sh-append shell "." "alias" "bg" "bind" "builtin" "caller" "compgen" "complete"
           "declare" "dirs" "disown" "enable" "fc" "fg" "help" "history" "jobs" "kill"
           "let" "local" "popd" "printf" "pushd" "shopt" "source" "suspend" "typeset"
           "unalias" "command" "hash" "test" "type" "eval" "export" "getopts" "newgrp" "pwd"
           "read" "readonly" "times" "ulimit" "alias" "bg" "false" "fc" "fg"
           "jobs" "kill" "let" "print"  "time" "typeset" "unalias" "whence"
           "edebug" "einfo" "ewarn" "eerror" "emerge" "alias_if_exists")

     (zsh sh-append bash "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
          "disable" "disown" "echotc" "enable" "functions" "getln" "hash" "history"
          "integer" "limit" "local" "log" "popd" "pushd" "r" "readonly" "rehash" "sched"
          "setopt" "source" "suspend" "true" "ttyctl" "type" "unfunction" "unhash"
          "unlimit" "unsetopt" "vared" "which" "zle" "compdef" "compinit" "zstyle" "colors"))))
  ;; :hook ((sh-mode .  (lambda ()
  ;;                      (let ((zsh-file (executable-find "zsh"))
  ;;                            (fname (buffer-file-name)))
  ;;                        (when (and fname
  ;;                                   (string-match (rx (*? ascii) (| "zsh" "zsh")) fname)
  ;;                                   zsh-file)
  ;;                          (setq-default sh-shell-file zsh-file)
  ;;                          (sh-set-shell (file-name-nondirectory zsh-file)))))))
  :config
  (progn
    (yc/add-run-unit 'shell 70
     (when (or (equal ext "sh")
               (equal ext "SH"))
       (lambda ()
         (interactive)
         (format "./%s" file)))))

  :hook (
         (sh-mode . (lambda ()
                      (yc/lsp--setup "bash-language-server" "npm i -g bash-language-server"
                                     (lambda ()
                                       (unless (boundp 'lsp-clients)
                                         (require 'lsp-mode))
                                       (unless
                                           (gethash 'bash-ls lsp-clients)
                                         (lsp-register-client
                                          (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                                                           :major-modes '(sh-mode)
                                                           :priority -1
                                                           :server-id 'bash-ls))))))))
  )

 ;; Make script executable.

(defvar new-script-list nil "List of newly created scripts.")
(defun check-script-status ()
  "Check status of scripts."
  (unless (file-exists-p buffer-file-name)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (and (looking-at "^#!")
                   (not (member buffer-file-name new-script-list)))
          (add-to-list 'new-script-list buffer-file-name))))))

(defun make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable."
  (interactive)
  (when (or (member buffer-file-name new-script-list)
            (called-interactively-p 'interactive))
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o111))
    (setq new-script-list (delete buffer-file-name new-script-list))
    (message "Made %s executable" buffer-file-name)))

(add-hook 'before-save-hook 'check-script-status)
(add-hook 'after-save-hook 'make-script-executable)


(provide '053-prog-scripts)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 053-prog-scripts.el ends here
