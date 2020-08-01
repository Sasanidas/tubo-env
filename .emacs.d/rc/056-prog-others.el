;;; 059-prog-others.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;; php mode
(use-package php-mode
  :mode (rx "." (or (: "php" (* (or "s" "t" digit))) "phtml" "Amkfile" "amk"))
  :interpreter ("php" . php-mode))


(use-package systemtap-mode :mode "\\.stp\\'")

 ;; Javascript mode
(use-package js-mode

  :commands (js-mode)
  :mode (rx (or (: bow "manifest") ".json" ".js" ".pac") eol)
  ;; :init
  ;; (progn
  ;;   (custom-set-variables
  ;;    '(safe-local-variable-values
  ;;      (quote
  ;;       ((js2-basic-offset . 4))))))
  :config
  (progn
    (define-key js2-mode-map "\C-c\C-x" 'executable-interpret)))


;; (use-package java
;;   :defer t
;;   :hook ((java-mode
;;           .
;;           (lambda ()
;;             ;; (yc/add-company-backends-with-yasnippet company-eclim)
;;             (unless (featurep 'lsp-java)
;;               (require 'lsp-java))
;;             (lsp)))))


(use-package batch-mode :mode (rx "." (or "bat" "cmd")))

;; Poweshell
(use-package powershell-mode
  :mode "\\.ps1\\'"
  :bind (:map powershell-mode-map
              ([(f1)] .  'yc/pws-get-help)
              ([(meta .)] . 'yc/pws-find-tag)))

(defun yc/pws-find-tag (function)
  "find defination of function under current poin"
  (interactive
   (let* ((fn (thing-at-point 'symbol))
          (val nil))
     (message fn)
     (setq val (completing-read (if fn
                                    (format "search for (default %s): " fn)
                                  "search for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (let ((cmd nil))
    (if (null function)
        (message "you didn't specify a function")
      (progn
        (setq cmd (concat "egrep -i \"^function +" function "\" . -ri"))
        (eshell-command cmd)
        (pop-to-buffer (get-buffer "*grep*"))
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (kill-line 3)
        (insert (concat "*********************** find tag for:"
                        function "********************\n\n"))
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun yc/pws-get-help (function)
  "display the documentation of function (a symbol)."
  (interactive
   (let ((fn (thing-at-point 'symbol))
         val)
     (message fn)
     (setq val (completing-read (if fn
                                    (format "help for (default %s): " fn)
                                  "help for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (if (null function)
      (message "you didn't specify a function")
    (progn
      (start-process "powershell-help" nil "devhelp" "-s" function))))

(use-package yaml-mode
  :mode (rx (or ".clang-format" (: ".y" (? "a" ) "ml")) eol)
  :hook ((yaml-mode .           (lambda ()
            (unless (buffer-file-name)
              (flycheck-mode -1))

            (yc/lsp--setup "yaml-language-server"
                           "https://github.com/redhat-developer/yaml-language-server")))))


(use-package qml-mode :mode "\\.qml$")
(use-package swig-mode  :mode (rx (or ".i" ".swig") eol))

(defun yc/bison-setup-imenu-function ()
  "Setup imenu-index-function for Bison mode.."
  (interactive)
  (PDEBUG "ENTER.")
  (setq imenu-create-index-function
        (lambda ()
          (let ((end))
            (beginning-of-buffer)
            (re-search-forward "^%%")
            (forward-line 1)
            (setq end (save-excursion (re-search-forward "^%%") (point)))
            (loop while (re-search-forward "^\\([a-z].*?\\)\\s-*\n?\\s-*:" end t)
                  collect (cons (match-string 1)
                                (point))))))
  (PDEBUG "BUF:" (current-buffer)
          "FUNC:" imenu-create-index-function)
  )

(use-package bison-mode
  :mode (rx "." (or "yy" "y" "jison") eol)
  :config
  (progn
    (setq  bison-rule-enumeration-column 10)))

 ;; SQL Mode
(defun yc/sqlup-comment-p-adv (func &rest args)
  "Advice for `sqlup-comment-p'.
FUNC is `sqlup-comment-p', and arguments are stored in ARGS.
It seems `syntax-pass' does not work properly when the buffer contains # -*- ."
  (looking-back (rx (or "#" "--") (*? nonl))))

(use-package sqlup-mode  :commands (sqlup-mode)
  :config
  (progn
    (advice-add 'sqlup-comment-p :around #'yc/sqlup-comment-p-adv)
    (advice-add 'sqlup-capitalize-as-you-type :around #'yc/sqlup-capitalize-as-you-type))
  :hook ((sql-mode . sqlup-mode)))


(defun yc/sqlup-capitalize-as-you-type (func &rest args)
  "Advice for `sqlup-capitalize-as-you-type'.
Call FUNC which is `sqlup-capitalize-as-you-type' with ARGS only when buffer is modified."
  (if (buffer-modified-p)
      (apply func args)))


(use-package sql-indent
  :commands (sqlind-minor-mode)
  :config
  (setq sqlind-indentation-offsets-alist
        `((select-clause 0)
          (insert-clause 0)
          (delete-clause 0)
          (update-clause 0)
          ,@sqlind-default-indentation-offsets-alist))

  :hook ((sql-mode . sqlind-minor-mode)))

(use-package sql+

  :commands (sql/eval-sql sql/choose-dbms sql/choose-database company-sql
                         company-sql-update-candidates
                         eshell/restart_pg sql/remove-costs))

(defun yc/sql-mode-hook ()
  "My hook to run for sql mode."
  (ws-butler-mode -1)
  (yc/add-company-backends-with-yasnippet company-sql company-keywords))

(use-package sql
  :mode ((rx (or (: "." (or "sql" "ddl") (? (or "_in" ".result" ".reject")))
                 (: (or "input" "output") "/" (+? nonl) ".source")
                 (: "test/" (+? nonl) (or "output" "input") "/"
                    (+? nonl) ".source")
                 (: (or "results" "expected") (*? nonl) "/"
                    (+? nonl) ".out")
                 ) eol)  . sql-mode)
  :hook (
         (sql-mode . yc/sql-mode-hook)
         (sql-interactive-mode
          .
          (lambda ()
            (yc/sql-mode-hook)
            (let ((yc/debug-log-limit -1))
              (PDEBUG "PRODUCT:"
                      sql-product))

            (if (equal sql-product 'postgres)
                (setq sql-prompt-regexp (rx bol (* (or alnum "_" ".")) "=" (or "#" ">") " "))))))

  :bind ((;; ,(kbd "C-c C-b")
          "". sql/eval-sql))
  :config
  (progn
    (require 'sql+)))

 ;; protobuf-mode
(use-package protobuf-mode :mode (rx ".proto" eol))

 ;; console mode
(use-package console-mode  :commands (console-mode))

 ;; scala-mode.
(use-package ensime
  :commands (ensime)
  :config
  (progn
    (advice-add 'ensime-company-enable :around #'yc/ensime-company-enable)
    (custom-set-variables
     '(ensime-startup-notification nil)
     '(ensime-startup-snapshot-notification nil)
     '(ensime-db-default-port "5005")
     '(ensime-startup-dirname (yc/make-cache-path "ensime")))))

(defun yc/ensime-company-enable (func &rest args)
  "Advice for `ensime-company-enable'.
Call FUNC with ARGS."
  (yc/add-company-backends-with-yasnippet ensime-company))


(defun yc/ensime-find-definition (pt)
  "Find definition at PT."
  (unless (fboundp 'ensime-edit-definition)
    (require 'ensime))
  (if (ensime-edit-definition nil)
      (yc/push-stack (cons 'ensime-pop-find-definition-stack nil))))

(defun yc/scala-mode-hook ()
  "My Hooks for scala-mode."
  (unless (featurep 'ensime)
    (require 'ensime))
  (add-to-list 'yc/find-def-func-list 'yc/ensime-find-definition))

(use-package scala-mode
  :mode (rx ".scala" eol)
  :hook ((scala-mode . yc/scala-mode-hook))
  :config
  (progn
    (modify-syntax-entry ?\" "\"" scala-syntax:syntax-table)))


(defun pgsql-perl-style ()
  "Perl style adjusted for PostgreSQL project."
  (interactive)
  (setq perl-brace-imaginary-offset 0)
  (setq perl-brace-offset 0)
  (setq perl-continued-statement-offset 2)
  (setq perl-continued-brace-offset (- perl-continued-statement-offset))
  (setq perl-indent-level 4)
  (setq perl-label-offset -2)
  ;; Next two aren't marked safe-local-variable, so .dir-locals.el omits them.
  (setq perl-indent-continued-arguments 4)
  (setq perl-indent-parens-as-block t)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(use-package cperl-mode
  :interpreter ("perl" . cperl-mode)
  :mode "\\.\\([pP][Llm]\\|al\\)$"
  :init
  (progn
    (custom-set-variables
     '(cperl-extra-newline-before-brace t )
     '(cperl-brace-offset -2              )
     '(cperl-merge-trailing-else nil      )))
  :hook ((perl-mode . (lambda ()
                        (when (and buffer-file-name
                                   (string-match "/postgres\\(ql\\)?/" buffer-file-name))
                          (pgsql-perl-style))))))

 ;; swig
(yc/add-compile-unit 'swig 55
 (when (or (equal ext "i")
           (equal ext "swig"))
   (lambda ()
     (interactive)
     (format "%s -c++ -java %s" (executable-find "swig") file))))


 ;; llvm
;; (use-package llvm-mode
;;   :mode (rx ".ll" eol)
;;   )

;; (use-package tablegen-mode
;;   :mode (rx ".td" eol))

;; (require 'autodisass-llvm-bitcode)

 ;; rust
(use-package rust-mode
  :hook ((rust-mode
          .
          (lambda ()
            (interactive)
            (yc/lsp--setup "rls" "rustup-init component add rls --toolchain'"))))
  :config
  (progn

    (defun yc/rust-trace-on ()
      "Turn on trace of rust.."
      (interactive)
      (setenv "RUST_BACKTRACE" "1"))

    (defun yc/rust-trace-off ()
      "Turn off trace of rust.."
      (interactive)
      (setenv "RUST_BACKTRACE" "0"))

    (defun yc/lsp--suggest-project-root-adv-rust (&rest args)
      "Advice for 'lsp--suggest-project-root'.
Call FUNC which is 'lsp--suggest-project-root with ARGS."
      (and (memq major-mode '(rust-mode))
           (when-let (dir (locate-dominating-file default-directory "Cargo.toml"))
             (expand-file-name dir)))
      )

    (advice-add 'lsp--suggest-project-root :before-until
                #'yc/lsp--suggest-project-root-adv-rust)

    (yc/add-compile-unit 'rust 60
      (if (locate-dominating-file default-directory "Cargo.toml")
          (lambda ()
            (interactive)
            (if (executable-find "cargo")
                (concat "cargo build" (if current-prefix-arg " --release"))
              (error "Can't find executable: cargo")))
        (when (equal major-mode 'rust-mode)
          (lambda ()
            (interactive)
            (if (executable-find "rustc")
                (concat "rustc " file)
              (error "Can't find executable: rustc"))))))

    (yc/add-run-unit 'rust 60
      (aif (locate-dominating-file default-directory "Cargo.toml")
          (lambda ()
            (interactive)
            (let* ((target (if current-prefix-arg "test" "run"))
                   (custom-script (concat (expand-file-name it) "my_script_"
                                          target)))
              (if (and (file-exists-p custom-script)
                       (file-executable-p custom-script))
                  custom-script
                (if (executable-find "cargo")
                    (concat "cargo "
                            (if current-prefix-arg
                                (if (listp current-prefix-arg)
                                    "test -- --nocapture"
                                  "test"
                                  )
                              "run"))
                  (error "Can't find executable: cargo")))))


        (when (equal major-mode 'rust-mode)
          (lambda ()
            (interactive)
            (if (executable-find "rustc")
                (concat "rustc " file " && ./" (file-name-sans-extension file))
              (error "Can't find executable: rustc")))))))
  :mode ((rx ".rs" buffer-end)))


(use-package lua-mode
  :commands (lua-mode)
  :mode (rx "." (or "lua") eow)
  )

(use-package typescript-mode
  :mode (rx "." (or "ts") buffer-end)
  :hook ((typescript-mode . lsp)))

(use-package go-mode
  :mode "\\.go\\'"
  )

(use-package groovy-mode
  :mode (rx "." (or "grovvy" "gradle") buffer-end)
  )



(provide '059-prog-others)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 059-prog-others.el ends here
