;;; 05-prog-common.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

(use-package which-func
  :commands (which-function-mode)
  :hook ((prog-mode . which-function-mode))
  :config
  (remove-hook 'after-change-major-mode-hook 'which-func-ff-hook)

  (defadvice! yc/which-func-update-adv (&rest args)
    "Apply function update to all windows.
ORIG-FUNC is called with ARGS."
    :override #'which-func-update
  (walk-windows
   (lambda (w)
     (when which-function-mode
       (which-func-update-1 w))) nil 'visible)))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-define-checker)
  :bind (:map flycheck-mode-map
              ([M-S-f9] . flycheck-first-error)
              ([S-f9] . flycheck-list-errors)
              ([f9] . flycheck-next-error)
              ([M-f9] . flycheck-previous-error))
  :hook ((c++-mode . (lambda ()
                       (setq flycheck-clang-language-standard "c++11"
                             flycheck-gcc-language-standard "c++11"))))
  :custom
  (flycheck-checker-error-threshold nil)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-flake8-maximum-line-length 120)

  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (flycheck-check-syntax-automatically
   '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (flycheck-display-errors-delay 0.25)

  :config
  (defadvice! yc/flycheck-next-error-adv (orig-func  &rest args)
    "Wrap to first error when tail reached.
Call ORIG-FUNC which is 'flycheck-next-error with ARGS."
    :around #'flycheck-next-error
    (condition-case var
        (apply orig-func args)
      (user-error (progn
                    (PDEBUG "Wrap to first error...")
                    (flycheck-first-error))))
    )
)

(use-package flycheck-popup-tip
  :ensure t
  :custom
  (flycheck-popup-tip-error-prefix "✕ ")
  :commands (flycheck-popup-tip-mode)
  :hook ((flycheck-mode . flycheck-popup-tip-mode)))

 ;; EMR.

(use-package list-utils :ensure t)
(use-package iedit :ensure t)

(use-package emr

  :bind (([S-f6] . emr-show-refactor-menu))
  :commands (emr-initialize)
  ;; :local-repo (expand-file-name "site-lisp/emr" user-emacs-directory)
  :custom
  (emr-clang-format-style '(("BasedOnStyle" . "LLVM")
                            ("IndentWidth" . "4")
                            ("BreakBeforeBraces" . "Linux")
                            ("AllowShortIfStatementsOnASingleLine" . "false")
                            ("AlignConsecutiveAssignments". "true")
                            ("AccessModifierOffset". -4)
                            ("IndentCaseLabels" . "false")
                            ("PointerAlignment" . "Left")
                            ("UseTab" . "Never"))))


 ;; Common Settings for prog-mode.
(yc/defmacro yc/add-keyword (sym type)
  `(font-lock-add-keywords
    nil (list (list ,sym 1 ,type t))))

;;;; Function and macro to add an regular expression string formed by (rx)
;;;; macro into specified face.

(defun yc/warning-keywords-matcher (limit)
  "Setup warning keywords.
LIMIT is the limit of search."
  (let ((case-fold-search t))
    (re-search-forward
     (rx (group (or "@" bow)
                    (or "bug" "fixme" "note" "todo" "todolist" "xxx" "yyc"
                        "deprecated"  "obsolete"
                        )
                    ":"))
     limit 'no-error)))

(defun setup-prog-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '((yc/warning-keywords-matcher
                                 1 font-lock-warning-face t)))

  (unless (member major-mode '(mhtml-mode html-mode nxml-mode))
    (yc/add-keyword (rx (repeat 120 not-newline) (group (+? not-newline)) eol) 'font-lock-warning-face))
  (yc/add-keyword (rx bow (group (or "NIL" "nil")) eow) 'font-lock-constant-face)
  )

 ;;;; CEDET Settings

;; Semantic mode.
(use-package semantic
  :commands (semantic-mode)
  :hook (;; (prog-mode . semantic-mode)
         (semantic-init . (lambda ()
                            (condition-case nil (imenu-add-to-menubar "TAGS") (error nil)))))
  :init
  (custom-set-variables
   '(semantic-default-submodes nil
                               (quote (global-semantic-decoration-mode
                                       global-semantic-idle-summary-mode
                                       global-semantic-idle-scheduler-mode
                                       global-semanticdb-minor-mode
                                       global-semantic-mru-bookmark-mode))
                               )
   '(semantic-idle-scheduler-idle-time 15)
   '(semantic-idle-scheduler-max-buffer-size 102400)
   '(semantic-lex-maximum-depth 20)
   '(pulse-flag 'never)
   '(semanticdb-default-save-directory (yc/make-cache-path "semanticdb"))
   '(srecode-map-save-file (yc/make-cache-path "srecode-map.el")))
  )

(use-package semantic/decorate/mode
  :defer t
  :config
  (progn
    (setq-default semantic-decoration-styles
                  '(("semantic-decoration-on-includes" . t)))))


;;;; Semanticdb 定制
;; Semantic DataBase存储位置
(use-package semantic/db-mode
  :commands (global-semanticdb-minor-mode)
  :config
  (setq-mode-local c-mode semanticdb-find-default-throttle
                   '(project unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
                   '(project unloaded system recursive)))

(use-package srecode/mode
  :commands (srecode-minor-mode)
  :hook ((prog-mode . srecode-minor-mode))
  :custom
  (srecode-insert-ask-variable-method 'field)
  :config
  (progn
    (dolist (dir '("~/.emacs.d/templates/srecode"
                   "~/.emacs.d/templates/srecode/private"))
      (add-to-list 'srecode-map-load-path (expand-file-name dir)))))


 ;; *************************** TAGS Database Settings *********************
(use-package counsel-xgtags
  :commands (counsel-xgtags-find-header counsel-xgtags-mode counsel-xgtags-find-definition
                                        counsel-xgtags-update-tags counsel-xgtags-parse-file)
  :custom
  (counsel-xgtags-kill-buffers nil)
  :config
  (yc/set-keys
   (list  (cons "M->" 'counsel-xgtags-find-definition))
   counsel-xgtags-mode-map)
  )

 ;; **** Unified Stack ****
(defvar yc/marker-stack (make-ring 256)
  "List of functions to run to return to previous context.")

(defun yc/push-stack (&optional m)
  "Add marker M at the beginning of `yc/marker-stack'."
  (ring-insert yc/marker-stack (or m (point-marker))))

(use-package gxref
  :ensure
  :commands (gxref-xref-backend))

(use-package ivy-xref
  :ensure
  :commands (ivy-xref-show-xrefs)
  :config
  (progn
    (advice-add 'ivy-xref-show-xrefs :after #'yc/ivy-xref-show-xrefs-adv)))

(use-package xref
  :commands (xref-backend-identifier-at-point  xref-find-backend)
  :config
  (progn
    (setq xref-show-xrefs-function 'ivy-xref-show-xrefs
          xref-show-definitions-function  'ivy-xref-show-defs)
    (setq-default xref-backend-functions '(gxref-xref-backend))))


(use-package semantic-uml
  :commands (uml/struct-to-dot uml/struct-to-dia uml/struct-to-puml)
  :bind (:map prog-mode-map
              ("\C-csD" . uml/struct-to-dot)
              ("\C-csd" . uml/struct-to-puml)
              (;; (kbd "C-c s M-d")
               [3 115 134217828] . uml/struct-to-puml-fields-only)))

 ;; lsp
(defvar yc/lsp-warned-mode-list nil "List of modes already been warn for disabling LSP.")

(defun yc/lsp--setup (executable install-tip)
  "Setup LSP.
Enable LSP if EXECUTABLE is t, and if `SETUP-FUNC' is not nil,
call this function to setup LSP.  Or show INSTALL-TIP."
  (PDEBUG "ENTER ys/lsp-setup, EXEC: "
          executable
          ", Available: " (or (executable-find executable)
          (file-executable-p executable)))
  (if (or (executable-find executable)
          (file-executable-p executable))
      (lsp)

    (unless (member major-mode yc/lsp-warned-mode-list)
      (add-to-list 'yc/lsp-warned-mode-list major-mode)
      (warn
       "LSP is disabled for %s since %s not found.%s"
       (symbol-name major-mode) executable
       (format "\n                Try install %s with command: '%s' to enable LSP."
               executable install-tip)))))

(use-package lsp-mode
  :preface
  (defun yc/modeline-update-lsp ()
    "Update `lsp-mode' status."
    (setq-local yc/modeline--lsp (lsp-mode-line)))

  (defun yc/lsp-format-adv (func &rest args)
    "Advice for 'lsp-format-buffer'.
Call FUNC which is 'lsp-format-buffer with ARGS."
    (let ((p (point)) )
      (apply func args)
      (if (= (point-min) (point))
          (goto-char p))))

  (defun yc/lsp-switch-client ()
    "Switch to another LSP server."
    (interactive)
    (require 'lsp-mode)

    (let ((matching-clients
           (mapcar (lambda (client)
                     (cons (lsp--client-server-id client)
                           (lsp--client-priority client)))
                   (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                #'lsp--server-binary-present?)))))
      (unless matching-clients
        (error "No matching lsp client was found"))

      (let* ((client (ivy-read "Choose client: " matching-clients))
             (match (car (lsp--filter-clients
                          (lambda (c) (eq  (lsp--client-server-id c) (intern client))))))
             (workspaces (lsp-workspaces)))
        (unless match
          (user-error "Couldn't find an LSP client named %S" client))

        (PDEBUG "CLIENT" (stringp client)
                "MATCH:" match)

        (let ((old-priority (lsp--client-priority match)))
          (setf (lsp--client-priority
                 (car (lsp--filter-clients
                       (lambda (c)
                         (eq  (lsp--client-server-id c) (intern client))))))
                9999)
          (unwind-protect
              (if workspaces
                  (lsp-workspace-restart
                   (if (cdr workspaces)
                       (lsp--completing-read "Select server: "
                                             workspaces
                                             'lsp--workspace-print
                                             nil t)
                     (car workspaces)))
                (lsp-mode +1))
            (setf (lsp--client-priority
                   (car (lsp--filter-clients
                         (lambda (c)
                           (eq  (lsp--client-server-id c) (intern client))))))
                  old-priority))))))

  :commands (lsp lsp-workspaces lsp--workspace-print lsp-format-region
                 lsp-format-buffer lsp-mode-line)
  :custom
  (lsp-diagnostic-package :auto)
  (lsp-restart 'interactive)
  (lsp-enable-imenu nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links t)
  (lsp-enable-snippet t)
  (lsp-completion-enable t)
  (lsp-auto-configure t)
  (lsp-log-io nil)
  (lsp-flycheck-live-reporting nil)
  (lsp-flycheck-default-level 'warn)
  ;; Auto-kill LSP server after last workspace buffer is killed.
  (lsp-keep-workspace-alive nil)
  ;; capf is the preferred completion mechanism for lsp-mode now
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))

  ;; Disable features that have great potential to be slow.
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-semantic-highlighting nil)

  ;; Don't modify our code without our permission
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)

  :hook
  ((lsp-after-open . (lambda ()
                       (setq-local xref-backend-functions
                                   (cons #'lsp--xref-backend
                                         xref-backend-functions)))))
  (lsp-mode . yc/modeline-update-lsp)
  (lsp-after . yc/modeline-update-lsp)


  :config
  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process)."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart)
        (funcall orig-fn restart)
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             300
             nil (lambda (workspace orig-fn)
                   (let ((lsp--cur-workspace workspace))
                     (unless (lsp--workspace-buffers lsp--cur-workspace)
                       (funcall orig-fn))))
             lsp--cur-workspace orig-fn))))

  (defadvice! yc/lsp-adv (orig-func  &rest args)
    "Advice for 'lsp'.
Call ORIG-FUNC which is 'lsp with ARGS.
Loading project specific settings before starting LSP."
    :around #'lsp
    ;; Load project-specific settings...
    (condition-case err
        (when (yc/lsp-load-project-configuration)
          ;; Calls lsp...
          (apply orig-func args)
          (flycheck-mode 1))
      (error nil))

    (unless (bound-and-true-p lsp-mode)
      (semantic-mode 1)
      (when (buffer-file-name)
        (semantic-force-refresh))))

  (advice-add 'lsp-format-buffer :around #'yc/lsp-format-adv)
  (advice-add 'lsp-format-region :around #'yc/lsp-format-adv)

  (defadvice! yc/lsp--imenu-create-index-adv (orig-func  &rest args)
    "Update cached symbols.
Call ORIG-FUNC which is 'lsp--imenu-create-index with ARGS."
    :around #'lsp--imenu-create-index
    (if (= yc/document-symbols-tick (buffer-chars-modified-tick))
        yc/cached-symbols

      (PDEBUG "Refreshing tags..." )
      (setq yc/document-symbols-tick (buffer-chars-modified-tick)
            yc/cached-symbols (apply orig-func args))))

  (defadvice! yc/lsp-enable-imenu-adv (&rest args)
    "Disable lsp-enable-imenu, which is very slow if there are lots of symbols.
ORIG-FUNC is called with ARGS."
    :before-until #'lsp-enable-imenu
    t)

  (defadvice! yc/lsp--suggest-project-root-adv (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :before-until #'lsp--suggest-project-root
    (when-let (root-file (yc/lsp-get-root-file))
      (expand-file-name (file-name-directory root-file))))

  (defadvice! yc/lsp-completion-mode-adv (&rest arg)
    "Remove `company-capf' from `company-backends'."
    :after  #'lsp-completion-mode
    (when (eq (car company-backends) 'company-capf)
      (pop company-backends))))



(defvar-local yc/cached-symbols nil "last cached index.")
(defvar-local yc/document-symbols-tick -1 "last tick of modification.")
(add-to-list 'auto-mode-alist (cons ".lsp-conf" 'emacs-lisp-mode))

(defun yc/lsp-get-root-file ()
  "Return root-file for lsp."
  (interactive)

  (defun normallize-file (file)
    (let ((fn (expand-file-name file)) )
      (if (s-ends-with-p "/" fn)
          (substring fn 0 -1)
        fn)))

  (let* ((f-list '(".lsp-conf" ".ccls-root" ".ccls" ".git"))
         (root-file
          (catch 'p-found
            (dolist (item f-list)
              (PDEBUG "YC/LSP-GET-ROOT-FILE: ITEM " item)
              (aif (locate-dominating-file default-directory item)
                  (throw 'p-found (concat (expand-file-name it) item)))))))

    (if (and root-file
             (string= (normallize-file (getenv "HOME"))
                      (normallize-file (file-name-directory root-file))))
        (setq root-file nil))

    (if (called-interactively-p)
        (message "Root file: %s." root-file))
    root-file))


(defun yc/lsp-get-log-file (client rootdir)
  "Get logfile name for specified workspace in ROOTDIR & CLIENT."
  (when (and (file-exists-p rootdir)
             (not (file-directory-p rootdir)))
    (setq rootdir (file-name-directory rootdir)))

  (format "%s%s_%s_%s.log" (temporary-file-directory)
          (aif (file-remote-p default-directory)
              (if (string-match
                   (rx bol "/" (or "ssh" "scp" "sudo") ":"
                       (group (+? nonl)) "@" (+? nonl) ":")
                   it)
                  (match-string 1 it)
                (error "File %s not handled"))
            user-login-name)
          client
          (let ((cmps (reverse (s-split "/" rootdir))))

                         (while (= (length (car cmps)) 0)
                           (pop cmps))
                         (or
                          (pop cmps)
                          "unamed"))))

(defun yc/lsp-load-project-configuration ()
  "Advice for 'lsp', loading project specific configurations."
  (interactive)
  (PDEBUG "MAJOR-MODE: " major-mode)

  (let* ((root-file (yc/lsp-get-root-file))
         (mode-specific-func
          (intern (concat "yc/lsp-load-project-configuration" "-"
                          (symbol-name major-mode)))))

    (PDEBUG "ROOT:" root-file)

    (if (and root-file
             (not (file-directory-p root-file))
             (string= ".lsp-conf" (file-name-nondirectory root-file))
             )
        (progn
          (PDEBUG "Loading from: " root-file)
          (with-temp-buffer
            (insert-file-contents root-file)
            (eval-buffer)
            (message "Loaded project configuration from %s" root-file))
          (when (fboundp mode-specific-func)
            (PDEBUG "Loading: " mode-specific-func)
            (funcall mode-specific-func root-file)))
      (user-error "LSP disabled at: %s, but no .lsp-conf is avaiable " root-file))

    (PDEBUG "leave")
    t))


(use-package imenu
  :commands (imenu--make-index-alist)
  :custom
  (imenu-auto-rescan-maxout (* 1024 1024)))


;;;; Common Program settings
(use-package prog-utils
  :commands (yc/doc-at-point yc/insert-single-comment yc/show-methods-dwim
                             yc/open-header)
  ;; :bind ((;; (kbd "M-m")
  ;;         [134217837] . yc/show-methods-dwim))
  :bind (:map esc-map
              ("." . yc/find-definitions)
              ("?" . yc/find-references)
              ("*" . yc/return-func)
              ("r" . yc/return-reflist)
              ("i" . yc/find-implementation)
              ("m" . yc/show-methods-dwim))
  :bind (([remap rectangle-mark-mode] . yc/store-current-location)))

;; (yc/set-keys `(,(cons (kbd "M-m") 'yc/show-methods-dwim)) nil)

(defun yc/show-doc-at-point ()
  "Show document."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-doc-glance)
    (yc/doc-at-point)))

(defun setup-prog-keybindings()
  "Common program-keybindings."
  (interactive)
  (local-set-key (kbd "M-|") 'align)
  (local-set-key "\C-co"    'yc/open-header)
  (local-set-key "\C-cp" 'semantic-ia-show-doc)
  (local-set-key "\C-cdp" 'yc/show-doc-at-point)
  (local-set-key "\C-cdP" 'yc/doc-at-point)

  ;;;; Keybindings for srecode
  (local-set-key "\C-cdc" 'srecode-document-insert-comment)
  (local-set-key "\C-cdf" 'srecode-document-insert-function-comment)
  (local-set-key "\C-cdh" 'yc/header-make)
  (local-set-key "\C-cde" 'yc/insert-empty-template)
  (local-set-key "\C-cds" 'yc/insert-single-comment)
  (local-set-key "\C-cdv" 'srecode-document-insert-variable-one-line-comment))

(defun yc/common-program-hook ()
  "My program hooks."
  (PDEBUG
    "FILE: " (or buffer-file-name (current-buffer))
    "MODE: " major-mode)

  (setq show-trailing-whitespace t)
  (setup-prog-keywords)
  (setup-prog-keybindings)
  (emr-initialize)

  (flycheck-mode 1))

(use-package prog-mode
  :defer t
  :hook ((prog-mode . yc/common-program-hook)))


(cdsq yc/compile-commands nil
  "List of CompileUnit to be checked for `compile'.
Each CompileUnit is actually a function who accepts two parameters:
`file' : name of file & `ext' extension name.
This function returns a string as compile command, or nil if it can't handle
  the input file.")

(cdsq yc/run-commands nil
  "List of CompileUnit to be checked for `run'.
Each CompileUnit is actually a function who accepts two parameters:
`file' : name of file & `ext' extension name.
This function returns a string as compile command, or nil if it can't handle
  the input file.")


(yc/defmacro yc/add-unit-to-target (target name priority &rest body)
  "Append to an unit to `TARGET'.
`NAME' name of this unit, for debug purpose.
`BODY' function to call, should return a command, or nil.
`PRIORITY' priority of this unit, if multiple units are available, unit with higher priority
  should be chosen."
  `(if (numberp ,priority)
       (add-to-list ',target (list :name ,name
                                   :priority ,priority
                                   :func (lambda (file ext) ,@body)))
     (error "Priority of %s should be a number" ,name)))

(yc/defmacro yc/add-compile-unit (name priority &rest body)
  "Append to yc/compile-commands."
  `(yc/add-unit-to-target yc/compile-commands ,name ,priority ,@body))

(yc/defmacro yc/add-run-unit (name priority &rest body)
  "Append to yc/run-commands."
  `(yc/add-unit-to-target yc/run-commands ,name ,priority ,@body))

(yc/add-compile-unit 'makefile 99
  (aif (or (yc/file-exists-p "makefile")
           (yc/file-exists-p "Makefile"))
      (lambda ()
        (counsel-make nil t t))))

(defun get-command-from-list (commands)
  "Get command from LST."
  (let* ((file (if buffer-file-name (file-name-nondirectory buffer-file-name)))
         (ext (if file (file-name-extension file)))
         best)
    (dolist (unit commands)
      (let ((name (plist-get unit :name))
            (priority (plist-get unit :priority)))
        (PDEBUG "Checking unit: " name)
        (awhen (funcall (plist-get unit :func) file ext)
          (when (or (not best) ;; never set
                    (> priority (plist-get best :priority))) ;; new one has higher priority
            (PDEBUG "Updating best match: "
              (plist-get best :name) "("(plist-get best :priority)") ==> "
              name"("priority")")
            (setq best (list :name name :priority priority :cmd it))))))

    (PDEBUG "FINAL: " best)
    (awhen (plist-get best :cmd)
      (funcall it))))

(defun get-compile-command()
  "Return command to compile current target."
  (get-command-from-list yc/compile-commands))

(defun get-run-command()
  "Return command to run current target."
  (get-command-from-list yc/run-commands))


(provide '05-prog-common)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 05-prog-common.el ends here
