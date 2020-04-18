;;; 06-prog-common.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:
 ;;
(use-package projectile
  :commands (projectile-project-root projectile-find-other-file)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-other-file-alist
   '( ;; handle C/C++ extensions
     ("cpp" . ("h" "hh" "hpp" "ipp"))
     ("ipp" . ("h" "hh" "hpp" "cpp"))
     ("hpp" . ("h" "hh" "ipp" "cpp" "cc"))
     ("cxx" . ("h" "hh" "hxx" "ixx"))
     ("ixx" . ("h" "hh" "hxx" "cxx"))
     ("hxx" . ("h" "hh" "ixx" "cxx"))
     ("c"   . ("h" "hh"))
     ("m"   . ("h" "hh"))
     ("mm"  . ("h" "hh"))
     ("h"   . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
     ("hh"   . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
     ("cc"  . ("h"  "hh""hh" "hpp"))
     ("hh"  . ("cc"))

     ;; vertex shader and fragment shader extensions in glsl
     ("vert" . ("frag"))
     ("frag" . ("vert"))

     ;; handle files with no extension
     (nil    . ("lock" "gpg"))
     ("lock" . (""))
     ("gpg"  . (""))
     )))

 ;; which-func
(defun yc/which-func-update (&rest args)
  "Advice for `which-func-update'.
Update the Which-Function mode display for all windows."
  (walk-windows
   (lambda (w)
     (when which-function-mode
       (which-func-update-1 w))) nil 'visible))

(advice-add 'which-func-update :override #'yc/which-func-update)

(use-package which-func
  :commands (which-function-mode)
  :hook ((prog-mode . which-function-mode)))

 ;; Flycheck.. XXX: flycheck- -- tmp-file move to /tmp

(use-package flycheck
  :ensure t
  :pin melpa
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
  (flycheck-flake8-maximum-line-length 120)
  (flycheck-c/c++-gcc-executable "g++")
  (flycheck-c/c++-clang-executable "clang++")
  (flycheck-gcc-warnings
   (quote ("all" "extra" "no-unused-parameter"
           "no-overloaded-virtual"
           "no-unknown-pragmas"
           "no-unused-local-typedefs")))
  (flycheck-clang-warnings
   (quote ("all" "extra" "no-unused-parameter"
           "no-overloaded-virtual"
           "no-unknown-pragmas"
           "no-unused-local-typedefs")))
  :config
  (progn
    (defun yc/flycheck-next-error-adv (func &rest args)
      "Advice for 'flycheck-next-error'.
Call FUNC which is 'flycheck-next-error with ARGS."
      (condition-case var
          (apply func args)
        (user-error (progn
                      (PDEBUG "Wrap to first error...")
                      (flycheck-first-error)))))

    (advice-add 'flycheck-next-error :around #'yc/flycheck-next-error-adv)

    ))

(use-package flycheck-popup-tip
  :commands (flycheck-popup-tip-mode)
  :hook ((flycheck-mode . flycheck-popup-tip-mode)))



 ;; EMR.
(use-package emr
  :bind (([S-f6] . emr-show-refactor-menu))
  :commands (emr-initialize)
  :pin melpa
  :custom
  (emr-clang-format-style '(("BasedOnStyle" . "LLVM")
                            ("IndentWidth" . "4")
                            ("BreakBeforeBraces" . "Linux")
                            ("AllowShortIfStatementsOnASingleLine" . "false")
                            ("AlignConsecutiveAssignments". "true")
                            ("AccessModifierOffset". -4)
                            ("IndentCaseLabels" . "false")
                            ("PointerAlignment" . "Left")
                            ("UseTab" . "Never")))
  :hook ((emacs-startup . emr-initialize)))




 ;; Common Settings for prog-mode.
(yc/defmacro yc/add-keyword (sym type)
  `(font-lock-add-keywords
    nil (list
         (list ,sym 1 ,type t ))))

;;;; Function and macro to add an regular expression string formed by (rx)
;;;; macro into specified face.
(defun setup-prog-keywords ()
  "Highlight additional keywords."
  (let ((r-match-warning
         (rx (group (or "@" bow)
                    (or "bug" "fixme" "note" "todo" "todolist" "xxx" "yyc"
                        "BUG" "FIXME" "NOTE" "TODO" "TODOLIST" "XXX" "YYC"
                        )
                    ":")))
        (r-match-longline
         (rx (repeat 120 not-newline) (group (+? not-newline)) eol))
        (r-match-const (rx bow (group (or "NIL" "nil")) eow)))

    (yc/add-keyword r-match-warning 'font-lock-warning-face)
    (unless (member major-mode '(mhtml-mode html-mode nxml-mode))
        (yc/add-keyword r-match-longline 'font-lock-warning-face))

    (yc/add-keyword r-match-const 'font-lock-constant-face)))

 ;;;; CEDET Settings
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2012-04/msg00430.html
(unless (boundp 'x-max-tooltip-size)
  (setq x-max-tooltip-size (cons 80 40)))

;; Semantic mode.
(use-package semantic
  :commands (semantic-mode)
  :hook (;; (prog-mode . semantic-mode) ;; TODO: should we enable this, if lsp not avaiable?
         (semantic-init . (lambda ()
                            (condition-case nil (imenu-add-to-menubar "TAGS") (error nil)))))
  :init
  (custom-set-variables
   '(semantic-default-submodes nil
                               ;; (quote (;; global-semantic-decoration-mode
                               ;;         global-semantic-idle-summary-mode
                               ;;         global-semantic-idle-scheduler-mode
                               ;;         global-semanticdb-minor-mode
                               ;;         global-semantic-mru-bookmark-mode))
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
(defun yc/print-cur-tag ( )
  "Print current tag."
  (interactive)
  (let ((tag (semantic-current-tag)))
    (if tag
        (PDEBUG tag)
      (error "No tag retrived."))))

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
  "Add FUNC at the beginning of `yc/marker-stack'."
  (ring-insert yc/marker-stack (or m (point-marker))))

(defun yc/store-current-location ()
  "Store current location (PT)."
  (interactive)
  (yc/push-stack)
  (if (called-interactively-p)
      (message "Location saved...")))

(use-package gxref
  :commands (gxref-xref-backend))

(use-package ivy-xref
  :commands (ivy-xref-show-xrefs))

(use-package xref
  :commands (xref-backend-identifier-at-point  xref-find-backend)
  :config
  (progn
    (setq xref-show-xrefs-function 'ivy-xref-show-xrefs
          xref-show-definitions-function  'ivy-xref-show-defs)
    (setq-default xref-backend-functions '(gxref-xref-backend))))

(defvar xref-ivy-last nil "Last state used by ivy-xref-show-xrefs.")

(defun yc/ivy-xref-show-xrefs-adv (&rest args)
  "Advice for 'ivy-xref-show-xrefs'.
Call FUNC which is 'ivy-xref-show-xrefs with ARGS."
  (setq xref-ivy-last ivy-last))

(advice-add 'ivy-xref-show-xrefs :after #'yc/ivy-xref-show-xrefs-adv)

(defun yc/return-reflist ()
  "Return to last reference list."
  (interactive)
  (unless xref-ivy-last
    (error "Stack is empty"))

  (let ((ivy-last xref-ivy-last) )
    (ivy-resume)))




(defun yc/find-definitions-xgtags ()
  "Goto definition with xgtags."
  (interactive)
  (counsel-xgtags-find-definition))

(defun lsp-ui-find-workspace-symbol (pattern)
  "List project-wide symbols matching the query string PATTERN."
  (interactive (list (read-string
                      "workspace/symbol: "
                      nil 'xref--read-pattern-history)))
)

(defun yc/find-definitions-xref ()
  "Description."
  (interactive)

  (PDEBUG "T: " (xref-find-backend))
  (PDEBUG "F: " (xref-backend-identifier-at-point (xref-find-backend)))


  (condition-case msg
      (let ((identifier (xref-backend-identifier-at-point (xref-find-backend))))
        (PDEBUG "ID: " identifier "LEN: " (length identifier))
        (if (and identifier
                 (> (length identifier) 0))
            (xref--find-definitions identifier nil)

          ;; if identifier is invalid, try apropos...
          (let ((pattern (read-string
                          "workspace/symbol: "
                          nil 'xref--read-pattern-history)) )
            (xref--find-xrefs pattern 'apropos pattern nil)))

        ;; Don't use marker-ring of xref, I'm using my own stack.
        (unless (ring-empty-p xref--marker-ring)
          (ring-remove xref--marker-ring 0))
        t)

    ('error (progn (PDEBUG "Failed to find definition with xref.") nil))))


(defvar-local yc/find-def-func-list nil
  "List of functions can be used when finding definitions.
If a function succeeded in finding a definition, it should push a method which can
  be used to return to previous position, and then returns t. Otherwise, return nil.
Also, ")

(setq-default yc/find-def-func-list '(yc/find-definitions-xref yc/find-definitions-xgtags))


(defun yc/prog-try-function-list (func-list tip)
  "Call functions in `FUNC-LIST..."

  (catch 'd-found
    (dolist (func func-list)
      (condition-case var
          (progn
            (PDEBUG "Func: " func)
            (if (and func (funcall func))
                (throw 'd-found t)
              (PDEBUG tip ":"
                (format "%s returns nil:, trying others.."
                        (symbol-name func)))))

        (error (PDEBUG tip ":" func var))))

    (error "Failed to %s" tip)))

(defun yc/find-definitions ()
  "Goto definition of symbol."
  (interactive)
  (let ((m (point-marker)))
    (condition-case msg
        (progn
          (yc/prog-try-function-list yc/find-def-func-list "find-definitions")
          (yc/push-stack m))
      ('error (PDEBUG "Failed to find definition.")))))


(defun yc/find-references-xgtags ()
  "Goto definition with xgtags."
  (interactive)
  (counsel-xgtags-find-reference))

(defun yc/find-references-xref ()
  "Description."
  (interactive)

  (condition-case msg
      (xref-find-references (xref-backend-identifier-at-point (xref-find-backend)))
    ('error (PDEBUG "Failed to find reference with xref."))))

(defvar-local yc/find-ref-func-list '(yc/find-references-xref yc/find-references-xgtags)
  "List of functions can be used when finding references.
Return t if succeeded, or nil otherwise.")


(defun yc/find-implementation ()
  "Description."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (let ((m (point-marker)) )
        (lsp-find-implementation)
        (yc/push-stack m))

    (error "Only works for lsp-mode for now.")))

(defun yc/find-references ()
  "Goto definition of symbol."
  (interactive)
  (let ((m (point-marker)))
    (condition-case msg
        (progn
          (yc/prog-try-function-list yc/find-ref-func-list "find-references")
          (yc/push-stack m))
      ('error (PDEBUG "Failed to find reference."))))
  )

(yc/set-keys
 (list (cons "." 'yc/find-definitions)
       (cons "?" 'yc/find-references)
       (cons "*" 'yc/return-func)
       (cons "r" 'yc/return-reflist)
       (cons "i" 'yc/find-implementation))
 esc-map)



(defun yc/find-header-lsp ()
  "Find header file based on lsp."
  (interactive)

  (unless lsp-mode
    (error "lsp-mode not enabled for this buffer"))

  (let ((overlays (overlays-in (point-at-bol) (point-at-eol))))
    (PDEBUG "OVERLAYS:" overlays)
    (catch 'p-found
      (dolist (overlay overlays)
        (let ((pops (overlay-properties overlay)))
          (PDEBUG "POPS: " pops)
          (case (overlay-get overlay 'category)
            ('default-button
              (let* ((action (overlay-get overlay 'action))
                     (res (funcall action overlay)))
                (PDEBUG "ACTION: " action
                  "RES:  " res)
                (if res
                    (throw 'p-found t)))))))
      nil)))


(defun yc/open-header ()
  "Open header file lied under current PT."
  (interactive)

  (let ((m (point-marker)))
    (condition-case msg
        (progn
          (yc/prog-try-function-list
           '(yc/find-header-lsp yc/find-definitions-xref counsel-xgtags-find-header)
           "find-header")
          (yc/push-stack m))
      ('error (PDEBUG "Failed to find header.")))))

(defun yc/return-func()
  "Return to previous tag."
  (interactive)

  (when (ring-empty-p yc/marker-stack)
    (error "Marker stack is empty"))

  (let ((marker (ring-remove yc/marker-stack 0)))
    (switch-to-buffer (or (marker-buffer marker)
                          (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)
    (run-hooks 'xref-after-return-hook)))


(use-package semantic-uml
  :commands (uml/struct-to-dot uml/struct-to-dia uml/struct-to-puml))

 ;; lsp
(use-package lsp-ui-imenu
  :commands (lsp-ui-imenu-enable))

(use-package lsp-ui-flycheck
  :commands (lsp-ui-flycheck-enable)
  :init
  (progn
    (custom-set-variables
     '(lsp-ui-flycheck-live-reporting t))))

(defvar yc/lsp-warned-mode-list nil "List of modes already been warn for disabling LSP.")

(defvar yc/lsp-server-dir (expand-file-name "~/.local/lsp/") "Nil.")


(defun yc/lsp--setup (executable install-tip &optional setup-func)
  "Enable LSP if EXECUTABLE is t."
  (if (or (executable-find executable)
          (file-executable-p executable))
      (progn
        (when setup-func
          (PDEBUG "setting up: " setup-func)
          (funcall setup-func))

        (lsp)
        )

    (unless (member major-mode yc/lsp-warned-mode-list)
      (add-to-list 'yc/lsp-warned-mode-list major-mode)
      (warn
       "LSP is disabled for %s since %s not found.%s"
       (symbol-name major-mode) executable
       (format "\n                Try install %s with command: '%s' to enable LSP."
               executable install-tip)))))

(use-package lsp-mode
  :commands (lsp lsp-workspaces lsp--workspace-print lsp-format-region lsp-format-buffer)
  :ensure t
  :pin melpa
  :custom
  (lsp-diagnostic-package 'auto)
  (lsp-restart 'ignore)
  (lsp-enable-file-watchers nil)
  (lsp-enable-indentation nil)
  (lsp-enable-imenu nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links t)
  (lsp-enable-snippet t)
  (lsp-auto-configure t)
  (lsp-log-io t)
  (lsp-flycheck-live-reporting nil)
  (flycheck-check-syntax-automatically '(save idle-change))

  :hook
  ((lsp-after-open . (lambda ()
                       (setq-local xref-backend-functions
                                   (cons #'lsp--xref-backend xref-backend-functions))))

   ;; mode specific hooks.
   ))

;; advice for format-buffer & format-region: save execution before format.
;; some servers (pyls) will move point to other unexpected place....

(defun yc/lsp-format-adv (func &rest args)
  "Advice for 'lsp-format-buffer'.
Call FUNC which is 'lsp-format-buffer with ARGS."
  (let ((p (point)) )
    (apply func args)
    (if (= (point-min) (point))
        (goto-char p))))

(advice-add 'lsp-format-buffer :around #'yc/lsp-format-adv)
(advice-add 'lsp-format-region :around #'yc/lsp-format-adv)

(defun yc/modeline-update-lsp (&rest args)
  "Update `lsp-mode' status."
  (set (make-local-variable 'yc/modeline--lsp)
       (if-let (workspaces (lsp-workspaces))
           (concat " LSP"
                   (string-join
                    (--map
                     (format "[%s:%s]"
                             (-> it lsp--workspace-client lsp--client-server-id symbol-name (propertize 'face 'bold-italic))
                             (propertize (format "%s" (process-id (lsp--workspace-cmd-proc it))) 'face 'italic))
                     workspaces)))
          (concat " LSP" (propertize "[Disconnected]" 'face 'warning)))
       )
  )

(add-hook 'lsp-mode-hook #'yc/modeline-update-lsp)
(add-hook 'lsp-after-uninitialized-hook #'yc/modeline-update-lsp)

(defun yc/lsp-kill ()
  "Kill LSP of current workspace."
  (interactive)
  (let* ((pids (-map (lambda (x)
                       (--> x lsp--workspace-cmd-proc process-id number-to-string))
                     (lsp-workspaces)))
         (len (length pids))
         (pid-str (mapconcat 'identity pids " "))
         (cmd (if (or (= len 1)
                      (and (> len 1)
                           (yes-or-no-p
                            (format "Going to kill multiple processes: %s, continue? "
                                    pid-str))))
                  (format "kill -9 %s" pid-str))))

    (when cmd
      (PDEBUG "KILL: " cmd)
      (shell-command-to-string cmd))))

(defun yc/lsp-restart ()
  "Restart lsp."
  (interactive)
  (yc/lsp-kill)
  (lsp))

(defalias 'yc/kill-lsp 'yc/lsp-kill)


(defvar-local yc/cached-symbols nil "last cached index.")
(defvar-local yc/document-symbols-tick -1 "last tick of modification.")

(defun yc/lsp--imenu-create-index-adv (func &rest args)
  "Advice for 'lsp--imenu-create-index'.
Call FUNC which is 'lsp--imenu-create-index with ARGS."
  (if (= yc/document-symbols-tick (buffer-chars-modified-tick))
      yc/cached-symbols

    (PDEBUG "Refreshing tags..." )
    (setq yc/document-symbols-tick (buffer-chars-modified-tick)
          yc/cached-symbols (apply func args))))

(advice-add 'lsp--imenu-create-index :around #'yc/lsp--imenu-create-index-adv)


;; does nothing but disable lsp--imenu, which is very slow if there are lots of symbols...
(defun yc/lsp-enable-imenu-adv (&rest args)
  "Advice for 'lsp-enable-imenu'.
Call FUNC which is 'lsp-enable-imenu with ARGS."
  t)

(advice-add 'lsp-enable-imenu :before-until #'yc/lsp-enable-imenu-adv)



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
              (PDEBUG "ITEM: " item)
              (aif (locate-dominating-file default-directory item)
                  (throw 'p-found (concat (expand-file-name it) item)))))))

    (if (and root-file
             (string= (normallize-file (getenv "HOME"))
                      (normallize-file (file-name-directory root-file))))
        (setq root-file nil))

    (if (called-interactively-p)
        (message "Root file: %s." root-file))
    root-file))

(defun yc/lsp-load-project-configuration ()
  "Advice for 'lsp', loading project specific configurations."
  (interactive)
  (PDEBUG "MAJOR-MODE: " major-mode)

  (let* ((root-file (yc/lsp-get-root-file))
         (mode-specific-func
          (intern (concat "yc/lsp-load-project-configuration" "-"
                          (symbol-name major-mode)))))

    (when root-file
      (PDEBUG "ROOT:" root-file)
      ;; loading project specific settings from .ccls-root
      (if (and root-file
               (not (file-directory-p root-file))
               (> (file-attribute-size (file-attributes root-file)) 10))
          (with-temp-buffer
            (insert-file-contents root-file)
            (eval-buffer)
            (message "Loaded project configuration from %s" root-file))))


    (when (fboundp mode-specific-func)
      (PDEBUG "Loading: " mode-specific-func)
      (funcall mode-specific-func root-file))

    (PDEBUG "leave")))

(defun yc/lsp-adv (func &rest args)
  "Advice for 'lsp'.
Call FUNC which is 'lsp with ARGS."

  ;; turn off flycheck mode before turning lsp..
  (flycheck-mode -1)

  ;; Load project-specific settings...
  (yc/lsp-load-project-configuration)

  ;; Calls lsp...
  (apply func args)

  ;; functions to run after lsp...
  (lsp-ui-flycheck-enable t)
  (unless (featurep 'company-lsp)
    (require 'company-lsp))

  (yc/add-company-backends-with-yasnippet company-lsp)
  (flycheck-mode 1))

(advice-add 'lsp :around #'yc/lsp-adv)


(defun yc/lsp--suggest-project-root-adv (&rest args)
  "Advice for 'ccls--suggest-project-root': locate .lsp-conf if possible.
Call FUNC which is 'ccls--suggest-project-root with ARGS."
  (when-let (root-file (yc/lsp-get-root-file))
    (expand-file-name (file-name-directory root-file))))

(advice-add 'lsp--suggest-project-root :before-until
            #'yc/lsp--suggest-project-root-adv)

;; lsp extras
(use-package lsp-ui
  :defer t
  :pin melpa
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  :hook ((lsp-mode . lsp-ui-mode))
  )

(use-package lsp-ui-doc
  :commands (lsp-ui-doc-glance)
  :custom
  (lsp-ui-doc-enable nil)
  )


(use-package company-lsp
  :ensure t
  :custom
  (company-lsp-cache-candidates 'auto)
  :commands (company-lsp))


(use-package imenu
  :commands (imenu--make-index-alist)
  :custom
  (imenu-auto-rescan-maxout (* 1024 1024))
  )


;;;; Common Program settings

(defvar yc/imenu--overlays nil
  "Store overlays.")

(defun yc/imenu--cleanup ()
  "Clean up the overlays."
  (while yc/imenu--overlays
    (delete-overlay (pop yc/imenu--overlays))))

(defun yc/imenu--add-overlay ()
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds.
WND, when specified is the window."
  (let ((ov               (make-overlay
                           (line-beginning-position)
                           (1+ (line-end-position)))))
    (overlay-put ov 'face 'swiper-line-face)
    (push ov yc/imenu--overlays)
    ))

(defun yc/counsel-imenu-update-fn()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (yc/imenu--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((regexp-or-regexps (funcall ivy--regex-function ivy-text))
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))

        (dolist (re regexps)
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re))
                 (num (get-text-property 0 'swiper-line-number (ivy-state-current ivy-last))))
            (unless (memq this-command '(ivy-yank-word
                                         ivy-yank-symbol
                                         ivy-yank-char
                                         scroll-other-window))
              (when (cl-plusp num)
                (goto-char num)
                (isearch-range-invisible (line-beginning-position)
                                         (line-end-position))
                (unless (and (>= (point) (window-start))
                             (<= (point) (window-end (ivy-state-window ivy-last) t)))
                  (recenter 3))
                ))
            (yc/imenu--add-overlay)))))))


(defun yc/lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (seq-filter
     #'identity
     (seq-map #'yc/lsp--symbol-to-hierarchical-imenu-elem
              (seq-sort #'lsp--imenu-symbol-lessp
                        (lsp--imenu-filter-symbols symbols))))))

(defun yc/counsel-imenu-get-candidates-from (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (progn
    (cl-mapcan (lambda (elm)
                 (PDEBUG "ELM:" elm
                   "PFX: " prefix)
                 (if (imenu--subalist-p elm)
                     (progn
                       (PDEBUG "SUBALIST")
                       (yc/counsel-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        ;; pass the prefix to next recursive call
                        (concat prefix (if prefix ".") (car elm))))
                   (let ((key (concat
                               (when prefix
                                 (if (get-text-property 0 'face prefix)
                                     (format " (%s) " prefix)

                                   (concat
                                    (propertize prefix 'face 'ivy-grep-info)
                                    ": ")))
                               (let ((name (car elm)))
                                 (if (get-text-property 0 'face name)
                                     name
                                   (propertize name 'face (yc/imenu--get-symbol-face prefix))
                                   )
                                 )
                               )))
                     (list (cons key
                                 (put-text-property
                                  0 1 'swiper-line-number
                                  (marker-position (cdr elm)) key))))))
               alist)))


(cdsq yc/lsp--symbol-face
  '(;; (1 . "File")
    (2 . 'font-lock-constant-face) ;; "Module"
    (3 . 'font-lock-constant-face) ;; "Namespace"
    ;; (4 . "Package")
    (5 . 'font-lock-type-face) ;;"Class"
    (6 . 'font-lock-function-name-face) ;; "Method"
    (7 . 'font-lock-variable-name-face) ;; "Property"
    (8 . 'font-lock-variable-name-face)
    (9 . 'font-lock-function-name-face) ;; "Constructor"
    (10 . 'font-lock-constant-face) ;; "Enum"
    (11 . 'font-lock-function-name-face) ;; "Interface"
    (12 . 'font-lock-function-name-face) ;; Function
    (13 . 'font-lock-variable-name-face) ;; Variables
    (14 . 'font-lock-constant-face)      ;; Constant
    (15 . 'font-lock-string-face)        ;;
    ;; (16 . "Number")
    ;; (17 . "Boolean")
    ;; (18 . "Array")
    ;; (19 . "Object")
    ;; (20 . "Key")
    ;; (21 . "Null")
    ;; (22 . "Enum Member")
    (23 . 'font-lock-type-face) ;; "Struct"
    ;; (24 . "Event")
    ;; (25 . "Operator")
    ;; (26 . "Type Parameter")
    )
  "")

(defun yc/lsp--get-symbol-face (sym)
  "The face for the kind of SYM."
  (-> (gethash "kind" sym)
      (assoc yc/lsp--symbol-face)
      (cdr)

      (or 'default)))


(cdsq yc/imenu--symbol-face
  '(
    ("Packages" . 'font-lock-constant-face)
    ("Module" . 'font-lock-constant-face)
    ("Class" . 'font-lock-type-face) ;;"Class"
    ("Module" . 'font-lock-constant-face) ;;"Class"
    (6 . 'font-lock-function-name-face) ;; "Method"
    ("Variables" . 'font-lock-variable-name-face) ;; "Property"
    ("Variable" . 'font-lock-variable-name-face) ;; "Property"
    (8 . 'font-lock-variable-name-face)
    (9 . 'font-lock-function-name-face) ;; "Constructor"
    (10 . 'font-lock-constant-face) ;; "Enum"
    ("Functions" . 'font-lock-function-name-face) ;; "Interface"
    ("Function" . 'font-lock-function-name-face) ;; "Interface"
    (12 . 'font-lock-function-name-face) ;; Function
    (13 . 'font-lock-variable-name-face) ;; Variables
    (14 . 'font-lock-constant-face)      ;; Constant
    (15 . 'font-lock-string-face)        ;;
    ;; (16 . "Number")
    ;; (17 . "Boolean")
    ;; (18 . "Array")
    ;; (19 . "Object")
    ;; (20 . "Key")
    ;; (21 . "Null")
    ;; (22 . "Enum Member")
    (23 . 'font-lock-type-face) ;; "Struct"
    ;; (24 . "Event")
    ;; (25 . "Operator")
    ;; (26 . "Type Parameter")
    )
  "")

(defun yc/imenu--get-symbol-face (sym)
  "The face for the kind of SYM."
  (-> sym
      (assoc yc/imenu--symbol-face)
      (cdr)
      (or 'default)))

(defun yc/lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (if (string= "Other" (lsp--get-symbol-type sym))
      nil
    (let* ((start-point (lsp--symbol-get-start-point sym))
           (name (gethash "name" sym))
           (ret (if (seq-empty-p (gethash "children" sym))
                    (cons (concat
                           (propertize (lsp--get-symbol-type sym) 'face 'ivy-grep-info)
                           ": "
                           (propertize name  'face (yc/lsp--get-symbol-face sym))
                           )
                          start-point)
                  (cons   (propertize name  'face (yc/lsp--get-symbol-face sym))
                          (cons (cons
                                 (propertize (lsp--get-symbol-type sym)  'face 'ivy-grep-info)
                                 start-point)
                                (yc/lsp--imenu-create-hierarchical-index (gethash "children" sym)))))))
      (PDEBUG "RET: " ret)
      ret)))

(defun yc/lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (PDEBUG "SYM: " sym)

  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym))
         (ret (cons (if (and lsp-imenu-show-container-name container)
                        (concat container lsp-imenu-container-name-separator name)
                      name)
                    start-point)))

    (PDEBUG "RET2: " ret)
    ret))


(defun yc/counsel-imenu-get-candidates (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (condition-case msg
      (yc/counsel-imenu-get-candidates-from alist prefix)
    (error (progn (PDEBUG "FAIL: " msg)    nil))))


(defvar-local yc/cached-tags nil "last cached index.")
(defvar-local yc/document-tags-tick -1 "last tick of modification.")

(defun yc/tags-from-imenu ()
  "Get tags from imenu.
If NO-CACHED is true, do not use cached value."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))

  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items))
         (items (counsel-imenu-categorize-functions items))
         (tags (yc/counsel-imenu-get-candidates items)))

    (if (called-interactively-p 'interface)
        (let ((yc/debug-log-limit -1))
          (PDEBUG "IMENU-FUNC: " imenu-create-index-function)
          (PDEBUG "TAGS: " tags)))

    (if tags
        (PDEBUG "TAGS from imenu"))
    tags))

(defun yc/tags-from-semantic ()
  "Get tags from semantic."
  (interactive)
  (if (semantic-active-p)
      (let ((items
             (mapcar
              (lambda (x)
                (let ((str (counsel-semantic-format-tag x)))
                  (put-text-property
                   0 1 'swiper-line-number (semantic-tag-start x) str)
                  str
                  ))
              (counsel-semantic-tags))) )

        (if (called-interactively-p 'interactive)
            (let ((yc/debug-log-limit -1))
              (PDEBUG "IMENU-ITEMS: " items)))

        (if items
            (PDEBUG "TAGS from semantic"))

        items)))


(defun yc/tags-from-lsp (&optional no-cached)
  "Get tags from imenu.
If NO-CACHED is true, do not use cached value."
  (interactive "P")

  (when no-cached
    (setq yc/document-tags-tick -1))

  ;; update tags should happen only when:
  (when (and (not (= yc/document-tags-tick
                     (buffer-chars-modified-tick)))        ;; timestamp changes
             (bound-and-true-p lsp-mode)                   ;; lsp-mode is enabled.
             (lsp--capability "documentSymbolProvider")    ;; Can provide symbol info.
             )

    (PDEBUG "Refreshing tags..." )
    (condition-case var
        (let* ((symbols (lsp--get-document-symbols))
               (filted-symbols (lsp--imenu-filter-symbols symbols))
               (items
                (if (lsp--imenu-hierarchical-p symbols)
                    (yc/lsp--imenu-create-hierarchical-index symbols)
                  (seq-map (lambda (nested-alist)
                             (cons (car nested-alist)
                                   (seq-map #'yc/lsp--symbol-to-imenu-elem (cdr nested-alist))))
                           (seq-group-by #'lsp--get-symbol-type filted-symbols)))))

          (when (called-interactively-p 'interactive)
              (PDEBUG "SYM: " symbols)
              (PDEBUG "ITEMS" items))

          (setq yc/document-tags-tick (buffer-chars-modified-tick)
                yc/cached-tags (yc/counsel-imenu-get-candidates items))
          )
      (error (PDEBUG "ERROR: " var)
             (setq yc/cached-tags nil)))

    (if (called-interactively-p 'interactive)
        (let ((yc/debug-log-limit 4096))
          (PDEBUG "IMENU-ITEMS: " yc/cached-tags))))

  (if yc/cached-tags
      (PDEBUG "TAGS from LSP"))

  yc/cached-tags)

(defun yc/counsel-imenu-or-semantic ()
  "List functions and go to selected one."
  (interactive)
  (let ((position (point))
        (candidates (or (yc/tags-from-lsp)
                        (yc/tags-from-semantic)
                        (yc/tags-from-imenu)
                        ))
        res)

    (unwind-protect
        (setq res
              (ivy-read "tag: " candidates
                        :update-fn #'yc/counsel-imenu-update-fn
                        :action (lambda (x)
                                  (recenter 3))
                        :caller 'yc/counsel-imenu))

      (unless res
        (goto-char position))
      (yc/imenu--cleanup))))

(defun yc/show-methods-dwim ()
  "Show methods found in current file, using any possible way.."
  (interactive)
  (condition-case error
      (yc/counsel-imenu-or-semantic)
    (error
     (progn
       (PDEBUG "yc/counsel-imenu-or-semantic failed: " error)
       (counsel-xgtags-parse-file (buffer-file-name))))))

(yc/set-keys `(,(cons (kbd "M-m") 'yc/show-methods-dwim)) nil)


(use-package prog-misc
  :commands (yc/doc-at-point yc/insert-single-comment))

(defun yc/show-doc-at-point ()
  "Show document."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-doc-glance)
    (yc/doc-at-point)))

(defun setup-prog-keybindings()
  ;;;; Common program-keybindings
  (interactive)
  ;;;; "keybindings for semantic"
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "M-|") 'align)
  (local-set-key (kbd "M-n") 'senator-next-tag)
  (local-set-key (kbd "M-p") 'senator-previous-tag)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-csD" 'uml/struct-to-dot)
  (local-set-key "\C-csd" 'uml/struct-to-puml)
  (local-set-key "\C-co"    'yc/open-header)
  (local-set-key (kbd "C-x SPC") 'yc/store-current-location)
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

  (yc/add-company-backends-with-yasnippet
    company-keywords company-dabbrev-code)
  (flycheck-mode 1))

(use-package prog-mode
  :defer t
  :hook ((prog-mode . yc/common-program-hook))
  )


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


(use-package counsel-compile2
  :commands (makefile/get-target-list))

(yc/add-compile-unit 'makefile 99
  (aif (or (yc/file-exists-p "makefile")
           (yc/file-exists-p "Makefile"))
      (lambda ()
        (format "make -j%d"
                (yc/get-compiling-threads)))))

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


(provide '06-prog-common)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 06-prog-common.el ends here
