;;; 051-prog-c.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:
;;;  Provides configurations for c family.
;;; Code:

 ;; cc-mode and derived.

;;;; Include settings
(defvar yc/system-include-dirs nil
  "A list of directories where the header files are stored.
It is derived from `semantic-gcc-get-include-paths,
and is reversed for better performance.")

(defun yc/add-common-includes ( )
  "Add common includes."
  (setq yc/system-include-dirs
        (reverse (append (semantic-gcc-get-include-paths "c++") '("./"))))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c-mode)
          (semantic-add-system-include dir 'c++-mode))
        yc/system-include-dirs))

(defun yc/get-all-includes ()
  "Return all include directories"
  (-flatten (list
             (if ede-object-project (ede-system-include-path ede-object-project)
               nil)
             yc/system-include-dirs)))

(defun yc/show-project-include-path ()
  (interactive)
  (if ede-object-project
      (print (ede-system-include-path ede-object-project))
    (message "Not controlled by EDE...")))



;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.
(mapc
 (lambda (table)
   (define-abbrev-table table '(
                                ("inc2" "" skeleton-include-2 1)
                                ("inc" "" skeleton-include 1)
                                )))
 '(c-mode-abbrev-table c++-mode-abbrev-table objc-mode-abbrev-table))

(defun yc/incfile-is-local (inc-file)
  "Judge whether inc-file is local."
  (let ((inc-fullpath nil)
        (local-dirs '("."))
        (ret-val nil))
    (if ede-object-project
        (setq local-dirs
              (append local-dirs
                      (ede-system-include-path ede-object-project))))
    (catch 'incfile
      (mapc
       (lambda (dir)
         (if (file-exists-p (format "%s/%s" dir inc-file))
             (progn
               (setq ret-val t)
               (throw 'incfile t))))
       local-dirs))
    ret-val))

(defun yc/update-inc-marks ()
  "Update place markers."
  (let ((statement (buffer-substring-no-properties
                    (point-at-bol) (point-at-eol)))
        (inc-file nil)
        (prompt nil)
        (to-begin nil)
        (to-end nil)
        (yc/re-include
         (rx (group (or "#include" "#import"))
             (+ blank) "|XXX|" (group (*? ascii)) "|XXX|")))
    (when (string-match yc/re-include statement)
      (setq prompt (match-string 1 statement))
      (setq inc-file (match-string 2 statement))
      (if (yc/incfile-is-local inc-file)
          (setq to-begin "\"" to-end "\"")
        (setq to-begin "<" to-end ">"))
      (move-beginning-of-line 1)
      (kill-line)
      (insert (format "%s %s%s%s" prompt to-begin inc-file to-end))
      (move-end-of-line 1)
      )))

(use-package dash
  :commands (-filter))

(define-skeleton skeleton-include
  "generate include<>" ""
  > "#include "
  ;; (company-lsp)
  )

(define-skeleton skeleton-include-2
  "generate include<>" ""
  > "#include "
  (let* ((header-paths (-filter 'file-exists-p
                                (union
                                 (if (and ede-object-project
                                          (ede-system-include-path ede-object-project))
                                     (ede-system-include-path ede-object-project)
                                   semantic-dependency-system-include-path)
                                 yc/system-include-dirs  :test 'equal))
                       )
         cands)

    (dolist (dir header-paths)
      (setq cands (union cands (directory-files dir nil
                                                (rx bol (+? (or alnum "_" "-"))
                                                    (or (: "." (or "h" "hpp")) eol)))
                         :test 'equal)))


    (let* ((req (lsp--make-request "textDocument/completion"
                                   (lsp--text-document-position-params)))
           (response (lsp--send-request req))(incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
           (items (cond ((hash-table-p response) (gethash "items" response))
                        ((sequencep response) response)))
           (candidates (mapcar (lambda (item)
                                 (propertize (gethash "label" item)
                                             'lsp-completion-item item 'lsp-completion-prefix ""))
                               (lsp--sort-completions items)))
           )

      (dolist (cand candidates)
        (add-to-list 'cands (substring cand 9))
        ))


    (defun build-header-list (string cands)
      "Build a header of list containing STRING from CANDS."
      (cond
       ((s-blank? string) cands)
       ((s-contains? "/" string)
        (let* (new-cands)
          (dolist (path header-paths)
            (let* ((dirpath (file-name-directory string))
                   (dirpath-cand (concat path "/" (file-name-directory string))))
              (when (file-exists-p dirpath-cand)
                (setq new-cands
                      (union new-cands
                             (mapcar
                              (lambda (x)
                                (concat dirpath x))
                              (remove-if
                               (lambda (x)
                                 (or (equal "." x)
                                     (equal ".." x)))
                               (directory-files dirpath-cand nil)))
                             :test 'equal)))))
          (remove-if-not (lambda (x) (s-contains? string x)) new-cands)))
       (t (remove-if-not (lambda (x) (s-contains? string x)) cands))))

    (defun counsel-inc-function (string cands)
      "Filter header CANDS fiels for STRING."
      (PDEBUG "STRING" string)
      (if (s-contains? " " string)
          (let* ((array (split-string string " "))
                 (r-match-cand (s-join ".*?" array))
                 (new-cands (build-header-list (car array) cands)))
            (message "Format: %s" r-match-cand)
            (remove-if-not (lambda (x)
                             ;; filter func...
                             (string-match-p r-match-cand x))
                           new-cands))

        (build-header-list string cands)))

    (ivy-read "Include File:"
              (lambda (string)
                (counsel-inc-function string cands))
              :initial-input ""
              :dynamic-collection t
              :unwind (lambda ()
                        (swiper--cleanup))
              :caller 'counsel-skeleton
              ))

  (yc/update-inc-marks))




(use-package member-function
  :commands (expand-member-functions)
  :config (setq mf--insert-commentary nil)
  :hook ((c++-mode . (lambda ()
                       (local-set-key (kbd "C-c m") 'expand-member-functions)))))

 ;; ================================== CPP-H switch ===========================
(use-package prog-utils
  :commands (
             yc/asm-post-process
             yc/format-files yc/switch-h-cpp yc/enable-disable-c-block
             yc/preprocess-file yc/insert-empty-template yc/header-make
             uniq-stack gcrash-analyze-buffer c++filt-buffer
             ))

 ;;;; Hide-ifdefs
(use-package hideif
  :commands (hide-ifdef-mode hif-set-var)
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-read-only nil)
  (hide-ifdef-shadow nil)

  :config
  (advice-add
   'hide-ifdefs :around
   (lambda (&rest args)
     (interactive)
     (setq hif-outside-read-only buffer-read-only)
     (unless hide-ifdef-mode (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
     (if hide-ifdef-hiding
         (show-ifdefs))			; Otherwise, deep confusion.
     (setq hide-ifdef-hiding t)
     (hide-ifdef-guts)
     (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))))

  (define-key hide-ifdef-mode-map "\C-c@t" 'hide-ifdef-toggle-shadowing))

(defun yc/update-hide-env-from-symbol-map ()
  "Update hide-env from symbol map."
  (when (bound-and-true-p semantic-lex-c-preprocessor-symbol-map)
    (mapc (lambda (x)
          (hif-set-var (intern (car x)) (cdr x)))
        semantic-lex-c-preprocessor-symbol-map)))

(defun yc/add-to-ifdef-env (lst)
  "Helper function to update ifdef-env."
  (let (kvp k v)
    (while (setq kvp (pop lst))
      (setq k (car kvp)
            v (cdr kvp))
      (hif-set-var (intern k) t)
      (when (and (symbolp k) (symbolp v))
        (add-to-list 'semantic-lex-c-preprocessor-symbol-map (cons (symbol-name k)
                                                                   (symbol-name v)))))
    (condition-case msg
        (hide-ifdefs)
      (error nil))))


(use-package semantic/bovine/gcc
  :commands (semantic-gcc-setup))

(use-package semantic/bovine/c
  :commands (semantic-default-c-setup)
  :hook ((c-mode-common .
                        (lambda ()
                          (yc/run-with-idle-timer
                           1 nil
                           (lambda ()
                             (unless (bound-and-true-p lsp-mode)
                               (message "Lazy hooks for mode %s" major-mode)
                               (let ((compiler (getenv "CC")))
                                 (if (or (and compiler (string= compiler "clang"))
                                         (eq system-type 'darwin))
                                     (semantic-clang-setup)
                                   (semantic-gcc-setup)))
                               (semantic-default-c-setup)
                               (yc/add-common-includes))))))))


(use-package clang-format
  :commands (clang-format-region clang-format-buffer))

(defun semantic-clang-get-include-paths (lang)
  "Return include paths as gcc uses them for language LANG."
  (let* ((lang "c++")
         (clang-cmd (cond
                     ((string= lang "c") "clang")
                     ((string= lang "c++") "clang++")
                     (t (if (stringp lang)
                            (error "Unknown lang: %s" lang)
                          (error "LANG=%S, should be a string" lang)))))
         (clang-output (semantic-gcc-query clang-cmd "-v" "-E" "-x" lang null-device))
         (lines (split-string clang-output "\n"))
         (include-marks 0)
         (inc-mark "#include ")
         (inc-mark-len (length "#include "))
         inc-path)

    (dolist (line lines)
      (when (> (length line) 1)
        (if (= 0 include-marks)
            (when (and (> (length line) inc-mark-len)
                       (string= inc-mark (substring line 0 inc-mark-len)))
              (setq include-marks (1+ include-marks)))
          (let ((chars (append line nil)))
            (when (= 32 (nth 0 chars))
              (let ((path (substring line 1)))
                (when (and (file-accessible-directory-p path)
                           (file-name-absolute-p path))
                  (add-to-list 'inc-path
                               (expand-file-name path)
                               t))))))))
    inc-path))

(defun semantic-clang-setup ()
  "Setup semantic parsing based on clang."
  (interactive)
  (message "Setting environment for clang...")
  (unless (featurep 'semantic/bovine/gcc)
    (require 'semantic/bovine/gcc))
  (let* ((fields (semantic-gcc-fields (semantic-gcc-query "clang" "-v")))
         (cpp-options `("-E" "-dM" "-x" "c++" ,null-device))
         (query (let ((q (apply 'semantic-gcc-query "cpp" cpp-options)))
                  (if (stringp q)
                      q
                    ;; `cpp' command in `semantic-gcc-setup' doesn't work on
                    ;; Mac, try `gcc'.
                    (apply 'semantic-gcc-query "gcc" cpp-options))))
         (defines (if (stringp query)
                      (semantic-cpp-defs query)
                    (message (concat "Could not query gcc for defines. "
                                     "Maybe g++ is not installed."))
                    nil))
         (ver (cdr (assoc 'version fields)))
         (host (or (cdr (assoc 'target fields))
                   (cdr (assoc '--target fields))
                   (cdr (assoc '--host fields))))
         ;; (prefix (cdr (assoc '--prefix fields)))
         ;; gcc output supplied paths
         ;; FIXME: Where are `c-include-path' and `c++-include-path' used?
         (c-include-path (semantic-gcc-get-include-paths "c"))
         (c++-include-path (semantic-gcc-get-include-paths "c++"))
         (gcc-exe (locate-file "clang" exec-path exec-suffixes 'executable))
         )
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    (when (and (not c-include-path) gcc-exe)
      ;; Fallback to guesses
      (let* ( ;; gcc include dirs
             (gcc-root (expand-file-name ".." (file-name-directory gcc-exe)))
             (gcc-include (expand-file-name "include" gcc-root))
             (gcc-include-c++ (expand-file-name "c++" gcc-include))
             (gcc-include-c++-ver (expand-file-name ver gcc-include-c++))
             (gcc-include-c++-ver-host (expand-file-name host gcc-include-c++-ver)))
        (setq c-include-path
              ;; Replace cl-function remove-if-not.
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include" gcc-include))))
        (setq c++-include-path
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include"
                                      gcc-include
                                      gcc-include-c++
                                      gcc-include-c++-ver
                                      gcc-include-c++-ver-host))))))

    ;;; Fix-me: I think this part might have been a misunderstanding, but I am not sure.
    ;; If this option is specified, try it both with and without prefix, and with and without host
    ;; (if (assoc '--with-gxx-include-dir fields)
    ;;     (let ((gxx-include-dir (cdr (assoc '--with-gxx-include-dir fields))))
    ;;       (nconc try-paths (list gxx-include-dir
    ;;                              (concat prefix gxx-include-dir)
    ;;                              (concat gxx-include-dir "/" host)
    ;;                              (concat prefix gxx-include-dir "/" host)))))

    ;; Now setup include paths etc
    (dolist (D (semantic-clang-get-include-paths "c"))
      (semantic-add-system-include D 'c-mode))
    (dolist (D (semantic-clang-get-include-paths "c++"))
      (semantic-add-system-include D 'c++-mode)
      (let ((cppconfig (list (concat D "/bits/c++config.h") (concat D "/sys/cdefs.h")
                             (concat D "/features.h"))))
        (dolist (cur cppconfig)
          ;; Presumably there will be only one of these files in the try-paths list...
          (when (file-readable-p cur)
            ;; Add it to the symbol file
            (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
                ;; Add to the core macro header list
                (add-to-list 'semantic-lex-c-preprocessor-symbol-file cur)
              ;; Setup the core macro header
              (setq semantic-lex-c-preprocessor-symbol-file (list cur)))
            ))))
    (if (not (boundp 'semantic-lex-c-preprocessor-symbol-map))
        (setq semantic-lex-c-preprocessor-symbol-map nil))
    (dolist (D defines)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map D))
    ;; Needed for parsing macOS libc
    (when (eq system-type 'darwin)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("__i386__" . "")))
    (when (featurep 'semantic/bovine/c)
      (semantic-c-reset-preprocessor-symbol-map))
    nil))

(cdsq yc/c-file-mode-mapping
  (list (cons (rx (or "linux-" "kernel" "driver" "samba")) "kernel")
        (cons (rx (or "curl" "emacs" "gnome")) "gnu")
        (cons (rx (or "mysql") (*? nonl) "/") "mysql")
        (cons (rx (or "postgresql" "postgres" "gpdb") (*? nonl) "/") "postgres")
        (cons (rx "/" (or "llvm" "clang")  "/") "llvm.org")
        )
  "List of possible coding styles.")

(defun yc/get-c-style (&optional filename)
  "Guess c-style based on input filename"
  (interactive)

  (let* ((dirpath (cond
                   (filename (file-name-directory filename))
                   (buffer-file-name (file-name-directory buffer-file-name))
                   (t default-directory)))

         (style (catch 'p-found
                  (dolist (kv yc/c-file-mode-mapping)
                    (when (string-match (car kv) dirpath)
                      (PDEBUG "MATCH: " kv)
                      (throw 'p-found (cdr kv))))
                  "tubo")))

    ;; Print style if called interactively.
    (when (called-interactively-p 'interactive)
      (message "Style is %s" style))
    style))

(use-package cwarn
  :commands (cwarn-mode))

(use-package ccls
  :ensure t
  :custom
  (ccls-executable (or (executable-find "ccls.sh") "ccls"))
  ;; (ccls-sem-highlight-method 'font-lock)
  :config
  (progn
    (advice-add 'ccls--suggest-project-root :before-until
                #'yc/ccls--suggest-project-root-adv)


    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection (lambda () (cons ccls-executable ccls-args)))
      :major-modes '(c-mode c++-mode cuda-mode objc-mode)
      :server-id 'ccls-remote
      :multi-root nil
      :remote? t
      :notification-handlers
      (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
              ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
      :initialization-options (lambda () ccls-initialization-options)
      :library-folders-fn nil)))

  )

(defun yc/c-mode-common-hook ()
  "My hooks to run for c-mode-common-hook."
  (interactive)
  (local-set-key (kbd "C-c s M-d")
                 (lambda ()
                   (interactive)
                   (let ((uml/extract-type 'fields))
                     (uml/struct-to-puml (region-beginning) (region-end)))))

  (c-setup-doc-comment-style)
  (ede-turn-on-hook)
  (cwarn-mode 1)

  (yc/run-with-idle-timer
   0.5 nil
   (lambda ()
     (message "Layzing hooks for mode %s" major-mode)

     (unless (featurep 'ccls)
       (load "ccls"))

     (lsp)

     (yc/update-hide-env-from-symbol-map)
     (condition-case msg
         (hide-ifdef-mode 1)
       (error nil))

     (let ((style (yc/get-c-style (buffer-file-name))) )
       (c-set-style style)
       (when (string= style "kernel-coding")
         (add-to-list
          'hide-ifdef-define-alist
          '(kernel __KERNEL__ CONFIG_SMP CONFIG_PCI CONFIG_MMU))
         (hide-ifdef-use-define-alist 'kernel))))))


(use-package cc-mode
  :commands (c++-mode objc-mode c-mode)
  :mode (((rx (or (: "." (or "H" "cc" "hh" "c" "h" "moc" "ipp") (? ".in") buffer-end)
                  (: "include/" alnum ))) . c++-mode)
         ((rx "." (or "C" "c" "ic") buffer-end) . c-mode)
         ((rx "." (or "mm" "m") buffer-end) . objc-mode))
  :bind (:map c-mode-base-map
              ("\C-c\C-h" . yc/switch-h-cpp)
              ( ;(kbd "M-:")
               [134217786] . yc/enable-disable-c-block)
              (;(kbd "C-c s M-d")
               [3 115 134217828] .
               (lambda ()
                 (interactive)
                 (let ((uml/extract-type 'fields))
                   (uml/struct-to-puml (region-beginning) (region-end)))))
              )

  :hook ((c-mode-common . yc/c-mode-common-hook)
         )

  :config
  (require 'smartparens-c)
  (custom-set-variables
   '(safe-local-variable-values
     (quote
      ((eval c-set-offset
             (quote innamespace)
             0)
       (eval c-set-offset
             (quote substatement-open)
             0)))))

  ;; Customized doc-font
  (cdsq tbdoc-font-lock-doc-comments
    (let ((symbol "[a-zA-Z0-9_]+")
          (header "^ \\* "))
      `((,(concat header "\\("     symbol "\\):[ \t]*$")
         1 ,c-doc-markup-face-name prepend nil)
        (,(concat                  symbol     "()")
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat header "\\(" "@" symbol "\\):")
         1 ,c-doc-markup-face-name prepend nil)
        (,(concat "[#%@]" symbol)
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat "\\\\" symbol)
         0 ,c-doc-markup-face-name prepend nil)
        )))

  (cdsq tbdoc-font-lock-doc-protection
    `(("< \\(public\\|private\\|protected\\) >"
       1 ,c-doc-markup-face-name prepend nil)))

  (cdsq tbdoc-font-lock-keywords
    `((,(lambda (limit)
          (c-font-lock-doc-comments "/\\*\\*.*$" limit
                                    tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!.*" limit
                                    tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!-+" limit
                                    tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!< " limit
                                    tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*< " limit
                                    tbdoc-font-lock-doc-protection)
          (c-font-lock-doc-comments "///.*$" limit
                                    tbdoc-font-lock-doc-comments)))))

 ;;;; This is a sample, real c-doc-comment-style will be set in "10-emacs-custome.el"
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (c-add-style
   "kernel"
   `("linux"
     (tab-width . 8)
     (indent-tabs-mode . t)
     (c-offsets-alist
      (arglist-cont-nonempty
       c-lineup-gcc-asm-reg
       c-lineup-arglist-tabs-only))))

  (c-add-style "llvm.org"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))

  ;; "Based on Google C/C++ Programming Style"
  (c-add-style
   "tubo"
   `("bsd"
     (c-recognize-knr-p . nil)
     (c-basic-offset . 4)
     (tab-width . 4)
     (indent-tabs-mode . nil)
     (comment-column . 40)
     (c-hanging-braces-alist . ((defun-open after)
                                (defun-close before after)
                                (class-open after)
                                (class-close before after)
                                (namespace-open after)
                                (inline-open after)
                                (inline-close before after)
                                (block-open after)
                                (block-close . c-snug-do-while)
                                (extern-lang-open after)
                                (extern-lang-close after)
                                (statement-case-open after)
                                (substatement-open after)))
     (c-hanging-colons-alist . ((case-label)
                                (label after)
                                (access-label after)
                                (member-init-intro before)
                                (inher-intro)))
     (c-hanging-semi&comma-criteria
      . (c-semi&comma-no-newlines-for-oneline-inliners
         c-semi&comma-inside-parenlist
         c-semi&comma-no-newlines-before-nonblanks))
     (c-indent-comments-syntactically-p . nil)
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        brace-catch-brace
                        empty-defun-braces
                        defun-close-semi
                        list-close-comma
                        scope-operator))
     (c-offsets-alist . ((func-decl-cont . ++)
                         (member-init-intro . +)
                         (member-init-cont  . c-lineup-multi-inher)
                         (inher-intro . ++)
                         (comment-intro . 0)
                         (arglist-close . c-lineup-arglist)
                         (topmost-intro . 0)
                         (block-open . 0)
                         (inline-open . 0)
                         (substatement-open . 0)
                         (statement-cont
                          . c-lineup-assignments)
                         (label . /)
                         (case-label . 0)
                         (statement-case-open . 0)
                         (statement-case-intro . +) ; case w/o {
                         (access-label . -)
                         (inextern-lang . 0)
                         (innamespace . 0)))
     (c-doc-comment-style . ((c-mode . tbdoc)
                             (c++-mode . tbdoc)
                             (objc-mode . tbdoc)
                             (java-mode . tbdoc)
                             (awk-mode . autodoc)
                             (other . tbdoc)))))

  ;; Coding style for MySql
  (c-add-style
   "mysql"
   '("tubo"
     (c-basic-offset . 2)
     (indent-tabs-mode . nil)
     (c-comment-only-line-offset . 0)
     (tab-width . 2)
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . 0)
                         (substatement-open . 0)
                         (label . -)
                         (statement-cont . +)
                         (arglist-intro . c-lineup-arglist-intro-after-paren)
                         (arglist-close . c-lineup-arglist)
                         (innamespace . 0)
                         (inline-open . 0)
                         (statement-case-open . +)))))

  (c-add-style
   "postgres"
   '("tubo"
     (c-basic-offset . 4)
     (c-auto-align-backslashes . nil)
     (indent-tabs-mode . t)
     (c-comment-only-line-offset . 0)
     (tab-width . 4)
     (fill-column . 78)
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . 0)
                         (substatement-open . 0)
                         (case-label . +)
                         (label . -)
                         (statement-cont . +)
                         (arglist-intro . c-lineup-arglist-intro-after-paren)
                         (arglist-close . c-lineup-arglist)
                         (innamespace . 0)
                         (inline-open . 0)
                         (statement-case-open . 0)))))



  (custom-set-variables
   '(c-doc-comment-style
     (quote ((c-mode . tbdoc)
             (c++-mode . tbdoc)
             (objc-mode . tbdoc)
             (java-mode . javadoc)
             (pike-mode . autodoc)))))

  ;; special keyword for `c++-mode'.
  (font-lock-add-keywords
   'c++-mode
   `((,(rx bow (group "NEW") (+ space)
           (group (+? (or alnum "_"))) eow)
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     (,(rx bow (group
                (or "NEW" "DELETE"
                    "DEC_ALWAYS_INLINE"
                    "ALWAYS_INLINE"
                    "NO_INLINE" "MAY_ALIAS"
                    )) eow)
      (1 font-lock-keyword-face))
     (,(rx bow (group (+ (or upper "_" digit))) (* blank) "(")
      (1 font-lock-builtin-face))
     ))

  (font-lock-add-keywords
   'c-mode
   `((,(rx bow (group (+ (or upper "_" digit))) (* blank) "(")
      (1 font-lock-builtin-face))))



  ;; C++
  (yc/add-compile-unit 'c++ 66
    (progn
      (PDEBUG "ext:" ext)
      (when (or (equal ext "cc")
                (equal ext "cpp"))
        (lambda ()
          (format "%s %s %s -std=gnu++11 -g -o %s"
                  (yc/get-env "CXX" 'executable-find
                              "clang++" "g++" "mingw-g++")
                  (or (getenv "CPPFLAGS")"-Wall  ")
                  file
                  (file-name-sans-extension file))))))

  ;; C
  (yc/add-compile-unit 'c 65
    (when (or (equal ext "c")
              (equal ext "C"))
      (lambda ()
        (interactive)
        (format "%s -o %s %s %s -g -std=gnu99"
                (yc/get-env "CC" 'executable-find
                            "clang" "gcc" "mingw-gcc")
                (file-name-sans-extension file)
                (or (getenv "CPPFLAGS") "-Wall")
                file)))))


 ;; lsp-support for C family.

(defun yc/ccls--suggest-project-root-adv (&rest args)
  "Advice for 'ccls--suggest-project-root'.
Call FUNC which is 'ccls--suggest-project-root with ARGS."
  (and (memq major-mode '(c-mode c++-mode cuda-mode objc-mode))
       (yc/lsp--suggest-project-root-adv)))

(defun yc/lsp-load-project-configuration-cc-mode (root-file)
  "Advice for 'ccls--suggest-project-root'.
Call FUNC which is 'ccls--suggest-project-root with ARGS."
  (PDEBUG "ENTER: root-" root-file)
  (let* ((blacklist '("/.ccls-cache/"  "build/" "build_Debug/"
                                    "build_RelWithDebInfo/" "build_Release/" "cmake_build_Debug/"
                                    "cmake_build_Release/"  "cmake_build_RelWithDebInfo/")))

    ;;  Use compile database file which is newer...
    (setq ccls-initialization-options nil)

    (when root-file
      (let ((root-dir (file-name-directory root-file)) )
        (setq ccls-args
              (list
               (format "--log-file=%s" (yc/lsp-get-log-file "ccls" root-dir))
               "-v=2"))
        ;; guessing compliation database....
        (PDEBUG "Before advice" ccls-initialization-options)

        (unless (member :compilationDatabaseDirectory ccls-initialization-options)
          (let (compile-dir last-mod-time)

            (dolist (dir '("."  "build/" "build_Debug/"
                             "build_RelWithDebInfo/" "build_Release/" "cmake_build_Debug/"
                             "cmake_build_Release/"
                             "cmake_build_RelWithDebInfo/"))
              (let* ((file (format "%s/%s/compile_commands.json" root-dir dir))
                     (mod-time (if (file-exists-p file)
                                   (file-attribute-modification-time
                                    (file-attributes file)))))

                (when (and mod-time
                         (or (not compile-dir) ;; not set..
                             (time-less-p last-mod-time mod-time) ;; file is newer
                             ))
                  (PDEBUG (format "Using newer database %s, generated at: %s"
                                  file (format-time-string "%D %T" mod-time)))
                  (setq compile-dir dir
                        last-mod-time mod-time))))

            (when compile-dir
              (push compile-dir ccls-initialization-options)
              (push :compilationDatabaseDirectory ccls-initialization-options)
              (add-to-list 'blacklist compile-dir ))))

        (unless (member :index ccls-initialization-options)
          (push (list :blacklist (vconcat blacklist nil)) ccls-initialization-options)
          (push :index ccls-initialization-options))

        (PDEBUG "After advice" ccls-initialization-options)))
    (PDEBUG "leave")))

(defalias 'yc/lsp-load-project-configuration-c-mode
  'yc/lsp-load-project-configuration-cc-mode)

(defalias 'yc/lsp-load-project-configuration-c++-mode
  'yc/lsp-load-project-configuration-cc-mode)

(use-package modern-cpp-font-lock
  :pin melpa
  :commands (modern-c++-font-lock-mode)
  :hook ((c++-mode . modern-c++-font-lock-mode))
)


(provide '051-prog-c)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 051-prog-c.el ends here
