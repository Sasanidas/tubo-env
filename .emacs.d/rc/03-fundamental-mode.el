;;; 03-rc-fundamental-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Settings for all modes..

;;; Code:


(use-package helpful
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

 ;; ivy mode
(use-package ivy
  :ensure t
  :commands (ivy-read)
  :custom
  (ivy-use-virtual-buffers t)        ;; Enable bookmarks and recentf
  (ivy-count-format "%d/%d ")        ;; Display count displayed and total
  (enable-recursive-minibuffers t)
  (ivy-height 17)                    ;; Number of result lines to display
  (ivy-initial-inputs-alist nil)     ;; No regexp by default
  (ivy-wrap nil)                       ;; wrap candidates
  (ivy-on-del-error-function #'ignore) ;; don't quit minibuffer on delete-error

  :bind ((;(kbd "C-c C-r")
          "" . ivy-resume)
         (;(kbd "M-r")
          [134217842] . ivy-resume)

         ([remap switch-to-buffer] . ivy-switch-buffer)
         )
  :config
  (require 'ivy-rich)
  (ivy-mode))

;;;; Ivy-rich
;; More friendly display transformer for Ivy
(use-package ivy-rich
  :preface
  (defun +ivy-rich-describe-variable-transformer (cand)
    "Previews the value of the variable (CAND) in the minibuffer."
    (let* ((sym (intern cand))
           (val (and (boundp sym) (symbol-value sym)))
           (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp val)
              (propertize (format "%s" val) 'face
                          (if (null val)
                              'font-lock-comment-face
                            'success)))
             ((symbolp val)
              (propertize (format "'%s" val)
                          'face 'font-lock-string-face))
             ((keymapp val)
              (propertize "<keymap>" 'face 'font-lock-constant-face))
             ((listp val)
              (prin1-to-string val))
             ((stringp val)
              (propertize (format "%S" val) 'face 'font-lock-string-face))
             ((numberp val)
              (propertize (format "%s" val) 'face 'font-lock-builtin-face))
             ((format "%s" val)))
       t)))
  :ensure t
  :custom
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-path-style 'abbrev)
  :config
  (message "Loading ivy-rich...")
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (plist-put! ivy-rich-display-transformers-list
    'counsel-bookmark
    '(:columns
      ((ivy-rich-candidate (:width 30))
       (ivy-rich-bookmark-info (:face font-lock-doc-face))))
    'counsel-describe-variable
    '(:columns
      ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
       (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
    'counsel-M-x
    '(:columns
      ((counsel-M-x-transformer (:width 60))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
    ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
    'counsel-projectile-switch-to-buffer
    (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
  (ivy-rich-mode +1))


(defun yc/counsel-grep (&optional deep)
  "Description."
  (interactive "P")
  (let ((m (point-marker))
        (init (aif (symbol-at-point) (symbol-name it))))

    (cond
     ;; use rga if specified...
     ((and deep (executable-find "rg"))
      (let ((counsel-rg-base-command (concat counsel-rg-base-command " -uuu ")))
        (counsel-rg init default-directory)))

     ;; ;; or, prefer to rg if possible..
     ((executable-find "rg")
      (counsel-rg init default-directory))

     ;; or, the-silver-searcher if possible
     ((executable-find "ag") (counsel-ag init default-directory))

     ;; at last, grep..
     ((executable-find "grep") (counsel-grep init))
     (t (error "Can't find proper grep function")))

    (yc/push-stack m)))

(defun yc/counsel-git-grep-action-adv (x)
  "Advice for 'counsel-git-grep-action'.
Call FUNC which is 'counsel-git-grep-action with X."
  (PDEBUG "X:" x)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let* ((file-name (match-string-no-properties 1 x))
           (line-number (match-string-no-properties 2 x))
           (buffer (get-file-buffer (expand-file-name
                                     file-name
                                     (ivy-state-directory ivy-last)))))
      (PDEBUG "BUF:" buffer
              "FILE:" file-name
              "LINE:" line-number)
      (when buffer
        (with-current-buffer buffer
          (switch-to-buffer buffer );; (display-buffer buffer)
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))

          (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
            (when swiper-goto-start-of-match
              (goto-char (match-beginning 0))))

          (swiper--ensure-visible)
          (recenter)

          (run-hooks 'counsel-grep-post-action-hook)
          (unless (eq ivy-exit 'done)
            (swiper--cleanup)
            (swiper--add-overlays (ivy--regex ivy-text))))
        t))))


(defun counsel-find-file-as-user (x)
  "Find file X with root privileges."
  (counsel-require-program counsel-root-command)
  (let* ((host (file-remote-p x 'host))
         (user-name (ivy-read "open file as user: " nil))
         (file-name (format "/%s:%s@%s:%s"
                            counsel-root-command
                            user-name
                            (or host "127.0.0.1")
                            (expand-file-name
                             (if host
                                 (file-remote-p x 'localname)
                               x)))))
    (PDEBUG "FILE:" file-name)
    ;; If the current buffer visits the same file we are about to open,
    ;; replace the current buffer with the new one.
    (if (eq (current-buffer) (get-file-buffer x))
        (find-alternate-file file-name)
      (find-file file-name))))

(defun counsel-grep-in-dir (x)
  "Grep in curtent dir X."
  (PDEBUG "X: " x)
  (interactive)
  (yc/counsel-grep))

(defvar yc/ivy-common-actions
  '(("u" counsel-find-file-as-user "Open as other user")
    ("g" counsel-grep-in-dir "Grep in current directory")
    ("l" find-file-literally "Open literally")
    ("v" vlf "Open with VLF")
    ("d" counsel-locate-action-dired "dired")
    )
  "My actions.")


(use-package counsel
  :commands (counsel-find-file
             counsel-recentf counsel-semantic-tags
             counsel-fzf counsel-imenu-categorize-functions
             counsel-grep-or-swiper
             counsel-git-grep)
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (rx (or (: buffer-start (or "#" "."))
                                           (: (or "#" "~")  buffer-end)
                                           (: buffer-start ".ccls-cache" buffer-end)
                                           (: ".elc")
                                           )))
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never %s")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

  :bind (
         ([remap apropos]                  .  counsel-apropos)
         ([remap bookmark-bmenu-list]      .  counsel-bookmark)
         ([remap describe-bindings]        .  counsel-descbinds)
         ([remap describe-face]            .  counsel-faces)
         ([remap describe-function]        .  counsel-describe-function)
         ([remap describe-variable]        .  counsel-describe-variable)
         ([remap find-file]                .  counsel-find-file)
         ([remap imenu]                    .  counsel-imenu)
         ([remap org-goto]                 .  counsel-org-goto)
         ([remap org-set-tags-command]     .  counsel-org-tag)
         ([remap recentf-open-files]       .  counsel-recentf)
         ([remap set-variable]             .  counsel-set-variable)
         ([remap swiper]                   .  counsel-grep-or-swiper)
         ([remap occur]                    . counsel-grep-or-swiper)
         ([remap unicode-chars-list-chars] .  counsel-unicode-char)
         ([remap yank-pop]                 .  counsel-yank-pop)
         ([remap execute-extended-command] .  counsel-M-x)
         ([remap eshell-previous-matching-input] . counsel-esh-history)

         (;; ,(kbd "C-c k")
          "k"  . yc/counsel-grep)

         (;; ,(kbd "C-c K")
          "K"  . (lambda ()
                     (interactive)
                     (yc/counsel-grep t)))

         )
  :bind (:map ctl-x-map
              ("\C-f" . counsel-find-file)
              ("\C-r" . counsel-recentf)
              ("F" . 'counsel-fzf))
  :config
  (defalias 'git-grep 'counsel-git-grep)
  (setq counsel-fzf-dir-function
        (lambda ()
          default-directory))

  (if (executable-find "rg")
      (setq counsel-grep-base-command
            "rg -S --no-heading --line-number --color never %s %s"))

  (defadvice! yc/counsel-grep-or-swiper-adv (orig-func &rest args)
    "Call counsel-grep-or-swiper with symbol-at-point.
ORIG-FUNC is called with ARGS."
    :around #'counsel-grep-or-swiper
    (apply orig-func (or args (list (aif (symbol-at-point) (symbol-name it))))))

  (defadvice! yc/counsel-git-grep-action-adv (x)
    "ORIG-FUNC is called with ARGS."
    :before-until #'counsel-git-grep-action
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (let* ((file-name (match-string-no-properties 1 x))
             (line-number (match-string-no-properties 2 x))
             (buffer (get-file-buffer (expand-file-name
                                       file-name
                                       (ivy-state-directory ivy-last)))))
        (PDEBUG "BUF:" buffer
                "FILE:" file-name
                "LINE:" line-number)
        (when buffer
          (with-current-buffer buffer
            (switch-to-buffer buffer );; (display-buffer buffer)
            (goto-char (point-min))
            (forward-line (1- (string-to-number line-number)))

            (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
              (when swiper-goto-start-of-match
                (goto-char (match-beginning 0))))

            (swiper--ensure-visible)
            (recenter)

            (run-hooks 'counsel-grep-post-action-hook)
            (unless (eq ivy-exit 'done)
              (swiper--cleanup)
              (swiper--add-overlays (ivy--regex ivy-text))))
          t))))

  (ivy-add-actions 'counsel-find-file  yc/ivy-common-actions))


(use-package bookmark
  :custom
  (bookmark-default-file (yc/make-cache-path "bookmarks"))
  :config
  (defadvice! yc/bookmark-set-adv (&rest args)
    "Save book mark file after new bookmark is added.
ORIG-FUNC is called with ARGS."
    :after #'bookmark-set
    (bookmark-save)))


;; With smex, ivy can sort commands by frequency.
(use-package amx
  :ensure t
  :commands amx-mode
  :hook (ivy-mode . amx-mode)
  :custom
  (amx-history-length 20))

 ;; Projectile...
(use-package projectile
  :commands (projectile-project-root projectile-find-other-file)
  :bind (("C-x M-k" . projectile-kill-buffers)
         ("C-x M-j" . projectile-dired)
         ("C-x M-s" . projectile-save-project-buffers))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))

  :config
  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster than git ls-files or find, and it respects
   ;; .gitignore. This is recommended in the projectile docs.
   ((executable-find "fd")
    (setq-default projectile-generic-command "fd . -0 --type f --color=never"))
   ((executable-find "rg")
    (setq projectile-generic-command "rg -0 --files --follow --color=never --hidden"))))


(use-package counsel-projectile
  :ensure t
  :preface
  (defun yc/projectile-find-file (&rest args)
    "My own version of `projectile-find-file'.
Faster than projectile-find-file, since git submodules are ignored.
Call FUNC which is 'projectile-find-file with ARGS."
    (interactive "P")
    (unless (featurep 'counsel-projectile)
      (require 'counsel-projectile))
    (cond
     (current-prefix-arg
      (ivy-read "Find file: " (projectile-files-via-ext-command default-directory (projectile-get-ext-command nil))
                :matcher counsel-projectile-find-file-matcher
                :require-match t
                :sort counsel-projectile-sort-files
                :action counsel-projectile-find-file-action
                :caller 'counsel-projectile-find-file
                ))

     ((file-exists-p (concat (projectile-project-root) ".git"))
      ;; find file, exclude files in submodules.
      (ivy-read (projectile-prepend-project-name "Find file: ")
                (magit-revision-files "HEAD")
                :matcher counsel-projectile-find-file-matcher
                :require-match t
                :sort counsel-projectile-sort-files
                :action counsel-projectile-find-file-action
                :caller 'counsel-projectile-find-file)
      )

     (t
      (counsel-projectile-find-file))))
  :defines (counsel-projectile-find-file-matcher counsel-projectile-sort-files
                                                 projectile-files-via-ext-command)
  :functions (counsel-projectile-find-file-action)
  :commands (counsel-projectile-find-file)
  :bind (("C-x M-f" . yc/projectile-find-file)
         ("C-x M-d" . counsel-projectile-find-dir)))

(use-package smartparens
  :ensure t
  :commands (smartparens-global-mode sp-local-pairs sp-with-modes)
  :hook ((after-init . smartparens-global-mode))
  :custom
  (sp-escape-quotes-after-insert nil)
  (sp-max-prefix-length 25)
  :custom-face
  (sp-pair-overlay-face ((t nil)))
  :config
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              ;; (;; (kbd "C-M-n")
              ;;  [134217742] . sp-forward-sexp)

              ;; (;; (kbd "C-M-p")
              ;;  [134217744] . sp-backward-sexp)

              (;; (kbd "C-M-k")
               [134217739] . sp-kill-sexp)
              (;; (kbd "C-M-w")
               [134217751] . sp-copy-sexp)))

 ; VLF: view large file.
(use-package vlf
  :commands (vlf)
  :custom
  (vlf-batch-size 2000000) ;; 2 MB.
  )

(use-package files

  :config
  ;;Handle file-error and suggest to install missing packages...
  (advice-add 'set-auto-mode :around #'yc/install-package-on-error)

  (defadvice! yc/find-file-noselect-adv (orig-func &rest args)
    "Docs.
ORIG-FUNC is called with ARGS."
    :around #'find-file-noselect
    (condition-case var
        (apply orig-func args)
      (error (progn
               (PDEBUG "VAR: " var)
               (if (string= (cadr var) "File already visited literally")
                   (find-buffer-visiting (car args))
                 (error "%s" (cadr var)))))))

  (defadvice! yc/abort-if-file-too-large-adv (size op-type filename  &optional OFFER-RAW)
    "Advice for `abort-if-file-too-large'.
If file SIZE larger than `large-file-warning-threshold', allow user to use
`vlf' to view part of this file, or call original FUNC which is
`abort-if-file-too-large' with OP-TYPE, FILENAME."
    :before-until #'abort-if-file-too-large
    (when (and (string= op-type "open")
               large-file-warning-threshold size
               (> size large-file-warning-threshold))
      (if (y-or-n-p (format "File %s is large (%s), view with VLF mode? "
                            (file-name-nondirectory filename)
                            (file-size-human-readable size)))
          (progn
            (vlf filename)
            (error "File %s opened in VLF mode" filename))))))


(use-package server
  :commands (server-start server-running-p)
  :hook ((emacs-startup .
                        (lambda ()
                          (unless (server-running-p)
                            (server-start))))))

 ;;; ABBREV-MODE;;;
(use-package abbrev

  :custom
  (abbrev-file-name  "~/.emacs.d/abbrev_defs")
  (save-abbrevs 'silently)
  :hook ((after-init . abbrev-mode))
  :config
  (progn
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))))

 ;; unset Ctrl+\: toggle-input methods

(yc/unset-keys
 (list (kbd "C-\\")))

(use-package ibuffer
  :bind ((;; ,(kbd "C-x C-b")
          "". ibuffer))
  :bind (:map ibuffer-mode-map
              (;; (kbd "C-x C-f")
               "" . counsel-find-file)))


(autoload 'switch-window "switch-window" ""  t)
(defun yc/switch-window (&optional reverse)
  "Switch window.
With REVERSE is t, switch to previous window."
  (interactive)
  (if (> (length (window-list)) 3)
      (switch-window)
    (other-window (if reverse 2 1))))

(defun auto-rename-buffer ()
  "Rename current buffer."
  (interactive)
  (let ((newname (concat (buffer-name) "-" (format-time-string  "%H:%M:%S" (current-time)))))
    (rename-buffer newname)))

(yc/set-keys
 (list
  (cons (kbd "<C-f2>") 'rename-buffer)
  (cons (kbd "<f2>") 'auto-rename-buffer)
  (cons (kbd "C-x o") 'yc/switch-window)
  (cons (kbd "C-x O") (lambda ()(interactive) (yc/switch-window t)))))

;; string functions..
(use-package s
  :commands (s-contains?
             s-ends-with? s-ends-with-p
             s-starts-with? s-blank? s-split))

(use-package super-save
  :ensure t
  :commands (super-save-mode)
  :hook ((emacs-startup . super-save-mode))
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil))

;; Tabs and spaces
(use-package ws-butler
  :ensure t
  :commands (ws-butler-mode)
  :hook ((prog-mode .  ws-butler-mode))
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  (c-basic-offset 4)
  (indent-tabs-mode nil))


(use-package undo-tree
  :defer t
  :ensure t
  :commands (global-undo-tree-mode undo-tree-undo undo-tree-visualize undo-tree-redo)
  :bind (:map undo-tree-map
              (;; (kbd "C-x U")
               "U" . 'undo-tree-visualize)
              (;; ,(kbd "\C-x u")
               "u". 'undo-tree-undo)
              (;; ,(kbd "\C-x M-u")
               [24 134217845]. 'undo-tree-redo))
  :bind ((;(kbd "C-x U")
          "U" . 'undo-tree-visualize)
         (;; ,(kbd "\C-x u")
          "u". 'undo-tree-undo)
         (;; ,(kbd "\C-x M-u")
          [24 134217845]. 'undo-tree-redo))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)

  :hook (after-init . global-undo-tree-mode)
)

;; Preview when `goto-line`
(use-package goto-line-preview
  :ensure t
  :bind ([remap goto-line] . goto-line-preview))


(use-package layout-restore
  :commands (layout-save-current layout-restore))


(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 9999)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :hook ((after-init . which-key-mode)))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
(provide '03-fundamental-mode)
;;; 03-fundamental-mode.el ends here
