;;; 03-rc-fundamental-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Settings for all modes..

;;; Code:


(use-package helpful
  :pin melpa
  :commands (helpful-callable helpful-variable)
  :bind ((;; (kbd "C-h c")
          "c" . helpful-key)
         (;; (kbd "C-h p")
          "p" . helpful-at-point)
         ))

 ;; ivy mode
(use-package ivy
  :commands (ivy-read ivy-mode)
  :pin melpa
  :ensure t
  :hook ((emacs-startup . ivy-mode))
  :config
  (progn
    (setq ivy-use-virtual-buffers t        ;; Enable bookmarks and recentf
          ivy-count-format "%d/%d "        ;; Display count displayed and total
          enable-recursive-minibuffers t
          ivy-height 20                    ;; Number of result lines to display
          ivy-initial-inputs-alist nil     ;; No regexp by default
          ivy-wrap nil                       ;; wrap candidates

          ))
  :bind ((;(kbd "C-c C-r")
          "" . ivy-resume)
         (;(kbd "M-r")
          [134217842] . ivy-resume)))

;;;; Ivy-rich
;; More friendly display transformer for Ivy
(use-package ivy-rich
  :pin melpa
  :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  :custom
  ;; For better performance
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (plist-put ivy-rich-display-transformers-list 'counsel-bookmark
           '(:columns
             ((ivy-rich-candidate (:width 30))
              (ivy-rich-bookmark-info (:face font-lock-doc-face))))))


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
  :pin melpa
  :ensure t
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (rx (or (: buffer-start (or "#" "."))
                                           (: (or "#" "~")  buffer-end)
                                           (: buffer-start ".ccls-cache" buffer-end)
                                           (: ".elc")
                                           )))
  (counsel-rg-base-command "rg -u --with-filename --no-heading --line-number --color never %s")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

  :bind (("M-x" . counsel-M-x)
         ([remap yank-pop] . counsel-yank-pop)
         ([remap bookmark-bmenu-list] . counsel-bookmark)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-function] . counsel-describe-function)
         (;; ,(kbd "C-c k")
          "k"  . yc/counsel-grep)

         (;; ,(kbd "C-c K")
          "K"  . (lambda ()
                     (interactive)
                     (yc/counsel-grep t)))

         ([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap eshell-previous-matching-input] . counsel-esh-history)
         ([remap occur] . counsel-grep-or-swiper)
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

  (advice-add 'counsel-grep-or-swiper :around #'yc/counsel-grep-or-swiper-adv)
  (advice-add 'counsel-git-grep-action :before-until #'yc/counsel-git-grep-action-adv)

  (ivy-add-actions 'counsel-find-file  yc/ivy-common-actions)
  )


(defun yc/counsel-grep-or-swiper-adv (func &rest args)
  "Advice for 'counsel-grep-or-swiper'.
Call FUNC which is 'counsel-grep-or-swiper with ARGS."
  (apply func (or args (list (aif (symbol-at-point) (symbol-name it))))))


(defun yc/get-woman-path (lst)
  "Get list of woman-path from `LST'."
  (let (result )
    (dolist (path lst)
      (if (file-directory-p path)
          (setq result (append result (directory-files (expand-file-name path) t "[a-z].*")))))
    result))

(use-package counsel-woman
  :bind (([f1] . counsel-woman))
  :custom
  (woman-use-own-frame nil)
  (woman-path (aif (getenv "EPREFIX")
                  (yc/get-woman-path (list (concat it "/usr/share/man")
                                           (concat it
                                                   "/usr/local/share/man")))))
  )


(use-package bookmark
  :defer t
  :custom
  (bookmark-default-file (yc/make-cache-path "bookmarks"))
  :config
  (progn
    (advice-add 'bookmark-set :after #'yc/bookmark-set)))

(defun yc/bookmark-set (func &rest args)
  "Save bookmark after `bookmark-set'.
Call FUNC with ARGS."
  (and (bookmark-time-to-save-p t)
       (bookmark-save)))


;; With smex, ivy can sort commands by frequency.
(use-package amx
  :ensure t
  :defer t
  :commands amx-mode
  :hook (ivy-mode . amx-mode)
  :custom
  (amx-history-length 20))


 ;; Projectile...

(use-package projectile
  :bind (("C-x M-k" . projectile-kill-buffers)
         ("C-x M-j" . projectile-dired)
         ("C-x M-s" . projectile-save-project-buffers))
  :custom
  (projectile-other-file-alist
   '( ;; handle C/C++ extensions
     ("cpp" . ("h" "hpp" "ipp"))
     ("ipp" . ("h" "hpp" "cpp"))
     ("hpp" . ("h" "ipp" "cpp" "cc"))
     ("cxx" . ("h" "hxx" "ixx"))
     ("ixx" . ("h" "hxx" "cxx"))
     ("hxx" . ("h" "ixx" "cxx"))
     ("c" . ("h"))
     ("m" . ("h"))
     ("mm" . ("h"))
     ("h" . ("cc" "cpp" "cxx"  "c" "ipp" "hpp" "ixx" "hxx" "m" "mm"))
     ("cc" . ("h" "hh" "hpp"))
     ("hh" . ("cc"))

     ;; vertex shader and fragment shader extensions in glsl
     ("vert" . ("frag"))
     ("frag" . ("vert"))

     ;; handle files with no extension
     (nil . ("lock" "gpg"))
     ("lock" . (""))
     ("gpg" . (""))))

  :config
  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster than git ls-files or find, and it respects
   ;; .gitignore. This is recommended in the projectile docs.
   ((executable-find "fd")
    (setq-default projectile-generic-command "fd . -0 --type f --color=never"))
   ((executable-find "rg")
    (setq projectile-generic-command "rg -0 --files --follow --color=never --hidden")))
  )

(use-package counsel-projectile
  :pin melpa
  :defines (counsel-projectile-find-file-matcher counsel-projectile-sort-files)
  :functions (counsel-projectile-find-file-action)
  :commands (counsel-projectile-find-file)
  :bind (("C-x M-f" . (lambda ()
                        (interactive)

                        (unless (featurep 'counsel-projectile)
                          (load "counsel-projectile"))

                        (if (or current-prefix-arg
                                (not (file-exists-p (concat (projectile-project-root) ".git"))))
                            (counsel-projectile-find-file)

                          ;; find file, exclude files in submodules.
                          (ivy-read (projectile-prepend-project-name "Find file: ")
                                    (magit-revision-files "HEAD")
                                    :matcher counsel-projectile-find-file-matcher
                                    :require-match t
                                    :sort counsel-projectile-sort-files
                                    :action counsel-projectile-find-file-action
                                    :caller 'counsel-projectile-find-file)
                          ))
          )
         ("C-x M-d" . counsel-projectile-find-dir)))


(use-package smartparens
  :pin melpa
  :ensure t
  :commands (smartparens-global-mode sp-local-pairs sp-with-modes)
  :hook ((after-init . smartparens-global-mode))
  :custom
  (sp-escape-quotes-after-insert nil)
  :custom-face
  (sp-pair-overlay-face ((t nil)))
  :config
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              (;; (kbd "C-M-n")
               [134217742] . sp-forward-sexp)

              (;; (kbd "C-M-p")
               [134217744] . sp-backward-sexp)

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

(defun yc/find-file-noselect-adv (func &rest args)
  "Advice for 'find-file-noselect'.
Call FUNC which is 'find-file-noselect with ARGS."
  (condition-case var
      (apply func args)
    (error (progn
             (PDEBUG "VAR: " var)
             (if (string= (cadr var) "File already visited literally")
                 (find-buffer-visiting (car args))
               (error "%s" (cadr var)))))))

(use-package files
  :config
  ;;Handle file-error and suggest to install missing packages...
  (advice-add 'set-auto-mode :around #'yc/install-package-on-error)
  (advice-add 'abort-if-file-too-large :before-until #'yc/abort-if-file-too-large)
  (advice-add 'find-file-noselect :around #'yc/find-file-noselect-adv))

(defun yc/abort-if-file-too-large (size op-type filename  &optional OFFER-RAW)
  "Advice for `abort-if-file-too-large'.
If file SIZE larger than `large-file-warning-threshold', allow user to use
`vlf' to view part of this file, or call original FUNC which is
`abort-if-file-too-large' with OP-TYPE, FILENAME."
  (when (and (string= op-type "open")
           large-file-warning-threshold size
           (> size large-file-warning-threshold))
    (if (y-or-n-p (format "File %s is large (%s), view with VLF mode? "
                          (file-name-nondirectory filename)
                          (file-size-human-readable size)))
        (progn
          (vlf filename)
          (error "File %s opened in VLF mode." filename)))))


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
  :ensure t
  :commands (s-contains?
             s-ends-with? s-ends-with-p
             s-starts-with? s-blank? s-split))

(use-package session
  :ensure t
  :commands (session-initialize)
  :custom
  (session-globals-include
   '(  (yc/marker-stack 100000 t)
       (hide-ifdef-env 100000 t)
       (kill-ring 10)
       (session-file-alist 100 t)
       (file-name-history 200)))

  :hook ((emacs-startup . session-initialize)))

(use-package super-save
  :ensure t
  :commands (super-save-mode)
  :hook ((emacs-startup . super-save-mode))
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil))

;; Tabs and spaces
(use-package ws-butler
  :commands (ws-butler-mode)
  :ensure t
  :hook ((prog-mode .  ws-butler-mode))
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  (c-basic-offset 4)
  (indent-tabs-mode nil))


(use-package undo-tree
  :defer 3
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
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)

  :hook (after-init . global-undo-tree-mode)
)

;; Preview when `goto-line`
(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))


(use-package layout-restore
  :commands (layout-save-current layout-restore))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
(provide '03-fundamental-mode)
;;; 03-fundamental-mode.el ends here
