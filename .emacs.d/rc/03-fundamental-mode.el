;;; 03-rc-fundamental-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Settings for all modes..

;;; Code:
 ;; Package Management...
(require 'package)

(custom-set-variables
 '(package-archives
   '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
     ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ))
 '(package-archive-priorities
   '(("melpa-stable" . 10)
     ("gnu" . 5)
     ("melpa" . 0))))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(package-initialize)

(setq-default use-package-always-ensure nil ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-always-pin nil
              use-package-enable-imenu-support t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

 ;; ivy mode
(use-package ivy
  :commands (ivy-read ivy-mode)
  :ensure t
  :hook ((emacs-startup . ivy-mode))
  :config
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "
          enable-recursive-minibuffers t
          ivy-height 20))
  :bind ((;(kbd "C-c C-r")
          "" . ivy-resume)
         (;(kbd "M-r")
          [134217842] . ivy-resume)))

(defun yc/counsel-find-file ()
  "Advice for Y with ARGS."
  (interactive)

  (unless (featurep 'counsel)
    (require 'counsel))

  (let* ((symbol (substring-no-properties (symbol-name (symbol-at-point))))
         (filename (aif (ffap-guess-file-name-at-point)
                       (if (string-match-p (rx (+? nonl) "://" (+ nonl)) it)
                           it
                         (expand-file-name it))))
         (collection (if (ffap-url-p filename) nil 'read-file-name-internal))
         (url (ffap-url-p filename))
         line column)


    (when (and filename
               (not url))

      ;; if we are in dired-mode, filename is expanded to fullpath, which is not what I want.
      (when (equal major-mode 'dired-mode)
        (PDEBUG "TRIMMING: " filename)

        (setq filename (file-name-nondirectory
                        (if (s-ends-with-p "/" filename)
                            (substring-no-properties filename 0 -1)
                          filename)))
        )

      (cond
       ;; try match filename directly.
       ((string-match (rx (group (+? nonl)) ":" (group (+ digit)) ":" (group (+ digit)))
                      filename)
        (setq line   (match-string 2 filename)
              column (match-string 3 filename)
              filename (match-string 1 filename)))
       ((string-match (rx (group (+? nonl)) ":" (group (+ digit)))
                      filename)
        (setq line   (match-string 2 filename)
              filename (match-string 1 filename)))

       ;; otherwise, try match from buffer content.
       (t
        (save-excursion
          (goto-char (point-at-bol))
          (PDEBUG "PT: " (point))

          (if (looking-at (format (rx (*? nonl) "%s:" (group (+ digit))
                                      ":" (group (+ digit)))  filename))
              (progn
                (setq line (match-string 1)
                      column (match-string 2))
                )
            (if (looking-at (format (rx (*? nonl) "%s:" (group (+ digit)))
                                                filename))
                (setq line (match-string 1))))))))

    (PDEBUG "FILENAME: " filename
      "URL:       " url
      "COLLECTION:" collection
      "LINE:      " line
      "COLUMN:    " column)

    (ivy-read "Find file: " collection
              :matcher #'counsel--find-file-matcher
              :initial-input filename
              :action
              (lambda (x)
                (PDEBUG "X: " x)
                (if url
                    (funcall ffap-url-fetcher url)
                  (with-ivy-window
                    (if (and counsel-find-file-speedup-remote
                             (file-remote-p ivy--directory))
                        (let ((find-file-hook nil))
                          (find-file (expand-file-name x ivy--directory)))
                      (find-file (expand-file-name x ivy--directory))
                      (when (and line buffer-file-name
                                 (string= buffer-file-name filename))
                        (goto-char (point-min))
                        (forward-line (1- (string-to-number line)))
                        (when column
                          (goto-char (point-at-bol))
                          (forward-char (1- (string-to-number column)))
                          ))))))
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-find-file)))

(defun yc/counsel-grep (&optional prefer-rga)
  "Description."
  (interactive "P")
  (let ((m (point-marker))
        (init (aif (symbol-at-point) (symbol-name it))))

    (cond

     ;; use rga if specified...
     ((and prefer-rga (executable-find "rg") (executable-find "rga"))
      (let ((counsel-rg-base-command
             "rga -S --no-heading --line-number --color never %s ."))
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

(use-package counsel
  :commands (counsel-find-file
             counsel-recentf counsel-semantic-tags
             counsel-fzf counsel-imenu-categorize-functions
             counsel-grep-or-swiper
             counsel-git-grep)
  :ensure t
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (rx (or (: buffer-start (or "#" "."))
                                            (: (or "#" "~")  buffer-end)
                                            (: buffer-start ".ccls-cache" buffer-end)
                                            )))
  :config
  (progn
    (defalias 'git-grep 'counsel-git-grep)
    (setq counsel-fzf-dir-function
          (lambda ()
            default-directory))

    (if (executable-find "rg")
        (setq counsel-grep-base-command
              "rg -S --no-heading --line-number --color never %s %s")))

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
              ("\C-f" . yc/counsel-find-file)
              ("\C-r" . counsel-recentf)
              ("F" . 'counsel-fzf)))


(defun yc/counsel-grep-or-swiper-adv (func &rest args)
  "Advice for 'counsel-grep-or-swiper'.
Call FUNC which is 'counsel-grep-or-swiper with ARGS."
  (apply func (or args (list (aif (symbol-at-point) (symbol-name it))))))

(advice-add 'counsel-grep-or-swiper :around #'yc/counsel-grep-or-swiper-adv)

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
  (bookmark-default-file (yc/make-cache-path "bookmarks")))

(defun yc/bookmark-set (func &rest args)
  "Save bookmark after `bookmark-set'.
Call FUNC with ARGS."
  (and (bookmark-time-to-save-p t)
       (bookmark-save)))

(advice-add 'bookmark-set :after #'yc/bookmark-set)

;; With smex, ivy can sort commands by frequency.
(use-package amx
  :ensure t
  :defer t
  )


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
  )

(use-package counsel-projectile
  :bind (("C-x M-f" . counsel-projectile-find-file)
         ("C-x M-d" . counsel-projectile-find-dir))
         )


(defun sp-lisp-invalid-hyperlink-p (id action context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or (and (looking-at "\\sw\\|\\s_")
               (save-excursion
                 (backward-char 2)
                 (looking-at "\\sw\\|\\s_")))
          (and (save-excursion
                 (backward-char 1)
                 (looking-at "\\sw\\|\\s_"))
               (save-excursion
                 (forward-char 1)
                 (looking-at "\\sw\\|\\s_")))))))

(use-package smartparens
  :pin melpa
  :commands (smartparens-global-mode sp-local-pairs)
  :ensure t
  :hook ((after-init . smartparens-global-mode))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (progn
    (sp-with-modes 'text-mode
      (sp-local-pair "```" "```"))
    (require 'smartparens-config)
    (yc/eval-after-load
      "org"
      (sp-with-modes 'org-mode
        (sp-local-pair "$" "$")
        (sp-local-pair "（" "）")))

    ;; c-mode config in smartparens-config is not valid...
    (yc/eval-after-load
      "cc-mode"
      (require 'smartparens-c))


    ))

;; (use-package elec-pair
;;   :commands (electric-pair-mode)
;;   :hook (after-init . electric-pair-mode)
;;   :custom  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


 ; VLF: view large file.
(use-package vlf
  :defines (vlf)
  :commands (vlf))

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

(advice-add 'abort-if-file-too-large :before-until #'yc/abort-if-file-too-large)


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
        "". ibuffer)))



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

(defalias 's-empty? 's-blank?)


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
  (auto-save-default nil)
  )

;; ws-butler-mode
(use-package ws-butler
  :commands (ws-butler-mode)
  :ensure t
  :init
  (progn
    (custom-set-faces
     '(sp-pair-overlay-face ((t nil)))))
  :hook (
         (prog-mode .  ws-butler-mode)))


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


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
(provide '03-fundamental-mode)
;;; 03-fundamental-mode.el ends here
