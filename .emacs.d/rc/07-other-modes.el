;;; 07-other-modes.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(use-package highlight-parentheses
  :commands (highlight-parentheses-mode)
  :hook ((prog-mode . highlight-parentheses-mode))
  :config
  (setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "firebrick1"
                          "IndianRed4")))

(use-package highlight-indentation
  :commands (highlight-indentation-mode))

 ;; Info settings.
(use-package info
  :commands (info)
  :config
  (let* ((r-match-info (rx (group (+ ascii)) ".info" (* ascii)))
         res)
    ;; initialize Info-directory-list if needed.
    (unless Info-directory-list
      (info-initialize))

    ;; add Info-default-directory-list at tail.
    (mapcar (lambda (x)
              (add-to-list 'Info-directory-list x t))
            Info-default-directory-list)))

(use-package counsel-info

  :commands (counsel/info)
  :bind (([remap info] . counsel/info))
  )


 ;;; spell checker..
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :hook ((prog-mode . flyspell-prog-mode)
         (markdown-mode . flyspell-mode)
         (emacs-startup . (lambda ()
                            (unless (executable-find "aspell")
                              (warn "aspell not found, flyspell will not work.")))))
  :config
  (progn
    (substitute-key-definition
     'flyspell-goto-next-error  'backward-page flyspell-mode-map)
    (substitute-key-definition
     'flyspell-auto-correct-word 'forward-page flyspell-mode-map)))

(use-package ispell
  :bind (([M-S-f11] . ispell-buffer)
         ([S-f11]   . ispell-word))
  :config
  (custom-set-variables
   '(ispell-program-name (executable-find "aspell"))
   '(ispell-extra-args '("--reverse"))
   '(ispell-skip-html t)
   '(ispell-dictionary "english")
   '(ispell-local-dictionary "english"))
  )

(use-package log-edit
  :hook ((log-edit-mode . (lambda ()
                            (flyspell-mode 1)))))


 ;;; Dictionary.
(use-package tdict
  :bind ((;; (kbd "<C-f10>")
          [C-f10]
          . tdict-search)))

(use-package tabbr
  :bind ((;; ,(kbd "<S-f10>")
          [S-f10]. tabbr-search)
         (;; ,(kbd "<C-S-f10>")
          [C-S-f10]. tabbr-edit)))

 ;; image-mode
(use-package image-mode
  :bind (:map image-mode-map
              (;; ,(kbd "C-c o")
               "o". yc/open-with-external-app)))

 ;; Diff & Merge
(defun yc/ediff-copy-file-name-A ()
  "Description."
  (interactive)
  (yc/ediff-copy-file-name 'A))

(defun yc/ediff-copy-file-name (id &optional absolute)
  "Description."
  (interactive)
  (let* ((buf
          (cond
           ((eq id 'A) ediff-buffer-A)
           ((eq id 'B) ediff-buffer-B)
           (t (error "OOPS"))))
         (name (buffer-file-name buf)))

    (kill-new     (if absolute
                     name
                    (file-name-base name)))))

(defun ediff-copy-both-to-C ()
  "Copy both regions into C."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defvar yc/flycheck-bufferes-to-restore nil
  "List of buffers whose flycheck-mode is disabled temporarily.")

(defun yc/ediff-prepare (&optional buffer)
  "Prepare BUFFER for Ediff."
  (with-current-buffer (or buffer (current-buffer))
    (PDEBUG "YC/EDIFF-PREPARE: " buffer)

    (ws-butler-mode -1)
    (if flycheck-mode
        (push (current-buffer) yc/flycheck-bufferes-to-restore))

    (if (fboundp 'show-ifdefs)
        (show-ifdefs))
    (PDEBUG "YC/EDIFF-PREPARE END.")))

(defun yc/ediff-cleanup ()
  "Clean up for ediff modes."
  (dolist (buffer yc/flycheck-bufferes-to-restore)
    (with-current-buffer buffer
      (flycheck-mode 1)))

  (setq yc/flycheck-bufferes-to-restore nil))

(defun yc/ediff-startup ()
  "Description."
  (dolist (buf '(ediff-buffer-A ediff-buffer-B  ediff-buffer-c))
    (if buf
        (yc/ediff-prepare buf))))



(use-package ediff
  :commands (ediff-files)
  :bind ((;; ,(kbd "<f12>")
          [f12]. ediff-buffers))
  :custom
  (ediff-diff-options "-w") ;; turn off whitespace checking
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-ignore-similar-regions t)

  :config
  (defadvice! yc/ediff-buffers-adv (buffer-A buffer-B &rest args)
    "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B, with optional ARGS.
ORIG-FUNC is called with ARGS."
    :before #'ediff-buffers
    (yc/ediff-prepare buffer-A)
    (yc/ediff-prepare buffer-B))


  (setq ediff-diff-ok-lines-regexp
        (concat
         "^\\("
         "[0-9,]+[acd][0-9,]+\C-m?$"
         "\\|[<>] "
         "\\|---"
         "\\|.*Warning *:"
         "\\|.*No +newline"
         "\\|.*missing +newline"
         "\\|.*文件尾没有 newline 字符"
         "\\|^\C-m?$"
         "\\)"))

  :hook
  ((ediff-keymap-setup
    .
    (lambda ()
      (define-key 'ediff-mode-map "d" 'ediff-copy-both-to-C)
      (define-key 'ediff-mode-map "ff" 'yc/ediff-copy-file-name-A)))

   (ediff--startup . yc/ediff-startup)
   (ediff-quit . yc/ediff-cleanup)))

(use-package smerge-mode
  :bind ((;; ,(kbd "<S-f12>")
          [S-f12]. smerge-ediff))
  :config
    (advice-add 'smerge-ediff :before #'yc/ediff-prepare))



(use-package yc-utils
  :commands (
             edit-elpa   edit-project   edit-rcs
             edit-template
             reload-file reload-all-files edit-emacs debug-on
             uniq-region
             xmind/convert-to-org
             yc/add-subdirs-to-load-path
             yc/copy-current-buffername yc/reload-emacs
             yc/decode-hex-color
             yc/encode-hex-color
             yc/eval-and-insert
             yc/eval-and-insert-comment
             yc/http-save-page
             yc/insert-current-date
             yc/insert-current-date-time
             yc/kill-proc yc/move-snapshot  yc/expand-macro
             yc/pack-env yc/unpack-env dos-unix unix-dos
             yc/syntax-color-hex

             yc/choose-compiler
             yc/show-compilers

             yc/update-env-from

             yc/view-with-ediff
             yc/get-cpu-number
             yc/get-compiling-threads
             yc/get-env
             yc/uef
             yc/touch-file
             yc/git-copy-file-path
             yc/new-snippet
             yc/dired-compress-file
             )

  :bind ((;(kbd "C-x J")
          "J" . yc/eval-and-insert-comment)
         (;(kbd "C-x j")
          "j" . yc/eval-and-insert)

         (;; (kbd "C-o")
          "" . zl-newline)
         (;; (kbd "C-S-o")
          [33554447] . zl-newline-up)
         (;; (kbd "")
          [143] . zl-newline-up)

         ([mouse-4] . down-slightly)
         ([mouse-5] . up-slightly)

         (;; (kbd "<C-return>")
          [C-return] . yc/insert-line-number)
         (;(kbd "<M-K>")
          [M-K] . kill-current-buffer)

         (;; (kbd "<M-f11>")
          [M-f11] . yc/fill-region)

         ("\C-c>" . shift-region-right)
         ( "\C-c<" . shift-region-left)
         ( "\M-;" . yc/comment-dwim-line)

         ( ;(kbd "C-+")
          [67108907] . increase-font-size)
         ( ;(kbd "C--")
          [67108909] . decrease-font-size)

         (;(kbd "C-h V")
          "V" . yc/debug-variable)

         (;(kbd "M-W")
          [134217815] . yc/kill-file-ln)

         (;; (kbd "C-x ^")
          "^" . yc/enlarge-window)
         (;; (kbd "C-x v")
          "v" . yc/shrink-window)

         (;;(kbd "C-x >")
          ">" . yc/enlarge-window-horizontal)
         (;; (kbd "C-x <")
          "<" . yc/shrink-window-horizontal)

         ([f5] . yc/open-eshell)
         ([remap shell-command] . yc/exec-command-via-eshell))

  :hook ((before-save  .change-file-permissions-to-writable))
  )

(use-package yc-dump
  :commands (yc/dump-emacs yc/config-emacs))


 ;; diff-mode

(use-package diff-mode
  :mode (rx (or ".rej"  "patch") eol)
  :bind (:map diff-mode-map
              (;(kbd "C-c C-v")
               "" . yc/view-with-ediff)
              (;; (kbd "C-c M-v")

               [3 134217846] . (lambda ()
                                 (interactive)
                                 (yc/view-with-ediff nil t)))
              (;; (kbd "C-c C-f")
               "" . (lambda ()
                          (interactive)
                          (kill-new (file-name-nondirectory buffer-file-name)))))
  :hook ((diff-mode . (lambda ()
                        (which-function-mode -1)))))

;; ************************** highlight utils ****************************
(use-package highlight-symbol
  :bind (([(control f3)]  . highlight-symbol-at-point)
         ( ;(kbd "M-[ 1 3 ^")
          [134217819 49 51 94] . highlight-symbol-at-point)

         ([f3]  . highlight-symbol-next)
         ([(shift f3)]  . highlight-symbol-remove-all)
         ([(meta f3)]  . highlight-symbol-prev)
         ([(control meta f3)]  . highlight-symbol-query-replace)))


 ;; **************************** RFCs ******************************
(use-package irfc
  :commands (irfc-visit irfc-follow)
  :config
  (custom-set-variables
   '(irfc-download-base-url "http://www.rfc-editor.org/rfc/")
   '(irfc-directory  "~/Documents/TechBooks/RFCs/")
   )
  :mode ("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . irfc-mode))

 ;; ********************* tramp *******************************
;; (cdsq tramp-ssh-controlmaster-options
;;   "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(use-package tramp
  :custom
  (tramp-default-method
     (cond
      ;; PuTTY is installed.  We don't take it, if it is installed on a
      ;; non-windows system, or pscp from the pssh (parallel ssh) package
      ;; is found.
      ((and (eq system-type 'windows-nt) (executable-find "plink")) "plink")
      ((and (eq system-type 'windows-nt) (executable-find "pscp")) "pscp")
      ;; There is an ssh installation.
      ((executable-find "scp") "scp")
      ;; Fallback.
      (t "ftp")))
  (tramp-completion-without-shell-p t)
  (tramp-auto-save-directory "~/.emacs.d/auto-save-list")
  (tramp-remote-path '("~/.local/bin/"
                       "/opt/devtools/bin"
                       "/usr/local/bin"
                       "/usr/local/sbin"
                       "/opt/bin" "/opt/sbin" "/opt/local/bin"
                       "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                       ))


  :config
  (progn
    (tramp-set-completion-function "ssh"
                                   '((tramp-parse-sconfig "/etc/ssh_config")
                                     (tramp-parse-sconfig "~/.ssh/config")))))

 ;; ****************** eww ***************************
(use-package shr
  :custom
  (shr-use-fonts nil)
  (shr-use-colors t)
  (shr-blocked-images
      (rx
       (or (: "https://www.postgresql.org"
              (or
               "/media/img/about/press/elephant.png"
               "/media/img/atpostgresql.png"
               "/media/img/git.png"))))))

(use-package browse-url
  :commands (browse-url-generic)
  :custom
  (browse-url-generic-program (cond ((string= system-type "darwin")
                                     "/usr/bin/open")
                                    ((string= system-type "gnu/linux")
                                     (or
                                      (executable-find "google-chrome-stable")
                                      (executable-find "google-chrome")
                                      (executable-find "google-chrome-beta")
                                      (executable-find "firefox")
                                      (executable-find "firefox-bin")
                                      "/usr/bin/xdg-open"))
                                    (t nil))))

(use-package eww
  :defer t
  :hook ((eww-mode . yc/disable-trailling-spaces))
  :bind (:map eww-mode-map
              ("\C-co" . eww-browse-with-external-browser)

              (;; ,(kbd "<M-left>")
               [M-left]. eww-back-url)
              (;; ,(kbd "<M-right>")
               [M-right]. eww-forward-url)
              ;; URL copy: bind to "w"
              ))

(use-package my-net-utils
  :commands (yc/download-url yc/open-url)
  :bind ((;; ,(kbd "C-x C-d")
          "". yc/download-url)
         (;;(kbd "C-x C-o")
          "" . yc/open-url)))

 ;; nov, epub reader
(use-package nov
  :mode ((rx ".epub") . nov-mode))

 ;; PDF
(use-package pdf-tools
  :commands (pdf-tools-install pdf-tools-enable-minor-modes)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook ((pdf-view-mode . pdf-tools-enable-minor-modes))
  :custom
  (pdf-info-epdfinfo-program (expand-file-name "~/.local/bin/epdfinfo"))
  (pdf-view-display-size 'fit-width)
  (pdf-view-resize-factor 1.10)
  :bind (:map pdf-view-mode-map
              ("l" . pdf-history-backward)
              ("r" . pdf-history-forward)
              ("i" . tnote))
  :config
  (unless (file-executable-p pdf-info-epdfinfo-program)
    (message "Tool %s does not exist, compiling ...")
    (pdf-tools-install)))

(defun yc/pdf-tools-re-install ()
  "Re-install `epdfinfo' even if it is installed.
The re-installation is forced by deleting the existing `epdfinfo'
binary.
Useful to run after `pdf-tools' updates."
  (interactive)
  (unless (featurep 'pdf-tools)
    (require 'pdf-tools))
  (when (pdf-info-running-p)
    (pdf-info-kill))
  (delete-file pdf-info-epdfinfo-program)
  (pdf-tools-install :no-query-p))


(use-package stringtemplate-mode

  :mode "\\.st\\'")


(use-package eshell
  :commands (eshell-command)
  :bind ((;; ,(kbd "<C-f5>")
          [C-f5]. eshell))
  :hook ((eshell-mode . (lambda ()
                          (setq eshell-path-env (getenv "PATH")))))
  :init
  (progn
    (custom-set-variables
     '(eshell-buffer-shorthand t)
     '(eshell-directory-name (yc/make-cache-path "eshell"))
     '(eshell-aliases-file (expand-file-name "~/.emacs.d/eshell_alias"))
     '(eshell-buffer-maximum-lines 20000)
     '(eshell-history-size 350)
     '(eshell-hist-ignoredups t)
     '(eshell-buffer-shorthand t)
     '(eshell-plain-echo-behavior t)))
  :config
  (require 'esh-opt)

  ;; quick commands
  (defalias 'eshell/e 'find-file-other-window)
  (defalias 'eshell/d 'dired)
  (setenv "PAGER" "cat")

  ;; automatically truncate buffer after output
  (when (boundp 'eshell-output-filter-functions)
    (push 'eshell-truncate-buffer eshell-output-filter-functions))

  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(use-package eshell+
  :commands (eshell/ldd eshell/restart_pg))

(use-package em-term
  :config
  (mapc (lambda (x) (push x eshell-visual-commands))
        '("el" "elinks" "htop" "less" "ssh" "tmux" "top" "vim" "tail"
          "spark-shell" "sbt" "watch")))

(use-package vterm
  :bind ((;; ,(kbd "<S-f5>")
          [S-f5]. vterm))
  :custom
  (vterm-kill-buffer-on-exit t)
  )

 ;; comint hook
(use-package comint

  :defer t
  :init
  (progn
    (add-hook 'comint-mode-hook
      (lambda ()
        (make-local-variable 'jit-lock-defer-timer)
        (set (make-local-variable 'jit-lock-defer-time) 0.25)))

    (custom-set-variables
     '(comint-buffer-maximum-size 32768))
    (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
    (add-hook 'comint-preoutput-filter-functions
      (lambda (text)
        (interactive)
        (let* ((regexp "\\(.\\{256\\}[;,: ]\\)")
               (shortened-text (replace-regexp-in-string regexp "\\1\n" text)))
          (if (string= shortened-text text)
              text
            shortened-text))))))


(use-package tabify
  :bind (("\C-xt" . untabify)
         ("\C-xT" . tabify)))

(use-package find-func
  :bind (("\C-cff"  . find-function)
         ( "\C-cfc" . find-function-on-key))
  :config
  (advice-add 'find-function :before (lambda (&rest args)
                                         (condition-case error
                                             (yc/push-stack)
                                           ('error nil)))))


(use-package hexview-mode :bind ((;; ,(kbd "C-x M-F")
                                  [24 134217798]. hexview-find-file)))

 ;; *********************** graphviz dot mode ***********

(defun yc/graphviz-dot-preview ()
  "Preview current buffer."
  (interactive)
  (unless (executable-find "dot")
    (error "Graphviz is required but not found"))

  (let* ((tempname (make-temp-file "dot-"))
         (output (concat tempname ".png"))
         (ob-name "*dot-preview*")
         (ob (get-buffer ob-name)))
    (write-region (point-min) (point-max) tempname)

    (when ob
      (kill-buffer ob))

    (setq ob (get-buffer-create ob-name))
    (set-buffer ob)
    (erase-buffer)
    (condition-case var
        (if (= (call-process "dot" nil ob nil "-Tpng" tempname "-o" output) 0)
            (progn
              (erase-buffer)
              (insert-file-contents output)
              (image-mode)
              (set-buffer-multibyte t)
              (display-buffer ob))

          (progn
            (message "Failed to compile: %s" (buffer-string))))

      (error (message "error: %s" var)))

    (dolist (fn (list tempname output))
      (when (file-exists-p fn)
        (delete-file fn)))))


(defun yc/graphviz-dot-view-external ()
  "View with external tool."
  (interactive)
  (let ((fn (concat (file-name-sans-extension buffer-file-name)
                    "." graphviz-dot-preview-extension)))
    (unless (file-exists-p fn)
      (error "File not compiled??"))
    (yc/open-with-external-app fn)))

(use-package graphviz-dot-mode
  :defer t
  :commands (graphviz-dot-mode graphviz-compile-command)
  :bind (:map graphviz-dot-mode-map
              (;; ,(kbd "C-c C-c")
               "". yc/graphviz-dot-preview)
              (;; ,(kbd "C-c C-o")
               "". yc/graphviz-dot-view-external))
  :custom
  (graphviz-dot-indent-width 4)
  :config
  (progn
    (yc/add-compile-unit 'dot 40
      (when (equal ext "dot")
        (lambda ()
          (graphviz-compile-command (buffer-file-name)))))))

(defalias 'dot-mode 'graphviz-dot-mode)
(defalias 'org-babel-execute:graphviz-dot 'org-babel-execute:dot)


 ;; Dired
(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package dired
  :commands (dired)
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (dired-dwim-target t)
  (dired-dnd-protocol-alist nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (ls-lisp-verbosity '(uid gid))
  :hook ((dired-mode . (lambda () (setq fill-column 9999))))
  :bind (:map dired-mode-map
              (;; (kbd "M-p")
               [134217840]
               . dired-up-directory)
              (;; (kbd "<C-return>")
               [C-return]
               . dired-find-file-other-window)
              (;; (kbd "<M-return>")
               [M-return]
               . yc/open-with-external-app)
              ([f12] . dired-ediff))

  :config
  (progn
    (setq-default dired-listing-switches "-alh")
    (defadvice! yc/dired-find-file-adv (&rest args)
      "If file already opened, switch to that buffer without confirm.
ORIG-FUNC is called with ARGS."
      :before #'dired-find-file
      (awhen (find-buffer-visiting (dired-get-file-for-visit))
          (switch-to-buffer it)))

    (load-library "ls-lisp")
    (define-key ctl-x-map "d" nil)))

(use-package dired-x
  :commands (dired-jump)
  :init
  (progn
    (define-key ctl-x-map "\C-j" 'dired-jump))
  :config
  (progn
    (load-library "ls-lisp")
    (add-to-list 'auto-mode-alist (cons "[^/]\\.dired$"
                                        'dired-virtual-mode))))

(use-package dired-aux
  :config
  (defadvice! yc/dired-compress-adv (&rest args)
    "Compress file with 7z if possible.
ORIG-FUNC is called with ARGS."
    :override #'dired-compress
      (let* (buffer-read-only
         (from-file (dired-get-filename))
         (new-file (yc/dired-compress-file from-file)))
    (if new-file
        (let ((start (point)))
          ;; Remove any preexisting entry for the name NEW-FILE.
          (ignore-errors (dired-remove-entry new-file))
          (goto-char start)
          ;; Now replace the current line with an entry for NEW-FILE.
          (dired-update-file-line new-file) nil)
      (dired-log (concat "Failed to compress" from-file))
      from-file))))


(use-package hl-line
  :hook ((bookmark-bmenu-mode . hl-line-mode)
         (ibuffer-mode . hl-line-mode)
         (grep-setup-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

 ;; vimrc-mode
(use-package vimrc-mode
  :mode (rx (or ".vim" (: "." (? "_") (? "g")  "vimrc") ".exrc")))


(use-package ztree
  :commands (ztree-dir ztree-diff))

(use-package charset-util :commands (yc/list-non-ascii))


(use-package x86-help   :bind (("x" . x86-help))) ;;(kbd "C-h x")


(use-package artist

  :bind ((;; ,(kbd "<C-S-f2>")
          [C-S-f2]. artist-mode)))

(use-package image-file

  :defer
  :config
  (progn
    (auto-image-file-mode t))) ; 自动加载图像


(use-package t-report  :commands (yc/new-wp yc/new-mail))

 ;; edit-indirect mode.
(use-package edit-indirect
  :bind ((;; ,(kbd "C-c '")
          "'". edit-indirect-region)))

 ;; fold
(defun yc/vimish-fold-toggle (beg end)
  "Description."
  (interactive "r")
  (unless (featurep 'vimish-fold)
    (load "vimish-fold"))
  (if mark-active
      (progn
        (dolist (overlay (overlays-in beg end))
          (when (vimish-fold--vimish-overlay-p overlay)
            (vimish-fold--delete overlay)))
        (vimish-fold beg end))
    (vimish-fold-toggle)))

(use-package vimish-fold
  :commands (vimish-fold vimish-fold-toggle vimish-fold-delete)
  :bind ((;; ,(kbd "C-c hr")
          "hr". yc/vimish-fold-toggle)))

(use-package mediawiki
  :commands (mediawiki-open mediawiki-site))

(use-package desktop
  :commands (desktop-save-in-desktop-dir desktop-read)
  :config
  (progn
    (setq desktop-path (list (yc/make-cache-path "desktop" t))
          desktop-dirname (yc/make-cache-path "desktop" t))))

;; ****************************** HTTP Code *****************************
;; Explain the meaning of an HTTP status code. Copy httpcode.el to your
;; load-path and add to your .emacs:

(autoload 'hc "httpcode" "http status code" t)

 ;; ****************************** HTML Mode ******************************
(use-package sgml-mode
  :mode ("/itsalltext/" . html-mode)
  :commands (html-autoview-mode)
  :hook ((html-mode . yc/html-mode-hook)))

(defun yc/html-mode-hook ()
  "My hook for html mode."
  (html-autoview-mode -1)
  (remove-hook 'after-save-hook 'browse-url-of-buffer t)
  (flyspell-mode 1))


 ;; css & scss & sass
(use-package sass
             :defer t)
(use-package css-mode
  :defer t
  :mode (rx "." (or "scss" "css" "rasi") eow)
  :config
  (yc/add-company-backends 'css-mode 'company-css)
  :custom
  (css-indent-offset 2)
)

 ;; *************************** nxml mode for XML *******************
(use-package nxml-mode
  :mode (rx "." (or "xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "rdf" "plist") eol)
  :custom
  (nxml-attribute-indent 2)
  (nxml-child-indent 2)
  (nxml-outline-child-indent 2)
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-bind-meta-tab-to-complete-flag t)
  (nxml-slash-auto-complete-flag t)
  :hook ((nxml-mode .
                    (lambda ()
                      (local-set-key (kbd "C-c /") 'nxml-finish-element)
                      (auto-fill-mode)
                      (rng-validate-mode)
                      (hs-minor-mode 1)
                      )))
  :config
  (yc/add-company-backends nxml-mode 'company-nxml))

 ;; **************************** Text Mode ***************************

(defvar yc/ditaa-package nil "Path of ditaa package")

(defun yc/load-ditaa ()
  "Load ditaa command and package path."
  (unless (featurep 's)
    (require 's))

  (unless yc/ditaa-package
    (setq yc/ditaa-package
          (cond
           ((executable-find "ditaa")
            (progn
              (let ((content (shell-command-to-string (format "cat %s" (executable-find "ditaa")))))
                (PDEBUG "CONTENT: " content)

                (catch 'p-found
                  (dolist (item (s-split " " content))
                    (PDEBUG "ITEM: " item)
                    (PDEBUG "MATCH: " (string-match (rx "ditaa" (+? nonl) ".jar") item))

                    (when (string-match (rx "ditaa" (+? nonl) ".jar") item)
                      (throw 'p-found item)))
                  nil))))
           (t
            (warn "yc/ditaa-package not setup")
            nil))))
  yc/ditaa-package
)


(defun yc/txt-to-png ()
  "Change a txt file into png file using ditaa."
  (interactive)
  (unless (executable-find "java")
    (error "Function `txt-to-png' requires java"))

  (unless yc/ditaa-package
    (yc/load-ditaa))

  (let* ((infile (buffer-file-name))
         (txt2png-buf-name "*txt2png*"))
    (get-buffer-create txt2png-buf-name)
    (pop-to-buffer txt2png-buf-name)
    (erase-buffer)
    (insert "\nInvoking command: java -jar"
            yc/ditaa-package
            infile "--overwrite")
    (set-process-sentinel
     (start-process "txt-to-png" txt2png-buf-name "java" "-jar"
                    yc/ditaa-package
                    infile "--overwrite")
     (lambda (process state)
       (when (and (string-match "finished" state)
                  (yes-or-no-p "Open generated file?"))

         (save-excursion
           (goto-char (point-min))
           (unless (search-forward-regexp
                    (rx "Rendering to file:" (* space) (group (+ nonl))) nil t)
             (error "Can't find output file")))

         (let ((outfile (match-string 1)))
           (unless (file-exists-p outfile)
             (error "File %s does not exist" outfile))
           (kill-current-buffer)
           (find-file outfile)))))
    (message "This may take for a while, refer to *txt2png* to check progress...")))

(use-package text-mode
  :bind (:map text-mode-map
              ("\C-c\C-e" . yc/txt-to-png))
  :config
  (sp-with-modes 'text-mode
    (sp-local-pair "```" "```"))

  :init
  (defalias 'txt-mode 'text-mode))

(use-package artist-mode
  :bind (:map artist-mode-map
              ("\C-c\C-e" . yc/txt-to-png)))

 ;; ************************** ChangeLog *****************************
(use-package add-log
  :bind (:map change-log-mode-map
              (;;(kbd "<C-return>")
               [C-return] . add-change-log-entry-other-window)))

 ;; markdown
(use-package org-table
  :commands (orgtbl-mode))

(defun yc/translate-markdown-filename (in)
  "Translate IN into filename.."
  (let ((out   (catch 'p-found
    (dolist (ext '("" ".org" ".md"))
      (aif (yc/file-exists-p (concat in ext))
          (throw 'p-found it))
      )
    (concat in ".md"))))

    (PDEBUG "IN: " in
      "OUT: " out)
    out))

(use-package markdown-mode
  :commands (markdown-mode markdown-follow-link-at-point)
  :hook ((markdown-mode . orgtbl-mode)
         (before-save . (lambda ()
                          (when (eq major-mode 'markdown-mode)
                            (save-excursion
                              (goto-char (point-min))
                              (while (search-forward "-+-" nil t)
                                (replace-match "-|-")))))))
  :custom
  (markdown-translate-filename-function 'yc/translate-markdown-filename)
  ;; (markdown-command "markdown_py")
  (markdown-xhtml-header-content
   "<style type=\"text/css\">html { margin: 0; font: .9em/1.6em \"Droid Serif\", \"Lucida Grande\", \"Lucida Sans Unicode\", \"DejaVu Sans\", Verdana, sans-serif; background-attachment: fixed; background-position: right bottom; background-repeat: no-repeat; background-color: white; }  body { font-size: 12pt; line-height: 18pt; color: black; margin-top: 0; }   pre { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace; font-size: 90%; padding: 1.2em; overflow: auto;  line-height: 1.3; font-weight: 100; background-color:#2e3436; box-shadow: 5px 5px 5px #888; border: none; margin-bottom: 10pt; color: white; padding: 1.2em; }  code { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace;} </style>")
  :bind (:map markdown-mode-map
              (;(kbd "C-c C-e")
               "" . markdown-export)
              (;(kbd "C-c o")
               "o" . markdown-follow-link-at-point))
  :mode (((rx (or (: bow "README" eow)
                  ) eol) . markdown-mode)))


 ;; ****************************** Edit Server for Chrome ***************************
(use-package edit-server
  :commands (edit-server-start)
  :config
  (custom-set-variables
   '(edit-server-new-frame nil)
   '(edit-server-url-major-mode-alist
     (list
      (cons (rx (or (: ".css" eow)
                    "Configure.aspx"
                    (: "/admin/plugins" eow)))
            'css-mode)
      (cons (rx (or (: ".htm" (? "l") eow)
                    (: "/posts/" (+ alnum))
                    (: ".asp" eow)))
            'html-mode)
      (cons (rx (+? nonl) "/mediawiki/" (+? nonl) eow)
            'mediawiki-mode)
      (cons (rx (or (:
                     (or "192.168." "172.16.") (+? digit) "." (+? digit))
                    "localhost"
                    "127.0.0.1"
                    "emacs-china.org"
                    "github.com")) 'markdown-mode)))))


(use-package logviewer
  :commands (logviewer-special-handling-csv)
  :mode (((rx (or (: bow "messages" eow)
                  (:  "/" (+? nonl) "_log/" (+? nonl) "."
                      (or "txt" "log" "csv"))
                  (: "." (or "log" "LOG" "Log"))
                  (: (or "log" "LOG" "Log") ".txt")) eol) . logviewer-mode)))

 ;; Htmlize mode
(use-package htmlize
  :commands (htmlize-buffer htmlize-region)
  :config
  (custom-set-variables
   '(htmlize-output-type 'inline-css)))


;; Kconfig-mode
(use-package kconfig-mode  :mode "Kconfig")


 ;; Blog and jekyll
;; (use-package jekyll-modes
;;   :mode (((rx "github.io/" (+ nonl) ".md" eol) . jekyll-markdown-mode)
;;          ((rx "github.io/" (+ nonl) "." (or "html" "htm") eol) . jekyll-html-mode)))

(use-package tblog :commands (tblog/new-post tblog/export tblog/find-file))


(use-package conf-mode

  :mode (rx (or "Doxyfile"
                (: (? "_" (+? nonl)) "init_file" (? "_" (+? nonl)))

                (: (+? ascii) (or "." "_") (or "doxy" "doxygen" "service"  "conf" "config" "rc"
                                               "cnf" "options"))
                (: "fvwm/" (+? ascii))
                (: ".config/" (+? ascii) "rc" buffer-end)
                ".globalrc"
                ".gitmodules"
                "conf\.d/")
            eol))

(use-package fvwm-mode
  :mode (rx ".fvwm/" (+ alnum) eol)
  )

(use-package thrift
  :commands (thrift-mode)
  :mode (((rx ".thrift" eol) . thrift-mode))
  )

 ;; latex
(yc/add-compile-unit 'latex 10
 (when (or (equal ext "tex")
           (equal ext "TEX"))
   (lambda ()
     (format "xelatex %s" file))))

(use-package tex-mode
  :mode (((rx buffer-start "." (or "tex" "latex") buffer-end) . LaTex-mode))
  )


 ;; All others..
(require 'generic-x)


(use-package fontawesome
  :commands (counsel-fontawesome))

(use-package counsel-nerd-fonts
  :commands (counsel-nerd-fonts))

(use-package leetcode
  :commands (leetcode)
  :config
  (progn
    (setq leetcode--user "yangyingchao"
          leetcode-prefer-language "c")

    (defun yc/leetcode-set-langnage ()
      "Description."
      (interactive)
      (setq leetcode-prefer-language
           (ivy-read "Set preferred language to: "
                     '("c" "cpp" "python3" "rust"))))
    ))

(use-package dockerfile-mode
  :mode (rx buffer-start (or "D" "d") "ockerfile" buffer-end))


(provide '07-other-modes)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 07-other-modes.el ends here
