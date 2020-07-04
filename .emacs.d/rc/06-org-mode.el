;;; 081-org-mode.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

 ;; PlantUML,

(defun yc/plantuml-path ()
  "Get path of plantUML."
  (catch 'path-found
    (dolist (path `(,(expand-file-name "~/.local/share/plantuml/lib/plantuml.jar")
                    "/usr/share/plantuml/lib/plantuml.jar"
                    "/usr/local/plantuml/lib/plantuml.jar"
                    "/usr/local/opt/plantuml/libexec/plantuml.jar"))
      (PDEBUG "CHECKING: " path)
      (when (file-exists-p path)
        (throw 'path-found path)))
    ""))


(defun uml/parse-stringfied-nodes (&optional silent)
  "Parse all stringfied nodes.
If `silent' is nil, print collected nodes before exit."
  (interactive)
  (setq uml/stringified-nodes nil)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx bol (* space)
                (or "enum" "class" "interface")
                (+ space)
                (group (+? nonl))
                (* space)
                "{" eol) nil t)
      (add-to-list 'uml/stringified-nodes (match-string-no-properties 1))))
  (unless silent
    (message "Stringfied notes: %s" (s-join ", " uml/stringified-nodes))))

(use-package flycheck-plantuml
  :commands (flycheck-plantuml-setup))

(defun company-plantuml--candidates (prefix)
  "Return candiates for `PREFIX'."
  (all-completions prefix plantuml-kwdList))

;;;###autoload
(defun company-plantuml (command &optional arg &rest ignored)
  "`company-mode' completion backend for plantuml.
plantuml is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-plantuml))
    (init (unless (equal major-mode 'plantuml-mode)
            (error "Major mode is not plantuml-mode")))
    (prefix (and (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-plantuml--candidates arg))))

(use-package plantuml-mode
  :pin melpa
  :mode (rx "." (or "plantuml" "puml" "plu" "pu") eow)
  :hook ((plantuml-mode .
                        (lambda ()
                          (flycheck-plantuml-setup)
                          (yc/add-company-backends-with-yasnippet company-plantuml)
                          (uml/parse-stringfied-nodes t))))
  :config
  (progn
    (setq plantuml-jar-path (yc/plantuml-path))
    (aif (executable-find "dot")
        (setenv "GRAPHVIZ_DOT" it)
      (warn "plantUML depends on graphviz, which is not installed."))
    )
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4))

(use-package ffap
  :commands (ffap-url-p))

(defun yc/org-download-annotate-func (link)
  "Annotate LINK with the time of download."
  (let ((lable (format "fig:%s"
                       (replace-regexp-in-string
                        (rx (+ space)) "_"
                        (file-name-sans-extension (file-name-nondirectory link)) t t))))

    (kill-new (format "[[%s]]" lable))
    (concat "#+CAPTION: \n"
            (format "#+NAME: %s\n" lable)
            (if (ffap-url-p link)
                (format "#+DOWNLOADED: %s @ %s\n"
                        link
                        (format-time-string "%Y-%m-%d %H:%M:%S"))
              ""))))

(defun yc/org-download-screenshot-adv (func &rest args)
  "Advice for 'org-download-screenshot'.
Call FUNC which is 'org-download-screenshot with ARGS.
Change default filename before applying original function."
  (let ((org-download-screenshot-file
         (expand-file-name (format-time-string "screenshot@%Y-%m-%d_%H:%M:%S.png")
                           temporary-file-directory)))
    (apply func args)))


(defun yc/get-image-width (filename)
  "Returns width of file FILENAME in pixel."
  (unless (file-exists-p filename)
    (error "File %s does not exist!" filename))
  (with-temp-buffer
    (insert-image-file filename)
    (car (image-size
          (image-get-display-property) t))))

(defun yc/org-download-insert-link-adv (func link filename)
  "Advice for 'org-download-insert-link'.
Call FUNC which is 'org-download-insert-link with ARGS."
  (let* ((width (if (fboundp 'image-size)
                    ;; if function `image-size' is avaiable,  we can do some calculation.
                    (if (file-exists-p filename)
                        ;; if file exists, calculate width to be used.
                        (let ((actual-width (yc/get-image-width filename)))
                          (if (> actual-width 1024)
                              1024 0))
                      ;; or, set to -1, and update it later.
                      -1)
                  ;; otherwise, return 0 to disable this feature.
                  0))
           (org-download-image-html-width width)
           (org-download-image-org-width width))
    (PDEBUG "width: " width)
    (funcall func link filename)))

(defun yc/org-download--image/url-retrieve-adv (link filename)
  "advice for 'org-download--image/url-retrieve'.
call func which is 'org-download--image/url-retrieve with args."
(url-retrieve
   link
   (lambda (status filename buffer)
     (org-download--write-image status filename)
     (cond ((org-download-org-mode-p)
            (with-current-buffer buffer
              (org-download--display-inline-images)))
           ((eq major-mode 'dired-mode)
            (let ((inhibit-message t))
              (with-current-buffer (dired (file-name-directory filename))
                (revert-buffer nil t)))))
     (with-current-buffer buffer
       (let* ((width (round (yc/get-image-width filename)))
              (width-str (concat (number-to-string (if (> width 960) 960 width)) "px")) )
         (save-excursion
           (let ((end (point)))
             (forward-line -4)
             (PDEBUG "W:" (point) end (buffer-substring-no-properties (point) end))
             (narrow-to-region (point) end)
             (while (search-forward "-1px" nil t)
               (replace-match width-str nil t)))
           (widen)))

       (org-download--display-inline-images)
       )
     )
   (list
    (expand-file-name filename)
    (current-buffer))
   nil t))


(use-package org-download
  :pin melpa
  :commands (org-download-image org-download-screenshot)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp nil)
  (org-download-image-html-width 1024)
  (org-download-image-org-width 1024)

  :config
  (setq-default
    org-download-screenshot-method
    (cond
     ((executable-find "screencapture") "screencapture -i %s")
     ((executable-find "scrot") "scrot -s %s")
     ((executable-find "gnome-screenshot") "gnome-screenshot -a -f %s")
     (t "")))
  (setq org-download-annotate-function 'yc/org-download-annotate-func
        org-download-file-format-function 'identity)

  (advice-add 'org-download-screenshot :around #'yc/org-download-screenshot-adv)
  (advice-add 'org-download-insert-link :around
              #'yc/org-download-insert-link-adv)
  (advice-add 'org-download--image/url-retrieve :override
              #'yc/org-download--image/url-retrieve-adv)
  )


 ;; *************************** Org Mode ********************************
(use-package org-indent
  :commands (org-indent-mode))

;; hide blocks marked as :hidden
(defun individual-visibility-source-blocks ()
  "Fold some blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (and
              (save-excursion
                (beginning-of-line 1)
                (looking-at org-block-regexp))
              (cl-assoc
               ':hidden
               (cl-third
                (org-babel-get-src-block-info))))
         (org-hide-block-toggle))))))


;; Auto set CUSTOM_ID...
(defun yc/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id))
        (if (called-interactively-p)
            (message "ORG-ID: %s" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        (if (called-interactively-p)
            (message "ORG-ID: %s" id))
        id)))))

(defun yc/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (eq major-mode 'org-mode))
    (delete-trailing-whitespace)
    (save-excursion
      (org-map-entries (lambda () (yc/org-custom-id-get (point) 'create)) nil 'file))))

(add-hook 'before-save-hook 'yc/org-add-ids-to-headlines-in-file)

(defun yc/org-mode-hook ()
  "My hooks for org mode."
  (interactive)

  (if window-system
      (org-display-inline-images))

  (setq show-trailing-whitespace t)

  (flyspell-mode)

  (individual-visibility-source-blocks)

  ;; modify syntax-entry, so electric-pair can work better.
  (modify-syntax-entry ?$ "\$")
  (modify-syntax-entry ?= "(=")
  )

(defun yc/org-html-paragraph-adv (func &rest args)
  "Advice for 'org-html-paragraph'.
Call FUNC which is 'org-html-paragraph with ARGS."
       "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
       (let ((orig-contents (cadr args))
             (reg-han "[[:multibyte:]]"))
         (setf (cadr args) (replace-regexp-in-string
                            (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                            "\\1\\2" orig-contents))
         (apply func args)))


(defun yc/org-latex-special-block-adv (func &rest args)
  "Advice for 'org-latex-special-block'.
Call FUNC which is 'org-latex-special-block with ARGS."
  (if (string= (org-element-property :type (car args)) "NOTES")
      (cadr args)
    (apply func args)))


(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun yc/org-open-at-point-adv (func &rest args)
  "Advice for 'org-open-at-point'.
Call FUNC which is 'org-open-at-point with ARGS."
  (let ((m (point-marker)))
    (progn
      (apply func args)
      (yc/push-stack m))))



(defun yc/org-comment-line-break-function (func &rest args)
  "Wrapper of: `org-comment-line-break-function'.
Ignore error signal in `org-comment-line-break-function'."
  (condition-case var
      (apply func args)
    (error nil)))



(use-package ol
  :bind (("\C-cl" . org-store-link)))

(use-package org-agenda
  :custom
  (org-agenda-files (list (expand-file-name "~/Documents/Database/org/")))
  (org-agenda-dim-blocked-tasks (quote invisible))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)

  :bind(("\C-ca" . org-agenda))
  :config
    (dolist (item (directory-files org-directory t))
      (unless (or (not (file-directory-p item))
                  (member (file-name-base item) '(".git" "slides" "images" "assets" "references" "." "..")))
        (add-to-list 'org-agenda-files item)))
  )

(use-package org-capture :bind ((;; ,(kbd "<M-S-f10>")
                                 [M-S-f10]. org-capture))
  :config
  (custom-set-variables
   `(org-capture-templates
     `(("t" "Todo" entry (file+headline ,(format "%s/gtd.org" org-directory) "Tasks")
        "\n* TODO %?\n  %i\n  %a")
       ("p" "Project" entry (file+headline ,(format "%s/gtd.org" org-directory) "Project")
        "
** NOTE %?\n %i\n %a" )
       ("n" "New" entry (file+datetree ,(format "%s/inbox.org" org-directory) "Inbox")
        "* %?\nEntered on %U\n  %i\n  %a")
       ("i" "Idea" entry (file+headline ,(format "%s/gtd.org" org-directory)"Idea")
        "* %?\nEntered on %U\n  %i\n  %a")
       ("w" "Web site" entry
        (file "")
        "* %a :website:\n\n%U %?\n\n%:initial")))))


(defun yc/org-update-yank ()
  "Update yanked text, removing or adding proper spaces."
  (interactive)
  (save-excursion
    ;; 测试 AAA --> 测试 AAA
    (goto-char (point-min))
    (while (search-forward-regexp (rx (group (not ascii))  (* blank) (group  ascii)) nil t)
      (replace-match "\\1 \\2" nil nil))

    ;;  AAA测试 -->  AAA 测试
    (goto-char (point-min))
    (while (search-forward-regexp (rx  (group  ascii) (* blank) (group (not ascii))) nil t)
      (replace-match "\\1 \\2" nil nil))

    ;; remove leading space
    (goto-char (point-min))
    (while (search-forward-regexp (rx  bol (+ blank)) nil t)
      (replace-match "")))

  ;; remove trailing space
  (delete-trailing-whitespace))

(defun yc/org-ctrl-c-ctrl-c-adv (&rest args)
  "Advice for 'org-ctrl-c-ctrl-c'.
Call FUNC which is 'org-ctrl-c-ctrl-c with ARGS."

  (PDEBUG "ENTER2: " (current-buffer))

  (when window-system
    (org-redisplay-inline-images)))



(defvar-local yc/org-post-save-timer nil)

(defun yc/org-roam-store-link-adv (arg &optional interactive?)
  "Advice for 'org-roam-store-link'.
Call FUNC which is 'org-roam-store-link with ARGS."
  (org-store-link arg interactive?))

(defun yc/grep-roam-files (x)
  "Description X."
  (interactive)
  (let ((default-directory (expand-file-name "~/Documents/Database/org/")))
    (yc/counsel-grep)))

(defun yc/org-roam-db--update-file-adv (func &optional file-name)
  "Advice for 'org-roam-db--update'.
Call FUNC which is 'org-roam-db--update with ARGS."
  (if yc/org-post-save-timer
      (cancel-timer yc/org-post-save-timer))

  (setq yc/org-post-save-timer
        (run-with-idle-timer
         5 nil
         (lambda (f &optional x)
           (PDEBUG "update db for file: " x)
           (funcall f x)
           (PDEBUG "after updating db.")
           (setq yc/org-post-save-timer nil))
         func file-name)))

(use-package org-roam
  :pin melpa
  :commands (org-roam-buffer-toggle-display org-roam-insert)

  :custom
  (org-roam-directory (expand-file-name "~/Documents/Database/org/"))
  (org-roam-graph-viewer `,(cond
                            ((executable-find "firefox") (executable-find "firefox"))
                            ((eq system-type 'darwin) "/usr/bin/open")
                            (t nil)))
  (org-roam-completion-system 'ivy)
  :hook ((org-mode . (lambda () (unless org-roam-mode (org-roam-mode 1)))))
  :bind (:map org-roam-backlinks-mode-map
              ("q" . org-roam-buffer-deactivate))

  :config
  (ivy-add-actions
   'org-roam--completing-read
   '(("g" yc/grep-roam-files "grep in current directory")))

  (advice-add 'org-roam-db--update-file :around #'yc/org-roam-db--update-file-adv)
  (advice-add 'org-roam-store-link :override #'yc/org-roam-store-link-adv))


 ;; auto-insert for org-mode.

(defun auto-insert--org-mode (&optional fn)
  "Update for `org-mode'.
 Update file name, replace '-' or '_' with whitespace."
  (interactive)
  (auto-update-defaults)

  (let* ((fn (or fn (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
         (bname (if (string-match
                     (rx bol
                         (repeat 4 digit) "-"   ;; year
                         (repeat 1 2 digit) "-" ;; month
                         (repeat 1 2 digit)     ;; day
                         "-" (group (* anything)) eol)
                     fn)
                    (match-string 1 fn)
                  fn))
         (replacement (replace-regexp-in-string (regexp-quote "-") " " bname t t )))

    (yc/auto-update-template "FILE_NO_EXT_MINUS" replacement))


  ;; create directory for images...
  (unless (file-directory-p "images")
    (mkdir "images"))

  (let* (tex)
    (when (executable-find "latex")
      (cond
       ((executable-find "convert")
        (setq tex "tex:imagemagick"))
       ((executable-find "dvipng")
        (setq tex "tex:dvipng"))
       (t
        (warn "latex installed, but can be used to convert math, imagemagick or dvipng is needed...")
        )))

    (if tex
        (progn
          (yc/auto-update-template "TEX" tex)
          (yc/auto-update-template "TEX_PREVIEW" "latexpreview"))

      (progn
        (yc/auto-update-template "TEX" "")
        (yc/auto-update-template "TEX_PREVIEW" "")))))

(defun counsel/org-link-target ()
  "List files under point??"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-at-bol) (point))))
    (if (string-match (rx "[[" (group (+ nonl)) eol) text)
        (let* ((dir (match-string 1 text))
               (target (counsel-list-directory dir nil 'identity)))

          (insert (file-name-nondirectory target)))
      (error "Can't find target for %s" text))))

(use-package org-superstar
  :pin melpa
  :hook ((org-mode . org-superstar-mode))
  :custom
  (org-superstar-headline-bullets-list '( "●"  "◎" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

(use-package ox-latex
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-default-figure-position "htbp";; "H"
                                     )
  (org-latex-prefer-user-labels t)
  (org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks={true},
CJKbookmarks={true},
linkcolor={black},
urlcolor={blue},
menucolor={blue}}
")
  :config
  (advice-add 'org-latex-special-block :around #'yc/org-latex-special-block-adv))


(defun yc/org-publish-needed-p-adv (func &rest args)
  "Advice for 'org-publish-needed-p'.
Call FUNC which is 'org-publish-needed-p with ARGS.
Restore to current location after executing."
  (save-excursion
    (apply func args)))

(use-package ox-publish
  :commands (org-publish-needed-p)
  :config
  (advice-add 'org-publish-needed-p :around #'yc/org-publish-needed-p-adv))


(use-package org
  :custom
  (org-image-actual-width nil)
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (expand-file-name "~/Documents/Database/org/notes.org"))
  (org-directory (expand-file-name "~/Documents/Database/org/"))
  (org-export-time-stamp-file nil)
  (org-hide-leading-stars t)
  (org-html-head-include-default-style nil)
  (org-html-head-include-scripts nil)
  (org-log-done 'time)
  (org-plantuml-jar-path (yc/plantuml-path))
  (org-pretty-entities t)
  (org-preview-latex-image-directory "/tmp/")
  (org-publish-list-skipped-files nil)
  (org-refile-targets (quote ((nil :maxlevel . 2))))  (org-html-postamble nil)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-ditaa-jar-path (yc/plantuml-path))
  (org-use-property-inheritance t)
  (org-enforce-todo-checkbox-dependencies t)
  ;; WAITING: Assigned to others, and waiting for their report.
  ;; PENDING: Pending for some reason, maybe scheduled but not started because task dependency.
  (org-todo-keywords (quote ((sequence "TODO(t)" "WAITING(w)" "DOING(g)"
                                       "DONE(d)" "CANCELED(c)" "PENDING(p)" ))))
  (org-tag-alist '(("question" . ?q) ("noexport" . ?n)))
  (org-tag-faces
   '(("HIGH" . (:foreground "red" :weight bold)) ("MEDIUM" . org-warning)
     ("LOW" . (:foreground "blue" :weight bold))))

  (org-return-follows-link t) ;; make RET follow links


  :hook ((org-after-todo-statistics . org-summary-todo)
         (org-mode . yc/org-mode-hook))

  :config
  (progn
    (require 'ox-plus)

    (sp-with-modes 'org-mode
      (sp-local-pair "$" "$")
      (sp-local-pair "（" "）")
      (sp-local-pair "“" "“"))

    (setq org-comment-string "ORG_COMMENT")

    (org-indent-mode 1)

    (font-lock-add-keywords
     'org-mode
     `((,(rx bow (group "TODO "))
        (1 font-lock-comment-face))
       (,(rx bow (group "DONE "))
        (1 font-lock-builtin-face))
       (,(rx bow (group "DOING "))
        (1 font-lock-function-name-face))))

    (advice-add 'org-html-paragraph :around 'yc/org-html-paragraph-adv)
    (advice-add 'org-open-at-point :around #'yc/org-open-at-point-adv)
    (advice-add 'org-ctrl-c-ctrl-c :after #'yc/org-ctrl-c-ctrl-c-adv)
    (advice-add 'org-edit-special :before #'layout-save-current)
    (advice-add 'org-edit-src-exit :after #'layout-restore)
    (advice-add 'org-comment-line-break-function :around #'yc/org-comment-line-break-function)

    (substitute-key-definition
     'org-cycle-agenda-files  'backward-page org-mode-map)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ditaa . t)
       (dot . t)
       (plantuml . t)
       (gnuplot . t)))
    )

  :bind (:map org-mode-map
              (;; ,(kbd "")
               [138]. org-meta-return)
              (;; ,(kbd "C-c j")
               "j". org-meta-return)


              (;;(kbd "M-m")
               [134217837] . yc/show-methods-dwim)

              (;; (kbd "C-x ds")
               "ds" . org-download-screenshot)
              (;; (kbd "C-x du")
               "du" . org-download-image)

              (;; (kbd "C-x n t")
               "nt" . org-roam-buffer-toggle-display)
              (;; (kbd "C-x n i")
               "ni" . org-roam-insert)

              ))

(defun yc/my-noter-mode-hook ()
  "Description."
  (unless (layout-restore)
    (PDEBUG "BUF" (current-buffer))
    (PDEBUG "FILE:" buffer-file-name)
    ;; (when (s-ends-with? ".pdf" buffer-file-name)
    ;;   (enlarge-window-horizontally (truncate (* (window-width) 0.5))))
    (dolist (win (window-list))
      (select-window win)
      (layout-save-current)))
  ;; (my-noter-sync-pdf-page-next)
  )

(use-package my-noter
  :commands (my-noter
             my-noter/dispatch-file my-noter/dispatch-directory
             my-noter/find-file)
  :bind (:map ctl-x-map
              ("nF" . my-noter/find-file)
              ("nn" . my-noter))

  :hook ((my-noter-mode . yc/my-noter-mode-hook))
  :custom
  (my-noter-disable-narrowing t))

(defun yc/deft-auto-populate-title-maybe-adv (file)
  "Advice for 'deft-auto-populate-title-maybe'.
Call FUNC which is 'deft-auto-populate-title-maybe with ARGS."
  (with-temp-file file
    (org-mode)
    (auto-insert))
  )

(use-package deft
  :pin melpa
  :custom
  (deft-recursive t)
  (deft-directory (expand-file-name "~/Documents/Database/org/"))
  (deft-file-naming-rules '((noslash . "_")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-extensions '("org" "md" "txt"))
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)

  :bind (:map ctl-x-map
              ("nf" . deft))
  :config
(advice-add 'deft-auto-populate-title-maybe :override #'yc/deft-auto-populate-title-maybe-adv)


  )


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 06-org-mode.el ends here
