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

(use-package plantuml+
  :commands (company-plantuml plantuml-indent-line))


(use-package plantuml-mode
  :mode (rx "." (or "plantuml" "puml" "plu" "pu") eow)
  :hook ((plantuml-mode .
                        (lambda ()
                          (flycheck-plantuml-setup)
                          (setq-local indent-line-function 'plantuml-indent-line)
                          (yc/add-company-backends-with-yasnippet company-plantuml)
                          (uml/parse-stringfied-nodes t))))
  :config
  (progn
    (setq plantuml-jar-path (yc/plantuml-path))
    (aif (executable-find "dot")
        (setenv "GRAPHVIZ_DOT" it)
      (warn "plantUML depends on graphviz, which is not installed."))
))

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
             (string-match-p ".*yangyingchao.gitub.io.*" buffer-file-name)
             (eq major-mode 'org-mode))
    (save-excursion
      (org-map-entries (lambda () (yc/org-custom-id-get (point) 'create)) nil 'file))))

(add-hook 'before-save-hook 'yc/org-add-ids-to-headlines-in-file)

(defun yc/org-mode-hook ()
  "My hooks for org mode."
  (interactive)

  (if window-system
      (org-display-inline-images))

  (flyspell-mode)

  (individual-visibility-source-blocks)

  ;; modify syntax-entry, so electric-pair can work better.
  (modify-syntax-entry ?$ "\$")
  (modify-syntax-entry ?= "(=")

  ;; update latex img directory.
  (setq org-preview-latex-image-directory
        (cond
         ((and buffer-file-name
               (string-match
                (rx (group buffer-start (+? nonl) "yangyingchao.github.io/" ))
                buffer-file-name))
          (concat (match-string 1 buffer-file-name) "assets/img/"))

         ((file-directory-p "images")
          (expand-file-name "images")
          )

         (t "/tmp/")
         )
        )

  (PDEBUG "LATEX_DIR: " org-preview-latex-image-directory)
  )

(use-package org
  :commands (org-version org-load-modules-maybe)
  :custom
  (org-agenda-files (list (expand-file-name "~/Work/org")
                          (expand-file-name "~/Work/yangyingchao.github.io/org")))
  (org-agenda-dim-blocked-tasks (quote invisible))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (expand-file-name "~/Work/org/notes.org"))
  (org-directory (expand-file-name "~/Work/org"))
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
  (org-tag-faces
   '(("HIGH" . (:foreground "red" :weight bold)) ("MEDIUM" . org-warning)
     ("LOW" . (:foreground "blue" :weight bold))))

  :hook ((org-after-todo-statistics . org-summary-todo)
         (org-mode . yc/org-mode-hook))

  :config
  (progn
    (require 'ox-plus)
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
              ("\C-cl" . org-store-link)
              ("\C-ca" . org-agenda    )
              ("\C-cb" . org-iswitchb  )
              (;;(kbd "M-m")
               [134217837] . yc/show-methods-dwim)
              )
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
  (progn
    (advice-add 'org-latex-special-block :around #'yc/org-latex-special-block-adv)
    )
  )

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

(defun open-mylist ()
  "Open my gtd file."
  (interactive)
  (unless (boundp 'ord-dir)
    (load-library "ox-plus"))
  (find-file (concat org-directory "/gtd.org")))

(global-set-key [(control f1)] 'open-mylist)

(use-package org-agenda :bind ((;; ,(kbd "<C-S-f1>")
                                [C-S-f1]. org-agenda)))
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

(use-package org-bullets
  :pin melpa
  :commands (org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "◇" "✚" "✜" "☯" "◆" ))
  :hook ((org-mode . org-bullets-mode)))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 06-org-mode.el ends here
