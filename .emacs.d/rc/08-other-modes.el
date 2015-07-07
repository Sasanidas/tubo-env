;;; 08-other-modes.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:


;; ****************************** HTTP Code *****************************
;; Explain the meaning of an HTTP status code. Copy httpcode.el to your
;; load-path and add to your .emacs:

(autoload 'hc "httpcode" "http status code" t)

 ;; ****************************** HTML Mode ******************************
(defun yc/html-remove-tags (start end)
  "Remove tags from region (START - END)."
  (interactive "rp")
  (when (region-active-p)
    (let ((orig (buffer-substring-no-properties start end))
          (r-match-tag (rx
                        (or (: "<" (? "/") (group (+? nonl)) ">")
                            (: bol (group (+ space)))))))
      (with-temp-buffer
        (insert orig)
        (goto-char (point-min))
        (while (search-forward-regexp r-match-tag nil t)
          (replace-match ""))

        (fill-region (point-min) (point-max))
        (kill-new (buffer-substring-no-properties (point-min) (point-max)))))
    (deactivate-mark)))


(defun yc/html-to-org ()
  "Turn region from html to org."
  (interactive)
  (when (region-active-p)
    (let ((orig (buffer-substring-no-properties (region-beginning) (region-end)))
          (r-match-tag (rx
                        (or (: "<" (? "/") (group (+? nonl)) ">")
                            (: bol (group (+ space)))))))
      (with-temp-buffer
        (insert orig)
        (goto-char (point-min))

        ;; handle TAGS
        (while (search-forward-regexp r-match-tag nil t)
          (let* ((tag (match-string 1))
                 (replace
                  (cond
                   ((member tag '("varname" "command"))
                    "=")
                   ((member tag '("emphasis"))
                    "*")
                   (t ""))))

            (replace-match replace)))

        ;; handle special characters
        (dolist (p '(("&copy;" . "(c)")
                     ("&amp;" . "&")
                     ("&lt;" . "<" )
                     ("&gt;" . ">" )))
          (goto-char (point-min))
          (replace-string (car p) (cdr p)))

        ;; fill region and put into kill-ring
        (fill-region (point-min) (point-max))
        (kill-new (buffer-substring-no-properties (point-min) (point-max)))))
    (deactivate-mark)))

(defun yc/semantic-html-parse-headings-adv (func &rest args)
  "Advice for 'semantic-html-parse-headings'.
Call FUNC which is 'semantic-html-parse-headings with ARGS."
  (condition-case var
      (apply func args)
    (error
     (let ((pass1 nil))
       ;; First search and snarf.
       (save-excursion
         (goto-char (point-min))

         (let ((semantic--progress-reporter
                (make-progress-reporter
                 (format "Parsing ...")
                 (point-min) (point-max))))
           (while (re-search-forward semantic-html-super-regex nil t)
             (setq pass1 (cons (match-beginning 0) pass1))
             (progress-reporter-update semantic--progress-reporter (point)))
           (progress-reporter-done semantic--progress-reporter)))

       (setq pass1 (nreverse pass1))
       ;; Now, make some tags while creating a set of children.
       (car (semantic-html-recursive-combobulate-list pass1 0))
       ))
    )

  )

(advice-add 'semantic-html-parse-headings :around #'yc/semantic-html-parse-headings-adv)


(use-package sgml-mode
  :mode ("/itsalltext/" . html-mode)
  :commands (html-autoview-mode)
  :bind (:map sgml-mode-map
              (;(kbd "M-W")
               [134217815] . yc/html-to-org))
  :hook ((html-mode . yc/html-mode-hook)))

(defun yc/html-mode-hook ()
  "My hook for html mode."
  (html-autoview-mode -1)
  (remove-hook 'after-save-hook 'browse-url-of-buffer t)
  (flyspell-mode 1))


 ;; css & scss & sass
(use-package sass :defer t)
(use-package css-mode :defer t
  :mode (rx "." (or "scss" "css" "rasi") eow)
  :custom
  (css-indent-offset 2)
  :hook ((css-mode .  (lambda ()
                        (yc/add-company-backends-with-yasnippet company-css)))))

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
  :hook ((nxml-mode .       (lambda ()
                              (local-set-key (kbd "C-c /") 'nxml-finish-element)
                              (auto-fill-mode)
                              (rng-validate-mode)
                              (hs-minor-mode 1)
                              (yc/add-company-backends-with-yasnippet company-nxml)))))

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
  :init (progn
          (defalias 'txt-mode 'text-mode)
          ))

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

(use-package markdown-mode
  :commands (markdown-mode)
  :hook ((markdown-mode . orgtbl-mode)
         (before-save . (lambda ()
                          (when (eq major-mode 'markdown-mode)
                            (save-excursion
                              (goto-char (point-min))
                              (while (search-forward "-+-" nil t)
                                (replace-match "-|-")))))))
  :custom
  (markdown-command "markdown_py")
  (markdown-xhtml-header-content
   "<style type=\"text/css\">html { margin: 0; font: .9em/1.6em \"Droid Serif\", \"Lucida Grande\", \"Lucida Sans Unicode\", \"DejaVu Sans\", Verdana, sans-serif; background-attachment: fixed; background-position: right bottom; background-repeat: no-repeat; background-color: white; }  body { font-size: 12pt; line-height: 18pt; color: black; margin-top: 0; }   pre { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace; font-size: 90%; padding: 1.2em; overflow: auto;  line-height: 1.3; font-weight: 100; background-color:#2e3436; box-shadow: 5px 5px 5px #888; border: none; margin-bottom: 10pt; color: white; padding: 1.2em; }  code { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace;} </style>")
  :bind (:map markdown-mode-map
              (;(kbd "C-c C-e")
               "" . markdown-export))
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
    )
  )

(use-package dockerfile-mode
  :mode (rx buffer-start (or "D" "d") "ockerfile" buffer-end)
  )



(provide '08-other-modes)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 08-other-modes.el ends here
