;;; ox-plus.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(autoload 'yc/file-exists-p "yc-utils" ""  t)


(defcustom my-link-home  "http://localhost"  "Link to homepage."
  :type 'string
  :group 'ox-plus)

(defcustom www-base-dir (expand-file-name "~/Work/yangyingchao.github.io/")
  "Base directory to hold both org files and htdocs.")

(defcustom org-dir (format "%s/org/" www-base-dir) "")

(defcustom htdoc-dir (format "%s/_posts/" www-base-dir) "")

(defconst my-html-head-extra
  "
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/bootstrap.min.css\">
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/style.css\">
<script src=\"/assets/js/jquery-2.1.3.min.js\" type=\"text/javascript\"></script>
<script src=\"/assets/js/org-exported.js\" type=\"text/javascript\"></script>"
  "Extra strings to put into html head.")

(defconst my-html-head-extra-top
  "
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/bootstrap.min.css\">
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/style.css\">
<script src=\"/assets/js/jquery-2.1.3.min.js\" type=\"text/javascript\"></script>
<script src=\"/assets/js/org-exported.js\" type=\"text/javascript\"></script>"
  "Extra strings to put into html head.")


(defun org-slides-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (if (string-match "sitemap.org" filename)
      (let ((new-plist (plist-put
                        (plist-put plist :html-head-extra my-html-head-extra)
                        :html-link-home my-link-home
                        )))
        (PDEBUG "plist:" plist)
        (PDEBUG "publishing file: " filename "with `org-html-publish-to-html'")
        (org-html-publish-to-html new-plist filename pub-dir))

    (PDEBUG "publishing file: " filename "with `org-ioslide-export-to-html'")
    (with-current-buffer (find-file-noselect filename)
      (org-ioslide-export-to-html))))


(defun org-compressed-publish-to-html (plist filename pub-dir)
  "Publish an compressed file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (mkdir pub-dir t))

  (PDEBUG "PLIST:" plist)
  (PDEBUG "FILENAME:" filename)
  (PDEBUG "PUBDIR:" pub-dir)

  (let ((base-dir (plist-get plist :base-directory)) )
    (dolist (item (directory-files base-dir t))
      (PDEBUG "ITEM: " item)
      (if (string-match (rx "/" (or "." "..") eol) item)
          (PDEBUG "Skip item:" item)
        (if (file-directory-p item)
            ;; publish directories as attachment..
            (let ((output (expand-file-name (file-name-nondirectory item) pub-dir)))
              (PDEBUG "Publishing DIRECTORY:" item)
              (unless (file-equal-p (expand-file-name (file-name-directory item))
			                        (file-name-as-directory (expand-file-name pub-dir)))
                (copy-directory item output t))
              ;; Return file name.
              output)

          (PDEBUG "Publishing FILE:" item)
          (cond
           ((string-match ".*\.org$" item)
            (org-html-publish-to-html plist item pub-dir))
           ((string-match (rx "." (or "7z" "rar" "gz" "bz2" "tar") eol) item)
            (let ((default-directory pub-dir))
              (shell-command (format "~/.emacs.d/tools/unpack.sh %s" item))))
           (t (error "Unhandled file: %s" item))))))))

(autoload 'tblog/--convert-item "tblog")


(defun org-top-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (when (string-match "sitemap.org" filename)
    (org-html-publish-to-html plist filename pub-dir))
  )


(defun org-publish-jekyll (plist filename pub-dir)
  "Publish an org file to HTML, with tblog/--convert-item.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.


Return output file name."

  ;; special handling for my github page...
  (unless (string-match-p "yangyingchao.github.io" filename)
    (error "Should be used against github page only: %s" filename))

  (tblog/--convert-item filename t))

(defun add-to-project-list (dir &optional rec)
  "Description."
  (interactive)
  (PDEBUG "Adding DIR: " dir)
  (add-to-list 'org-publish-project-alist
               `(,(format "org-%s" dir)
                 :base-directory ,(concat org-dir "/" dir)
                 :base-extension "org"
                 :publishing-directory ,(concat htdoc-dir "/" dir)
                 :recursive rec
                 :publishing-function org-html-publish-to-html
                 :section-numbers nil
                 :with-toc t
                 :makeindex nil
                 :auto-sitemap t
                 :auto-preamble t
                 :sitemap-filename "index.org"
                 :html-preamble t
                 :html-head-extra ,my-html-head-extra
                 :html-link-up "sitemap.html"
                 :html-link-home ,my-link-home
                 ))

  (add-to-list 'org-publish-project-alist
               `(,(format "org-%s-static" dir)
                 :base-directory ,(concat org-dir "/" dir)
                 :publishing-directory ,(concat htdoc-dir "/" dir)
                 :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf" "txt"
                                          "gz" "html"))
                 :recursive rec
                 :publishing-function org-publish-attachment)
               t)
  nil)


;; (when (try-require 'mediawiki)

;;   (defun org-mw-export-as-mediawiki-string ()
;;     "Export current buffer to a Mediawiki string."
;;     (interactive)
;;     (let* (str)
;;       (save-excursion
;;         (with-current-buffer  (org-export-to-buffer 'mw "ox-mw-buffer")
;;           (goto-char (point-min))
;;           (setq str  (buffer-substring-no-properties (point-min) (point-max)))
;;           (kill-buffer (current-buffer))))
;;       str))


;;   (defmacro twiki--mkfield (x)
;;     `(rx bol ,x (* space) (group (+? nonl) eol)))

;;   (defun twiki-fetch-field (regex &optional SPLIT)
;;     "Fetch all fields, and return a concatenated string.
;; REGEX: format used by regex.
;; FUNC: function to be called."
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (search-forward-regexp regex nil t)
;;         (let ((result (match-string-no-properties 1)))
;;           (if (string-match "(nil)" result)
;;               (setq result nil))
;;           (when (and SPLIT result)
;;             (print (string-split result SPLIT))
;;             (setq result (mapconcat 'identity (string-split result SPLIT) " ")))
;;           result))))

;;   (defun get-category ()
;;     "Get category name from FILENAME."
;;     (print (buffer-file-name))
;;     (aif (twiki-fetch-field (twiki--mkfield "#+CATEGORY:"))
;;         (print it)
;;       it
;;       (aif (buffer-file-name)
;;           (let* ((cmps (string-split (file-name-directory it) "/"))
;;                  (pos (- (length cmps) 2)))
;;             (aif (and (> pos 0)
;;                       (nth pos cmps))
;;                 (if (> (length it) 0)
;;                     (setq category it)
;;                   )))
;;         "Default")))

;;   (defun org-mw-title ()
;;     "Get tigle."
;;     (interactive)
;;     (let ((title "Untitled."))
;;       (save-excursion
;;         (goto-char (point-min))
;;         (when (search-forward-regexp (rx "#+TITLE:" (* space) (group (+ nonl))) nil t)
;;           (setq title (match-string-no-properties 1))))
;;       title))

;;   (defvar org-mw-exporting nil "Nil.")

;;   ;; TODO: Handle images....
;;   (defun org-mw-publish ()
;;     "Publish it.."
;;     (interactive)
;;     (let* ((org-mw-exporting t)
;;            (category (get-category))
;;            (r-match-section (format "^==[[:space:]]*%s[[:space:]]*==$" category))
;;            (item-name (org-mw-title))
;;            (content (org-mw-export-as-mediawiki-string))
;;            (r-match-item (format "^\\* \\[\\[%s]]$" item-name))
;;            (img-dir "images")
;;            item-start item-end)
;;       (save-excursion
;;         ;; Push images first.
;;         (message "Pushing images...")
;;         (if (file-exists-p img-dir)
;;             (with-temp-buffer
;;               (call-process "sudo" nil t nil
;;                             "php"
;;                             (format "%s/localhost/htdocs/mediawiki/maintenance/importImages.php" www-base-dir)
;;                             "--overwrite"
;;                             img-dir)
;;               (print (buffer-substring (point-min) (point-max)))))

;;         ;; Create new page if necessary.
;;         (with-current-buffer (mediawiki-site w-sitename)
;;           (goto-char (point-min))

;;           ;; locate Section.
;;           (if (search-forward-regexp r-match-section nil t)
;;               ;; Find interested section
;;               (progn
;;                 (setq item-start (match-end 0))
;;                 (goto-char item-start)
;;                 (if (search-forward-regexp "^==" nil t)
;;                     (setq item-end (1- (match-beginning 0)))
;;                   (setq item-end (point-max))))

;;             ;; Insert new section
;;             (goto-char (point-max))
;;             (insert (format "\n== %s ==" category))
;;             (setq item-start (point)
;;                   item-end (point-max)))

;;           ;; Locate Item
;;           (goto-char item-start)
;;           (if (search-forward-regexp r-match-item item-end t)
;;               (kill-buffer (current-buffer))
;;             (goto-char item-end)
;;             (insert (format "\n* [[%s]]" item-name))
;;             (sort-lines nil item-start (point))
;;             (mediawiki-save-and-bury (format "added entry: %s" item-name))))

;;         (let ((mediawiki-site mediawiki-site-default) )
;;           (with-current-buffer (mediawiki-open item-name)
;;             (erase-buffer)
;;             (insert content)
;;             (mediawiki-save-and-bury (format "updated entry: %s" item-name)))))))

;;   (defun org-mw-target (target contents info)
;;     "Transcode a TARGET object from Org to HTML.
;; CONTENTS is nil.  INFO is a plist holding contextual
;; information."
;;     "")

;;   (yc/eval-after-load
;;     "ox-publish"
;;     (require 'ox-mediawiki)
;;     (org-export-define-derived-backend
;;      'mw2 'mw
;;      :filters-alist '((:filter-parse-tree . org-mw-separate-elements))
;;      :menu-entry
;;      '(?m "Export to Mediawiki"
;;           ((?p "Publish to wiki" (lambda (a s v b) (org-mw-publish)))))
;;      :translate-alist '((bold . org-mw-bold)
;;                         (code . org-mw-verbatim)
;;                         (underline . org-mw-verbatim)
;;                         (comment . (lambda (&rest args) ""))
;;                         (comment-block . (lambda (&rest args) ""))
;;                         (example-block . org-mw-example-block)
;;                         (fixed-width . org-mw-example-block)
;;                         (footnote-definition . ignore)
;;                         (footnote-reference . org-mw-footnote-reference)
;;                         (headline . org-mw-headline)
;;                         (horizontal-rule . org-mw-horizontal-rule)
;;                         (inline-src-block . org-mw-verbatim)
;;                         (italic . org-mw-italic)
;;                         (item . org-mw-item)
;;                         (line-break . org-mw-line-break)
;;                         (link . org-mw-link)
;;                         (paragraph . org-mw-paragraph)
;;                         (plain-list . org-mw-plain-list)
;;                         (plain-text . org-mw-plain-text)
;;                         (quote-block . org-mw-quote-block)
;;                         (quote-section . org-mw-example-block)
;;                         (section . org-mw-section)
;;                         (src-block . org-mw-example-block)
;;                         (inner-template . org-mw-inner-template)
;;                         (template . org-mw-template)
;;                         (verbatim . org-mw-verbatim)
;;                         (table . org-mw-table)
;;                         (table-cell . org-mw-table-cell)
;;                         (table-row . org-mw-table-row)
;;                         (target . org-mw-target))))

;;   (defun yc/org-mw-verbatim (func verbatim contents info)
;;     "Advice for `org-mw-verbatim'.
;; Call FUNC with ARGS."
;;     (format "<code>%s</code>"
;;             (org-element-property :value verbatim))
;;     )

;;   (advice-add 'org-mw-verbatim :around #'yc/org-mw-verbatim)

;;   (defun yc/org-mw-plain-text (func text info)
;;     "Advice for `org-mw-plain-text'.
;; Call FUNC with ARGS."

;;     (when (plist-get info :with-smart-quotes)
;;       (setq text (org-export-activate-smart-quotes text :html info)))
;;     ;; Protect ambiguous #.  This will protect # at the beginning of
;;     ;; a line, but not at the beginning of a paragraph.  See
;;     ;; `org-mw-paragraph'.
;;     (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
;;     ;; Protect ambiguous !
;;     (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
;;     (when (plist-get info :with-special-strings)
;;       (setq text (org-html-convert-special-strings text)))
;;     ;; Handle break preservation, if required.
;;     (when (plist-get info :preserve-breaks)
;;       (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
;;     ;; Return value.
;;     text)

;;   (advice-add 'org-mw-plain-text :around #'yc/org-mw-plain-text)

;;   (defun yc/org-mw-link (func link contents info)
;;     "Advice for `org-mw-link'.
;; Call FUNC with ARGS."

;;     (let ((--link-org-files-as-html-maybe
;;            (function
;;             (lambda (raw-path info)
;;               ;; Treat links to `file.org' as links to `file.html', if
;;               ;; needed.  See `org-html-link-org-files-as-html'.
;;               (cond
;;                ((and org-html-link-org-files-as-html
;;                      (string= ".org"
;;                               (downcase (file-name-extension raw-path "."))))
;;                 (concat (file-name-sans-extension raw-path) "."
;;                         (plist-get info :html-extension)))
;;                (t raw-path)))))
;;           (type (org-element-property :type link)))
;;       (cond ((member type '("custom-id" "id"))
;;              (let ((destination (org-export-resolve-id-link link info)))
;;                (if (stringp destination)	; External file.
;;                    (let ((path (funcall --link-org-files-as-html-maybe
;;                                         destination info)))
;;                      (if (not contents) (format "<%s>" path)
;;                        (format "[%s](%s)" contents path)))
;;                  (concat
;;                   (and contents (concat contents " "))
;;                   (format "(%s)"
;;                           (format (org-export-translate "See section %s" :html info)
;;                                   (mapconcat 'number-to-string
;;                                              (org-export-get-headline-number
;;                                               destination info)
;;                                              ".")))))))
;;             ((org-export-inline-image-p link org-html-inline-image-rules)
;;              (let ((path (let ((raw-path (org-element-property :path link)))
;;                            (if (not (file-name-absolute-p raw-path)) raw-path
;;                              (expand-file-name raw-path)))))
;;                (format "[[file:%s|%s]]"(file-name-nondirectory path)
;;                        (let ((caption (org-export-get-caption
;;                                        (org-export-get-parent-element link))))
;;                          (when caption (org-export-data caption info))))))
;;             ((string= type "coderef")
;;              (let ((ref (org-element-property :path link)))
;;                (format (org-export-get-coderef-format ref contents)
;;                        (org-export-resolve-coderef ref info))))
;;             ((equal type "radio")
;;              (let ((destination (org-export-resolve-radio-link link info)))
;;                (org-export-data (org-element-contents destination) info)))
;;             ((equal type "fuzzy")
;;              (let ((destination (org-export-resolve-fuzzy-link link info)))
;;                (if (org-string-nw-p contents) contents
;;                  (when destination
;;                    (let ((number (org-export-get-ordinal destination info)))
;;                      (when number
;;                        (if (atom number) (number-to-string number)
;;                          (mapconcat 'number-to-string number "."))))))))
;;             (t (let* ((raw-path (org-element-property :path link))
;;                       (path (cond
;;                              ((member type '("http" "https" "ftp"))
;;                               (concat type ":" raw-path))
;;                              ((equal type "file")
;;                               ;; Treat links to ".org" files as ".html",
;;                               ;; if needed.
;;                               (setq raw-path
;;                                     (funcall --link-org-files-as-html-maybe
;;                                              raw-path info))
;;                               ;; If file path is absolute, prepend it
;;                               ;; with protocol component - "file://".
;;                               (if (not (file-name-absolute-p raw-path)) raw-path
;;                                 (concat "file://" (expand-file-name raw-path))))
;;                              (t raw-path))))
;;                  (if (not contents) (format "%s" path)
;;                    (format "[%s %s]" path (s-join " " (s-split "\n" contents))))))))

;;     )

;;   (advice-add 'org-mw-link :around #'yc/org-mw-link)

;;   (defun yc/org-html--anchor (func &rest args)
;;     "Advice for `org-html--anchor'.
;; Call FUNC with ARGS."
;;     (if org-mw-exporting
;;         ""
;;       (apply func args)))

;;   (advice-add 'org-html--anchor :around #'yc/org-html--anchor)

;;   (defun yc/org-mw-quote-block (quote-block contents info)
;;     "Transcode QUOTE-BLOCK element into Mediawiki format.
;; CONTENTS is the quote-block contents.  INFO is a plist used as
;; a communication channel."
;;     ;;(replace-regexp-in-string
;;     ;; "^" "> "
;;     (concat
;;      "<blockquote>"
;;      (replace-regexp-in-string "\n\\'" "" contents) "</blockquote>"))

;;   (advice-add 'org-mw-quote-block :around #'yc/org-mw-quote-block))



(defvar yc/org-html--embed-img nil "Nil.")
(use-package ox-html
  :defer t
  :init
  (progn
    (custom-set-variables
     '(org-html-inline-image-rules
       '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")
         ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")
         ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")))))

  (defun yc/org-html--format-image (func source attributes info)
    ""
    (if yc/org-html--embed-img
        (let* ((fn_ext_name (file-name-extension source))
               (fn_base_name (file-name-sans-extension (file-name-nondirectory source)))
               (b64_cmd (format "base64 %s | tr -d '\n'" source))
               (b64_content (shell-command-to-string b64_cmd)))

          (unless (file-exists-p source) (error "File %s does not exist!" source))
          (org-html-close-tag
           "img"
           (org-html--make-attribute-string
            (org-combine-plists
             (list :src (format "data:image/%s;base64, %s"fn_ext_name b64_content)
                   :alt (if (string-match-p "^ltxpng/" source)
                            (org-html-encode-plain-text
                             (org-find-text-property-in-string 'org-latex-src source))
                          (file-name-nondirectory source)))
             attributes))
           info))
      (funcall func source attributes info)))

  (advice-add 'org-html--format-image :around #'yc/org-html--format-image)

  ;; (defun yc/org-html-export-to-html (func &rest args)
  ;;   "Advice for `org-html-export-to-html', with yc/org-html--embed-img set to t."
  ;;   (let ((yc/org-html--embed-img t))
  ;;     (apply func args)))

  ;; (advice-add 'org-html-export-to-html :around #'yc/org-html-export-to-html)
  )


(defun yc/fetch-field (field)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp
           (format (rx bol "#+%s:" (group (+? nonl)) eol) field) nil t)
      (s-trim (match-string 1)))))

;; export org to mail..
(defun org-html-export-to-html-mail (&rest args)
  "Export current buffer to a HTML file, with `ARGS'.
Finally, title will be removed."
  (interactive)
  (let ((to (yc/fetch-field "EMAIL_TO"))
        (cc (yc/fetch-field "EMAIL_CC"))
        (title (yc/fetch-field "TITLE"))
        (filename (expand-file-name (apply 'org-html-export-to-html args))))

    (PDEBUG "FILENAME:" filename)
    (with-temp-file filename
      (insert-file-contents filename)
      (goto-char (point-min))
      (if (search-forward-regexp (rx bol "<h1" (+? anything) "/h1>" eol) nil t)
          (delete-region (match-beginning 0) (match-end 0))))

    (aif (or (executable-find "thunderbird-bin")
             (executable-find "thunderbird"))

        (let* (
               (cmd (format "%s -compose to='%s',cc='%s',subject='%s',message=%s"
                           it (or to "") (or cc "") title
                           filename)))

          (PDEBUG "CMD: " cmd)
          (shell-command cmd))

      (if (eq system-type 'darwin)
          (shell-command (format "open -a  Safari \"%s\"" filename))
          (browse-url-generic filename)))))


(defun yc/org-reload-projects ()
  "Reload all projects..."
  (interactive)

  (when (file-exists-p www-base-dir)

    (if (string-match-p "yangyingchao.github.io" www-base-dir)
        ;; special handling for my github page...
        (setq org-publish-project-alist
              `(
                ;; Org top level, to generate index.org
                ("github"
                 :base-directory ,org-dir
                 :base-extension "org"
                 :publishing-directory ,htdoc-dir
                 :recursive t
                 :publishing-function org-publish-jekyll
                 :section-numbers nil
                 :with-toc t
                 :makeindex nil
                 :auto-sitemap nil
                 :auto-preamble t
                 :sitemap-filename "index.org"
                 :html-preamble t)))

        (setq org-publish-project-alist
              `(
                ;; Org top level, to generate index.org
                ("org"
                 :base-directory ,org-dir
                 :base-extension "org"
                 :publishing-directory ,htdoc-dir
                 :recursive t
                 :publishing-function org-top-publish-to-html
                 :section-numbers nil
                 :with-toc t
                 :makeindex nil
                 :auto-sitemap t
                 :auto-preamble t
                 :html-head-extra ,my-html-head-extra-top
                 :html-link-home ,my-link-home
                 :sitemap-filename "index.org"
                 :html-preamble t)
                ;; Org top level, to export index.org
                ("org-index"
                 :base-directory ,org-dir
                 :base-extension "org"
                 :publishing-directory ,htdoc-dir
                 :recursive nil
                 :publishing-function org-html-publish-to-html
                 :section-numbers nil
                 :with-toc t
                 :makeindex nil
                 :auto-sitemap nil
                 :auto-preamble t
                 :html-head-extra ,my-html-head-extra-top
                 :html-link-home ,my-link-home
                 :sitemap-filename "index.org"
                 :html-preamble t)
                ("org-assets"
                 :base-directory ,(format "%s/assets" org-dir)
                 :publishing-directory ,(format "%s/assets" htdoc-dir)
                 :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf"))
                 :recursive t
                 :publishing-function org-publish-attachment)

                ;; references
                ("org-refs"
                 :base-directory ,(format "%s/references" org-dir)
                 :publishing-directory ,(format "%s/references" htdoc-dir)
                 ;; :base-extension ".*"
                 :recursive nil
                 :publishing-function org-compressed-publish-to-html)

                ;; ;; Slides.
                ;; ("org-slides"
                ;;  :base-directory ,(format "%s/slides" org-dir)
                ;;  :base-extension "org"
                ;;  :publishing-directory ,(format "%s/slides" htdoc-dir)
                ;;  :recursive t
                ;;  :publishing-function org-slides-publish-to-html
                ;;  :section-numbers nil
                ;;  :with-toc nil
                ;;  :makeindex nil
                ;;  :auto-sitemap t
                ;;  :auto-preamble t
                ;;  :sitemap-filename "index.org"
                ;;  :html-preamble t)

                ;; ("org-slides-static"
                ;;  :base-directory ,(format "%s/slides" org-dir)
                ;;  :publishing-directory ,(format "%s/slides" htdoc-dir)
                ;;  :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf" "html"))
                ;;  :recursive t
                ;;  :publishing-function org-publish-attachment)
                ))
        )


    (dolist (item (directory-files org-dir))
      (unless (or (not (file-directory-p (format "%s/%s" org-dir item)))
                  (member item '(".git" "slides" "images" "assets" "references" "." "..")))
        (add-to-project-list item t)))))

(yc/eval-after-load
  "ox-publish"
  (require 'ox-html)
  (require 'ox-odt)
  (require 'ox-md)
  (org-export-define-derived-backend
   'mail 'html
   ;; :export-block '("MW" "MEDIAWIKI")
   ;; :filters-alist '((:filter-parse-tree . org-mw-separate-elements))
   :menu-entry
   '(?h "Export to HTML"
        ((?m "As HTML buffer (for email)" org-html-export-to-html-mail))))

  (yc/org-reload-projects)

  (defun yc/org-publish-get-base-files (func &rest args)
    "Advice for `org-publish-get-base-files'.
Call FUNC with ARGS."
    (remove-if (lambda (x)
                 (string-match-p "sitemap" x))
               (apply func args)))

  (advice-add 'org-publish-get-base-files :around
              #'yc/org-publish-get-base-files))

(defun yc/org-md-template-adv (func contents info)
  "Advice for 'org-md-template'.
Call FUNC which is 'org-md-template with ARGS. Adding GEN-TITLE as mark of title."
  (aif (plist-get info :title)
      (concat "<!-- GEN-TITLE: " (org-export-data it info) " -->\n" contents)
    contents))
(advice-add 'org-md-template :around #'yc/org-md-template-adv)

(yc/eval-after-load
  "ox-odt"
  ;; org v8 bundled with Emacs 24.4
  (setq org-odt-preferred-output-format "doc")

  (let ((soffice   (aif (executable-find "soffice")
                       it
                     (yc/file-exists-p
                      "/Applications/LibreOffice.app/Contents/MacOS/soffice"))))
    (when soffice
      (setq org-odt-convert-processes `(("LibreOffice"
                                         ,(concat  soffice " --headless --convert-to %f%x --outdir %d %i" )))))
    ))


(defun org-publish-get-projects-from-filename (filename &optional up)
  "Return the project that FILENAME belongs to.
A separate static project is returned to."
  (let* ((filename (expand-file-name filename))
         project-name)

    (catch 'p-found
      (dolist (prj org-publish-project-alist)
        (unless (plist-get (cdr prj) :components)
          ;; [[info:org:Selecting%20files]] shows how this is supposed to work:
          (let* ((r (plist-get (cdr prj) :recursive))
                 (b (expand-file-name (file-name-as-directory
                                       (plist-get (cdr prj) :base-directory))))
                 (x (or (plist-get (cdr prj) :base-extension) "org"))
                 (e (plist-get (cdr prj) :exclude))
                 (i (plist-get (cdr prj) :include))
                 (xm (concat "^" b (if r ".+" "[^/]+") "\\.\\(" x "\\)$")))
            (when
                (or (and i
                         (member filename
                                 (mapcar (lambda (file)
                                           (expand-file-name file b))
                                         i)))
                    (and (not (and e (string-match e filename)))
                         (string-match xm filename)))
              (setq project-name (car prj))
              (throw 'p-found project-name))))))
    (when up
      (dolist (prj org-publish-project-alist)
        (if (member project-name (plist-get (cdr prj) :components))
            (setq project-name (car prj)))))
    (list (assoc project-name org-publish-project-alist)
          (assoc (concat project-name "-static") org-publish-project-alist))))

(defun yc/org-publish-current-project (func &optional force async)
  "Advice for `org-publish-current-project'.
Call FUNC with ARGS."
  (interactive "P")
  (save-excursion
    (dolist (project  (org-publish-get-projects-from-filename (buffer-file-name) 'up))
      (when project
        (message "Exporting project: %s" (car project))
        (org-publish project force async)))))

(advice-add 'org-publish-current-project :around #'yc/org-publish-current-project)


;; ox-reveal
(require 'ox-reveal)

(defun yc/org-reveal-export-to-html-adv (func &rest args)
  "Advice for 'org-reveal-export-to-html'.
Call FUNC which is 'org-reveal-export-to-html with ARGS.

Check js/slides.js exist or not, if not exist, re-fetch resource."

  ;; check required resources...
  (let* ((version "3.8.0")
         (package-name "reveal.js")
         (package-name-with-version (format "%s-%s" package-name version))
         (package-full-path (format "%s/%s.tar.gz" yc/emacs-tools-dir
                                    package-name-with-version))
         (final-target-dir (concat default-directory package-name))
         (final-target-file (concat final-target-dir "/js/reveal.js"))

         (basename (format "reveal.js-%s.tar.gz" version))
         (package (concat yc/emacs-tools-dir basename))
         (targetdir (concat default-directory "reveal.js"))
         (targetbase (file-name-directory targetdir)))


    (when (or (not (file-exists-p final-target-file)) ;; target does not exists
              (with-temp-buffer
                (insert-file final-target-file)
                (goto-char (point-min))

                (if (search-forward-regexp
                     (rx "var" (+ space) "VERSION" (+ space) "=" (+ space)
                         "'" (group (+ (or digit "."))) "'" (* space) ";") nil t)
                    (let ((v2 (match-string 1)))
                      (PDEBUG "source version" v2)
                      (PDEBUG "target version" version)
                      (not (string= version v2)))
                  t)))

      ;; clear existing files before further operation.
      (let* ((cmd (format "rm -rf %s; rm -rf %s; tar xzf %s && mv %s %s"
                          package-name-with-version final-target-dir
                          package-full-path package-name-with-version final-target-dir)))
        (PDEBUG "COMMAND: " cmd)
        (shell-command cmd))))

  ;; backup existing file....

  ;; do export

  ;; rename to "FILE-slide.html"

  ;; revert backuped file (if exists...)

  )

(advice-add 'org-reveal-export-to-html :around #'yc/org-reveal-export-to-html-adv)

;;; markdown, adding language tag if possible...
(defun yc/org-md-example-block-adv (f b c i)
  "Advice for 'org-md-example-block'.
Call FUNC which is 'org-md-example-block with ARGS."

  (if (and (string= (symbol-name (car b)) "src-block")
           (plist-get (cadr b)  :language))
      (concat "```"
              (plist-get (cadr b)  :language) "\n"
              (org-remove-indentation
               (org-export-format-code-default b i))
              "\n```")
    (funcall f b c i)))

(advice-add 'org-md-example-block :around #'yc/org-md-example-block-adv)

(provide 'ox-plus)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ox-plus.el ends here
