;;; fontawesome.el --- fontawesome utility

;;; Commentary:

;; fontawesome.el provides fontawesome(https://fortawesome.github.io/Font-Awesome/) utilities.

;;; Code:

(require 'cl-lib)
(declare-function ivy-read "ivy")
(defconst fa/github-url-template
  "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/%s/metadata/icons.json"
  "URL of fontawesome."
  )


(defun fontawesome--propertize (glyph)
  (propertize glyph
              'face '(:family "FontAwesome" :height 1.5)))

(defun fa/get-installed-version ()
  "Return version of FontAwesome."
  (cond
   ((executable-find "eix")
    (s-trim (shell-command-to-string
             "eix fontawesome | awk '/Installed versions/{$1=$2=\"\";print $3}' | awk -F \"(\" '{print $1}'")))
   (t (error "TODO: find version of fontawesome"))))

(defun fa/get-data-get-from-cache (path)
  "Get data from cache.
Returns (VERSION . DATA-ALIST)"
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(autoload 'http/get-body "yc-utils.el")

(defun fa/get-data-from-github (version)
  "Get data from github..."
  (let* ((version "5.10.2")
         (url (format fa/github-url-template version))
         (j-hash (json-parse-string (http/get-body url)
                                  :object-type 'hash-table
                                  :null-object nil
                                  :false-object nil))
         data)

    (maphash (lambda (k v)
               (let ((c (gethash "unicode" v)))
                 (PDEBUG "KEY: " k
                   "CODE: " c)
                 (push (cons k c) data)))
             j-hash)

    (cons version (nreverse data)))
  )

(defun fa/put-data-to-cache (cache path)
  "Write data into cache."
  (with-temp-file path
    (insert "(" (car cache))

    (mapc (lambda (x)
            (insert (format "(\"%s\" . \"\\x%s\")" (car x) (cdr x))))
          (cdr cache))
    (insert ")")))

(defun fa/get-fontawesome-alist ()
  "Get fontawesome list."
  (let* ((path (yc/make-cache-path "fontawesome-data.dat"))
         (cur-version (fa/get-installed-version))
         (cache (fa/get-data-get-from-cache path)))

    (when (or (not cache)
              (not (string= (car cache) cur-version)))
      (aif (fa/get-data-from-github cur-version)
          (progn
            (fa/put-data-to-cache it path)
            (setq cache (fa/get-data-get-from-cache path)))
        (error "Failed to get data from github")))

    (cdr cache)))

(defun fontawesome--construct-candidates ()
  (mapcar (lambda (fontawesome)
            (cons (concat (car fontawesome)
                          " -> "
                          (fontawesome--propertize
                           (cdr fontawesome)))
                  (cdr fontawesome)))
          (fa/get-fontawesome-alist)))

;;;###autoload
(defun counsel-fontawesome ()
  (interactive)
  (require 'ivy)
  (ivy-read "Font awesome> " (fontawesome--construct-candidates)
            :action (lambda (font)
                      (insert (cdr font)))))

(provide 'fontawesome)

;;; fontawesome.el ends here
