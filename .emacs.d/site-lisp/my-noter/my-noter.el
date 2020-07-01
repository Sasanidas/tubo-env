;;; my-noter.el --- A synchronized, Org-mode, document annotator -*- lexical-binding: t; -*-

;; Copyright (C) 2020, yyc

;;; Commentary:
;;
;; Inspired by interleave & org-noter.

;;; Code:

 ;; required packages.
(require '02-functions)

(require 'org)
(require 'org-element)

(require 'cl-lib)
(require 'doc-view)
(require 'image-mode)

(require 'org-roam)

(autoload 'ffap-url-p "ffap" ""  nil)
(autoload 'eww-current-url "eww" "get current url."  nil)
(autoload 'dired-get-marked-files "dired" ""  nil)


(declare-function doc-view-goto-page "doc-view")
(declare-function image-display-size "image-mode")
(declare-function image-get-display-property "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function image-scroll-up "image-mode")
(declare-function nov-render-document "ext:nov")
(declare-function org-attach-dir "org-attach")
(declare-function org-attach-file-list "org-attach")
(declare-function pdf-info-getannots "ext:pdf-info")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-outline "ext:pdf-info")
(declare-function pdf-info-pagelinks "ext:pdf-info")
(declare-function pdf-util-tooltip-arrow "ext:pdf-util")
(declare-function pdf-view-active-region "ext:pdf-view")
(declare-function pdf-view-active-region-p "ext:pdf-view")
(declare-function pdf-view-active-region-text "ext:pdf-view")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")
(defvar nov-documents-index)
(defvar nov-file-name)

 ;; user variables.
(defgroup my-noter nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom my-noter/root-directory   (expand-file-name "~/Documents/Database")
  "Root directory of my database."
  :type 'String
  :group 'my-noter)


(defcustom my-noter-property-doc-file "NOTER_DOCUMENT"
  "Name of the property that specifies the document."
  :group 'my-noter
  :type 'string)

(defcustom my-noter-property-note-location "NOTER_PAGE"
  "Name of the property that specifies the location of the current note.
The default value is still NOTER_PAGE for backwards compatibility."
  :group 'my-noter
  :type 'string)


(defcustom my-noter-doc-split-fraction '(0.6 . 0.4)
  "Fraction of the frame that the document window will occupy when split.
This is a cons of the type (HORIZONTAL-FRACTION . VERTICAL-FRACTION)."
  :group 'my-noter
  :type '(cons (number :tag "Horizontal fraction") (number :tag "Vertical fraction")))

(defcustom my-noter-auto-save-last-location nil
  "When non-nil, save the last visited location automatically; when starting a new session, go to that location."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-hide-other nil
  "When non-nil, hide all headings not related to the command used.
For example, when scrolling to pages with notes, collapse all the
notes that are not annotating the current page."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-always-create-frame t
  "When non-nil, my-noter will always create a new frame for the session.
When nil, it will use the selected frame if it does not belong to any other session."
  :group 'my-noter
  :type 'boolean)


(defcustom my-noter-insert-selected-text-inside-note t
  "When non-nil, it will automatically append the selected text into an existing note."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-closest-tipping-point 0.3
  "Defines when to show the closest previous note.

Let x be (this value)*100. The following schematic represents the
view (eg. a page of a PDF):

+----+
|    | -> If there are notes in here, the closest previous note is not shown
+----+--> Tipping point, at x% of the view
|    | -> When _all_ notes are in here, below the tipping point, the closest
|    |    previous note will be shown.
+----+

When this value is negative, disable this feature.

This setting may be overridden in a document with the function
`my-noter-set-closest-tipping-point', which see."
  :group 'my-noter
  :type 'number)

(defcustom my-noter-default-notes-file-names '("Notes.org")
  "List of possible names for the default notes file, in increasing order of priority."
  :group 'my-noter
  :type 'string)

(defcustom my-noter-notes-search-path "~/Documents/Database/org/"
  "List of paths to check (non recursively) when searching for a notes file."
  :group 'my-noter
  :type 'string)

 ;; Functions
(defalias 'my-noter/find-file 'org-roam-find-file)

(defun my-noter/do-dispatch-file (item)
  "Dispatch single ITEM."
  (interactive)
  (unless (file-exists-p item)
    (error "File: %s not accessible" item))

  (let* ((checksum (shell-command-to-string (format "md5sum '%s'" item)))
         (target-dir (concat my-noter/root-directory "/"
                             (downcase (file-name-extension item)) "/"
                             (substring checksum 0 1)))
         (note-file (my-noter/get-note-file item))
         (target-file (expand-file-name (file-name-nondirectory item) target-dir))
         )

    (unless (directory-name-p target-dir)
      (make-directory target-dir t))

    (PDEBUG
      "FILE: " item
      "\nCHECKSUM: " checksum
      "\nNOTE:" note-file)

    (shell-command (format "mv \"%s\" %s/" item target-dir))

    (if (file-exists-p note-file)
        (with-temp-file note-file
          (insert-file-contents note-file)
          (save-excursion
            (goto-char (point-min))
            (when (search-forward-regexp
                 (rx bol "#+NOTER_DOCUMENT: " (group (+? nonl)) eol) nil t)
                (replace-match (concat "#+NOTER_DOCUMENT: " target-file))))

          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp
                    (format (rx bol ":NOTER_DOCUMENT:" (+ space) (group (+? nonl) "%s") eol)
                            (file-name-nondirectory target-file))
                     nil t)
              (replace-match (concat ":NOTER_DOCUMENT: " target-file)))))
      (aif (get-buffer (file-name-nondirectory note-file))
          (with-temp-buffer it
                            (save-excursion
                              (find-file (buffer-file-name))))))

    (message "%s --> %s" item target-dir)))

(defun my-noter/dispatch-file ()
  "Dispatch single ITEM."
  (interactive)
  (cond
   (buffer-file-name

    (my-noter/do-dispatch-file buffer-file-name)
    (kill-buffer))

   ((equal major-mode 'dired-mode)
    (mapc 'my-noter/do-dispatch-file (dired-get-marked-files))
    (revert-buffer))

   (t (error "Not handled: %S" major-mode))))


(defun my-noter/dispatch-directory (&optional directory)
  "Dispatch all files in DIRECTORY."
  (interactive)
  (let ((directory (or directory default-directory)))
    (unless (file-directory-p directory)
      (error "Directory %s not accessible" directory))

    (mapc 'my-noter/do-dispatch-file (directory-files-recursively directory ".*"))))

(defun my-noter/get-note-file (input)
  "Return note file for INPUT."
  (let ((pdf-file-name input)
        (org-file-create-dir (expand-file-name "org" my-noter/root-directory)))

    (unless (file-directory-p org-file-create-dir)
      (make-directory org-file-create-dir t))

    (expand-file-name (concat (file-name-base pdf-file-name) ".org")
                      org-file-create-dir)))


(defun my-noter/doc-approx-location ()
  "Return current location."
  (cond
   ((memq major-mode '(doc-view-mode pdf-view-mode))
    (image-mode-window-get 'page))

   ((eq major-mode 'nov-mode)
    nov-documents-index
    )

   (t (point))))

(defun my-noter/check-if-document-is-annotated-on-file (document-path notes-path)
  "Check if doc file (DOCUMENT-PATH) is annotated on note file (NOTES-PATH)."
  (let ((buffer (find-buffer-visiting notes-path)))
    (when buffer (with-current-buffer buffer (save-buffer)))

    (with-temp-buffer
      (insert-file-contents notes-path)
      (catch 'break
        (while (re-search-forward (org-re-property my-noter-property-doc-file) nil t)
          (when (file-equal-p (expand-file-name (match-string 3) (file-name-directory notes-path))
                              document-path)
            ;; NOTE(nox): This notes file has the document we want!
            (throw 'break t)))))))

(defun my-noter-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (derived-mode-p 'doc-view-mode)
    (doc-view-kill-proc))
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (kill-buffer (current-buffer))))

(defvar my-noter-org-buffer nil
  "Org notes buffer name.")

(defvar my-noter-doc-buffer nil
  "Name of PDF buffer associated with `my-noter-org-buffer'.")


;;;###autoload
(defvar my-noter-pdf-previous-page-fn #'doc-view-previous-page
  "Function to call to display the previous page.")

;;;###autoload
(defun my-noter-goto-doc-page (page)
  "Goto PAGE of document.."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)  (pdf-view-goto-page page))
   ((eq major-mode 'doc-view-mode)  (doc-view-goto-page page))

   (t
    (when (>= (point-max) page)
      (forward-char (- page (point)))
      (recenter)))))


;;;###autoload

(defvar my-noter-pdf-scroll-up-or-next-page-fn #'doc-view-scroll-up-or-next-page
  "Function to call for line/page scrolling in upward direction." )

;;;###autoload

(defvar my-noter-pdf-scroll-down-or-previous-page-fn #'doc-view-scroll-down-or-previous-page
  "Function to call for line/page scrolling in downward direction.")

(defcustom my-noter-sort-order 'asc
  "Specifiy the notes' sort order in the notes buffer.

The possible values are 'asc for ascending and 'desc for descending."
  :type '(choice (const  asc)
                 (const  desc))
  :group 'my-noter)

(defcustom my-noter-split-direction 'vertical
  "Specify how to split the notes buffer."
  :group 'my-noter
  :type '(choice (const vertical)
                 (const horizontal)))

(defcustom my-noter-split-lines nil
  "Specify the number of lines the PDF buffer should be increased or decreased.

If nil both buffers are split equally.  If the number is positive,
the window is enlarged.  If the number is negative, the window is
shrunken.

If `my-noter-split-direction' is 'vertical then the number is
taken as columns."
  :group 'my-noter
  :type '(choice integer
                 (const nil)))

(defcustom my-noter-disable-narrowing nil
  "Disable narrowing in notes/org buffer."
  :group 'my-noter
  :type 'boolean)

;;; suppress "functions are not known to be defined" warnings
(declare-function pdf-view-next-page "pdf-view.el")
(declare-function pdf-view-previous-page "pdf-view.el")
(declare-function pdf-view-goto-page "pdf-view.el")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view.el")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view.el")

(make-variable-buffer-local
 (defvar my-noter-page-marker 0
   "Caches the current page while scrolling"))

(defun my-noter--find-pdf-path (buffer)
  "Search the `my-noter_pdf' property in BUFFER and extracts it when found."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+noter_document: \\(.*\\)" nil :noerror)
          (match-string 1))))))

(defun my-noter--headline-doc-path (buffer)
  "Return the NOTER_DOCUMENT property of the current headline in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((headline (org-element-at-point)))
        (PDEBUG
         "CONTENT: " headline
         "HEADLINE: " (org-element-type headline)
         "FF:" (org-entry-get nil my-noter-property-doc-file t))

        (org-entry-get nil my-noter-property-doc-file t)))))

(defun my-noter--open-file (split-window)
  "Opens the pdf file in besides the notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let* ((buf (current-buffer))
         (doc-file-name
          (or (my-noter--headline-doc-path buf)
              (my-noter--find-pdf-path buf))))
    (PDEBUG "CURRNET-BUFF:" buf
            "POINT: " (point)
            "HEADLINE-PDF: " (my-noter--headline-doc-path buf)
            "FIND-PDF:" (my-noter--find-pdf-path buf)
            )
    (unless doc-file-name
      (setq doc-file-name
            (read-file-name "No NOTER_DOCUMENT property found. Please specify path: " nil nil t))

      ;; Check whether we have any entry at point with `org-entry-properties' before
      ;; prompting if the user wants multi-pdf.
      (if (and (org-entry-properties) (y-or-n-p "Is this multi-pdf? "))
          (org-entry-put (point) "NOTER_DOCUMENT" doc-file-name)
        (save-excursion
          (goto-char (point-min))
          (insert "#+NOTER_DOCUMENT: " doc-file-name))))

    (delete-other-windows)
    (funcall split-window)
    (when (integerp my-noter-split-lines)
      (if (eql my-noter-split-direction 'horizontal)
          (enlarge-window my-noter-split-lines)
        (enlarge-window-horizontally my-noter-split-lines)))

    (aif (ffap-url-p doc-file-name)
        (eww it)
        (find-file (expand-file-name doc-file-name)))


    (my-noter-doc-mode 1)
    doc-file-name))

(defun my-noter--goto-parent-headline (property)
  "Traverse the tree until the parent headline.

Consider a headline with property PROPERTY as parent headline."
  (catch 'done
    (if (and (eql (org-element-type (org-element-at-point)) 'headline)
             (org-entry-get (point) property))
        (org-element-at-point)
      (condition-case nil
          (org-up-element)
        ('error
         (throw 'done nil)))
      (my-noter--goto-parent-headline property))))

(defun my-noter--goto-search-position ()
  "Move point to the search start position.

For multi-pdf notes this is the outermost parent headline.  For everything else
this is the beginning of the buffer."
  (my-noter--goto-parent-headline my-noter-property-doc-file)
  )

(defun my-noter--narrow-to-subtree (&optional force)
  "Narrow buffer to the current subtree.

If `my-noter-disable-narrowing' is non-nil this
function does nothing.

When FORCE is non-nil `my-noter-disable-narrowing' is
ignored."
  (when (and (not (org-before-first-heading-p))
             (or (not my-noter-disable-narrowing)
                 force))
    (org-narrow-to-subtree)))

(defun my-noter--go-to-page-note (page)
  "Look up the notes for the current pdf PAGE.

Effectively resolves the headline with the my-noter_page_note
property set to PAGE and returns the point.

If `my-noter-disable-narrowing' is non-nil then the buffer gets
re-centered to the page heading.

It (possibly) narrows the subtree when found."
  (PDEBUG "PAGE:" page)
  (with-current-buffer my-noter-org-buffer
    (let (point
          (window (get-buffer-window (current-buffer) 'visible)))
      (save-excursion
        (widen)
        (my-noter--goto-search-position)
        ;; (when my-noter-multi-pdf-notes-file
        ;;   ;; only search the current subtree for notes. See. Issue #16
        ;;   (my-noter--narrow-to-subtree t))
        (when (re-search-forward (format "^\[ \t\r\]*\:noter_page\: %s$"
                                         page)
                                 nil t)
          ;; widen the buffer again for the case it is narrowed from
          ;; multi-pdf notes search. Kinda ugly I know. Maybe a macro helps?
          (widen)
          (org-back-to-heading t)
          (my-noter--narrow-to-subtree)
          (org-show-subtree)
          (org-cycle-hide-drawers t)
          (setq point (point))))
      ;; When narrowing is disabled, and the notes/org buffer is
      ;; visible recenter to the current headline. So even if not
      ;; narrowed the notes buffer scrolls allong with the PDF.
      (when (and my-noter-disable-narrowing point window)
        (with-selected-window window
          (goto-char point)
          (recenter)))
      point)))

(defun my-noter/doc-next-page ()
  "Go to next page of document."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-next-page))
   ((eq major-mode 'doc-view-mode)
    (doc-view-next-page))
   (t
    (error "Error: next-page is not implemented for mode %s"
           (symbol-name major-mode)))))

(defun my-noter/doc-previous-page ()
  "Go to next page of document."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-previous-page))
   ((eq major-mode 'doc-view-mode)
    (doc-view-previous-page))
   (t
    (error "Error: previous-page is not implemented for mode %s"
           (symbol-name major-mode)))))

(defun my-noter-go-to-next-page ()
  "Go to the next page in PDF.  Look up for available notes."
  (interactive)
  (my-noter/doc-next-page)
  (my-noter--go-to-page-note (my-noter/doc-approx-location)))

(defun my-noter-go-to-previous-page ()
  "Go to the previous page in PDF.  Look up for available notes."
  (interactive)
  (my-noter/doc-previous-page)
  (my-noter--go-to-page-note (my-noter/doc-approx-location)))

(defun my-noter-scroll-up ()
  "Scroll up the PDF.  Look up for available notes."
  (interactive)
  (setq my-noter-page-marker (my-noter/doc-approx-location))
  (funcall my-noter-pdf-scroll-up-or-next-page-fn)
  (unless (= my-noter-page-marker (my-noter/doc-approx-location))
    (my-noter--go-to-page-note (my-noter/doc-approx-location))))

(defun my-noter-scroll-down ()
  "Scroll down the PDF.  Look up for available notes."
  (interactive)
  (setq my-noter-page-marker (my-noter/doc-approx-location))
  (funcall my-noter-pdf-scroll-down-or-previous-page-fn)
  (unless (= my-noter-page-marker (my-noter/doc-approx-location))
    (my-noter--go-to-page-note (my-noter/doc-approx-location))))

(defun my-noter--switch-to-org-buffer (&optional insert-newline-maybe position)
  "Switch to the notes buffer.

Inserts a newline into the notes buffer if INSERT-NEWLINE-MAYBE
is non-nil.
If POSITION is non-nil move point to it."

  (PDEBUG "CURRENT-BUFF:" (current-buffer)
          "EQ" (eq (buffer-name) my-noter-org-buffer))
  (if (eq (buffer-name) my-noter-org-buffer)
      (switch-to-buffer my-noter-org-buffer)
    (switch-to-buffer-other-window my-noter-org-buffer)
    )

  (when (integerp position)
    (goto-char position))
  (when insert-newline-maybe
    (save-restriction
      (when my-noter-disable-narrowing
        (my-noter--narrow-to-subtree t))
      (my-noter--goto-insert-position))
    ;; Expand again. Sometimes the new content is outside the narrowed
    ;; region.
    (org-show-subtree)
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *" (line-beginning-position)))
      (org-return))
    ))

(defun my-noter--switch-to-doc-buffer ()
  "Switch to the pdf buffer."
  (switch-to-buffer-other-window my-noter-doc-buffer)
  ;; (if (derived-mode-p 'org-mode)
  ;;     (switch-to-buffer-other-window my-noter-doc-buffer)
  ;;   (switch-to-buffer my-noter-doc-buffer))
  )

(defun my-noter--goto-insert-position ()
  "Move the point to the right insert postion.

For multi-pdf notes this is the end of the subtree.  For everything else
this is the end of the buffer"
  (prog1
        (my-noter--goto-parent-headline my-noter-property-doc-file)
      (org-end-of-subtree)))

(defun my-noter--create-new-note (document page &optional selected-text)
  "Create a new headline for the page PAGE of DOCUMENT."
  (PDEBUG "ENTER: page: " page)
  (unless (or (ffap-url-p document)
              (and document
         (file-exists-p document)))
    (warn "Document not specified..."))

  (let ((title (completing-read "Note: " nil nil nil nil nil
                                (or selected-text (format "Notes for page %d" page))))
        new-note-position)

    (with-current-buffer my-noter-org-buffer
      (widen)

      (condition-case var
          (progn
            (outline-show-entry)
            )
        (error (message "%s" var)))

      (org-insert-heading '(4))
      (insert (org-trim (replace-regexp-in-string "\n" " " title)))
      (org-end-of-subtree)
      (org-N-empty-lines-before-current 1)

      (org-entry-put nil my-noter-property-doc-file
                     (or document
                         (my-noter--headline-doc-path (current-buffer))
                         (my-noter--find-pdf-path (current-buffer))))
      (org-entry-put nil my-noter-property-note-location (number-to-string page))


      (org-N-empty-lines-before-current 2)

      (org-cycle-hide-drawers t)
      (setq new-note-position (point)))

    (my-noter--switch-to-org-buffer t new-note-position)))

(defun my-noter-add-note (&optional precise-info)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer.  When the input is empty, a default title
will be generated.

If there are other notes related to the current location, the
prompt will also suggest them.  Depending on the value of the
variable `my-noter-closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `my-noter-insert-precise-note' for more
info)."
  (interactive)
  (PDEBUG "INFO: " precise-info)
  (let* ((page (my-noter/doc-approx-location))
         ;; (position (my-noter--go-to-page-note page))
         (selected-text
          (cond
           ((eq major-mode 'pdf-view-mode)
            (when (pdf-view-active-region-p)
              (mapconcat 'identity (pdf-view-active-region-text) ? )))

           (t
            (when (region-active-p)
              (buffer-substring-no-properties (mark) (point))))))

         (document-path (my-noter/get-document-path)))

    (if (region-active-p)
        (deactivate-mark))

    (PDEBUG "TEXT:" selected-text)
    (PDEBUG "DOCUMENT-PATH:" document-path)

    (if current-prefix-arg
        ;; when current prefix-arg is set, copy page info only..
        (progn
        (kill-new (format ":%s: %s\n:%s: %d\n"
                          my-noter-property-doc-file document-path
                          my-noter-property-note-location page))
        (message "note info killed."))
        (my-noter--create-new-note document-path page selected-text))))

(define-obsolete-function-alias
  'my-noter--sync-pdf-page-current 'my-noter-sync-page-current "1.3.0")

(defun my-noter/eww-after-render ()
  "Function to run after eww is rendered."
  (remove-hook 'eww-after-render-hook 'my-noter/eww-after-render)
  (my-noter-sync-page-current))

(defun my-noter-sync-page-current ()
  "Open page for currently visible notes."
  (interactive)
  (my-noter--switch-to-org-buffer)
  (let ((page (string-to-number
               (org-entry-get-with-inheritance my-noter-property-note-location)))
        (doc-path (my-noter--headline-doc-path (current-buffer))))
    (when (and (integerp page)
               (> page 0)) ; The page number needs to be a positive integer
      (my-noter--narrow-to-subtree)
      (my-noter--switch-to-doc-buffer)

      (PDEBUG "CURNET-BUFF:" (current-buffer)
              "DOC-PATH: " doc-path
              "CURRENT-URL: " (eww-current-url)
              )
      ;; check if we need to update doc content...
      (cond
       ((ffap-url-p doc-path)
        (unless (string-equal doc-path (eww-current-url))
          (add-hook 'eww-after-render-hook 'my-noter/eww-after-render)
          (eww doc-path)
          )
        )
       )
      (my-noter-goto-doc-page page))))

(defun my-noter-sync-pdf-page-previous ()
  "Move to the previous set of notes.

This show the previous notes and synchronizes the PDF to the right page number."
  (interactive)
  (my-noter--switch-to-org-buffer)
  (widen)
  (my-noter--goto-parent-headline my-noter-property-note-location)
  (org-backward-heading-same-level 1)
  (my-noter--narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (let ((page (string-to-number
                   (org-entry-get-with-inheritance my-noter-property-note-location))))
    (when (and (integerp page)
               (> page 0)) ; The page number needs to be a positive integer

      (my-noter--switch-to-doc-buffer)
      (my-noter-goto-doc-page page))))

(defun my-noter-sync-doc-page-next ()
  "Move to the next set of notes.

This shows the next notes and synchronizes the PDF to the right page number."
  (interactive)
  (my-noter--switch-to-org-buffer)
  (widen)
  ;; go to the first notes heading if we're not at an headline or if
  ;; we're on multi-pdf heading. This is useful to quickly jump to the
  ;; notes if they start at page 96 or so. Image you need to skip page
  ;; for page.
  (if (my-noter--goto-parent-headline my-noter-property-note-location)
      (org-forward-heading-same-level 1)

    (org-show-subtree)

    (outline-next-visible-heading 1))
  (my-noter--narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (let ((pdf-page (string-to-number
                   (org-entry-get (point) my-noter-property-note-location))))
    (when (and (integerp pdf-page)
               (> pdf-page 0)) ; The page number needs to be a positive integer
      (my-noter--switch-to-doc-buffer)
      (my-noter-goto-doc-page pdf-page))))

(defun my-noter-quit ()
  "Quit my-noter mode."
  (interactive)
  (with-current-buffer my-noter-org-buffer
    (widen)
    (my-noter--goto-search-position)
    (when (my-noter--headlines-available-p)
      (my-noter--sort-notes my-noter-sort-order)
      (org-overview))
    (my-noter-notes-mode 0))
  (my-noter-kill-proc-and-buffer))

(defun my-noter--headlines-available-p ()
  "True if there are headings in the notes buffer."
  (save-excursion
    (re-search-forward "^\* .*" nil t)))

(defun my-noter--sort-notes (sort-order)
  "Sort notes by my-noter_page_property.

SORT-ORDER is either 'asc or 'desc."
  (condition-case nil
      (org-sort-entries nil ?f
                        (lambda ()
                          (let ((page-note (org-entry-get nil "my-noter_page_note")))
                            (if page-note
                                (string-to-number page-note)
                              -1)))
                        (if (eq sort-order 'asc)
                            #'<
                          #'>))
    ('user-error nil)))

(defun my-noter--select-split-function ()
  "Determine which split function to use.

This returns either `split-window-below' or `split-window-right'
based on a combination of `current-prefix-arg' and
`my-noter-split-direction'."
  (let ((split-plist (list 'vertical #'split-window-right
                           'horizontal #'split-window-below))
        (current-split my-noter-split-direction))
    (plist-get split-plist
               (if current-prefix-arg
                   (if (eql current-split 'vertical)
                       'horizontal
                     'vertical)
                 current-split))))

;;; my-noter
;; Minor mode for the org file buffer containing notes

(defvar my-noter-notes-mode-map (make-sparse-keymap)
  "Keymap while command `my-noter-mode' is active in the org file buffer.")

;;;###autoload
(define-minor-mode my-noter-notes-mode
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'my-noterd' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+NOTER_DOCUMENT: /the/path/to/your/pdf.pdf
- Start `my-noter-mode' with `M-x my-noter-mode'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

The split direction is determined by the customizable variable
`my-noter-split-direction'. When `my-noter-mode' is invoked
with a prefix argument the inverse split direction is used
e.g. if `my-noter-split-direction' is 'vertical the buffer is
split horizontally.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{my-noter-doc-mode-map}

Keybindings (org-mode buffer):

\\{my-noter-map}"
  :lighter " ≡"
  :keymap  my-noter-notes-mode-map
  (if my-noter-notes-mode
      (condition-case nil
          (progn
            (setq my-noter-org-buffer (buffer-name))
            (my-noter--open-file (my-noter--select-split-function))
            ;; expand/show all headlines if narrowing is disabled
            (when my-noter-disable-narrowing
              (with-current-buffer my-noter-org-buffer
                (my-noter--goto-search-position)
                ;; (org-show-subtree)
                (org-cycle-hide-drawers 'all)))
            (my-noter--go-to-page-note 1)
            (message "my-noter enabled"))
        ('quit
         (my-noter-notes-mode -1)))
    ;; Disable the corresponding minor mode in the PDF file too.
    (when (and my-noter-doc-buffer
               (get-buffer my-noter-doc-buffer))
      (my-noter--switch-to-doc-buffer)
      (my-noter-doc-mode -1)
      (setq my-noter-doc-buffer nil))
    (setq my-noter-org-buffer nil)
    (message "my-noter mode disabled")))

(defun my-noter/get-document-path ()
  "Return path of document visited by this buffer."
  (interactive)
  (let ((document-path
         (cond
          ((eq major-mode 'nove-mode) nov-file-name)
          ((eq major-mode 'eww-mode) (eww-current-url))

          (t (expand-file-name (or buffer-file-name buffer-file-truename))))))

    (if (called-interactively-p 'interactive)
        (PDEBUG "Document path: " document-path))
    document-path))

(defun my-noter ()
  "Start `my-noter' session."
  (interactive)

  (unless org-roam-mode
    (org-roam-mode 1))

  (if (eq major-mode 'org-mode)
      ;; Creating session from notes file.
      (progn
        ;; (when (org-before-first-heading-p)
        ;;   ;; TODO: check #+NOTER_DOCUMENT:
        ;;   (error "`my-noter' must be issued inside a heading"))
        (my-noter-notes-mode))

    ;; Creating the session from the annotated document
    (let* ((document-path
            (or buffer-file-name
                (if (eq major-mode 'nov-mode)
                    (bound-and-true-p nov-file-name))
                (if (eq major-mode 'eww-mode)
                    (eww-current-url)
                  (error "This buffer does not seem to be visiting any file"))))

           (document-name (file-name-nondirectory document-path))
           (document-base (file-name-base document-name))
           (document-directory
            (if buffer-file-name
                (file-name-directory buffer-file-name)
              (if (and document-name  buffer-file-truename)
                  (if (file-equal-p document-name buffer-file-truename)
                      default-directory
                    (file-name-directory buffer-file-truename))
                default-directory)))

           (search-names (append my-noter-default-notes-file-names (list (concat document-base ".org"))))
           notes-files-annotating     ; List of files annotating document
           notes-files                ; List of found notes files (annotating or not)
           )

      ;; NOTE(nox): Check the search path
      (dolist (name search-names)
          (let ((file-name (expand-file-name name my-noter-notes-search-path)))
            (when (file-exists-p file-name)
              (push file-name notes-files)
              (when (my-noter/check-if-document-is-annotated-on-file
                     document-path file-name)
                (PDEBUG "Annotated file:" file-name)
                (push file-name notes-files-annotating)))))

      ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
      ;; and it will end up in the correct order
      (dolist (name search-names)
        (let ((directory (locate-dominating-file document-directory name))
              file)
          (when directory
            (setq file (expand-file-name name directory))
            (unless (member file notes-files) (push file notes-files))
            (when (my-noter/check-if-document-is-annotated-on-file document-path file)
              (push file notes-files-annotating)))))

      (setq search-names (nreverse search-names))

      (when (not notes-files-annotating)
        (let* ((file-name document-path)
               (choices (list
                         "  Create new note file."
                         "  Locate note file manually."))
               (action-index (cl-position
                              (completing-read
                               (format "Could not find note file %s, choose an action: "
                                       file-name)
                               choices
                               nil
                               t)
                              choices
                              :test 'equal)))

          (PDEBUG "INDEX: " action-index)

          (case action-index
            (0 (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                        search-names nil nil))
                      list-of-possible-targets
                      target)

                 ;; NOTE(nox): Create list of targets from current path
                 (catch 'break
                   (let ((current-directory document-directory)
                         file-name)
                     (PDEBUG "CUR-DIR:" current-directory)
                     (while current-directory
                       (setq file-name (expand-file-name notes-file-name current-directory))
                       (when (file-exists-p file-name)
                         (setq file-name (propertize file-name 'display
                                                     (concat file-name
                                                             (propertize " -- Exists!"
                                                                         'face '(foreground-color . "green")))))
                         (push file-name list-of-possible-targets)
                         (throw 'break nil))

                       (push file-name list-of-possible-targets)

                       (PDEBUG "KKKK: " current-directory)
                       (when (string= current-directory
                                      (setq current-directory
                                            (file-name-directory (directory-file-name current-directory))))
                         (throw 'break nil)))))

                 (setq list-of-possible-targets (nreverse list-of-possible-targets))

                 ;; NOTE(nox): Create list of targets from search path
                 (when (file-exists-p my-noter-notes-search-path)
                   (let ((file-name (expand-file-name notes-file-name (expand-file-name "org" my-noter/root-directory))))
                     (unless (member file-name list-of-possible-targets)
                       (when (file-exists-p file-name)
                         (setq file-name (propertize file-name 'display
                                                     (concat file-name
                                                             (propertize " -- Exists!"
                                                                         'face '(foreground-color . "green"))))))
                       (push file-name list-of-possible-targets))))

                 (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                               nil t))
                 (set-text-properties 0 (length target) nil target)
                 (unless (file-exists-p target) (write-region "" nil target))

                 (setq notes-files (list target))))
            (1
             (push (read-file-name "Select note file: "
                                   (expand-file-name my-noter-notes-search-path))
                   notes-files))
            (t nil)))

        (when (> (length notes-files) 1)
          (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                   notes-files nil t))))

        (if (member (car notes-files) notes-files-annotating)
            ;; NOTE(nox): This is needed in order to override with the arg
            (setq notes-files-annotating notes-files)
          (with-current-buffer (find-file-noselect (car notes-files))
            (goto-char (point-max))
            (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                    "* " document-base)
            (org-entry-put nil my-noter-property-doc-file document-path))
          (setq notes-files-annotating notes-files)))

      (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
        (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                            notes-files-annotating nil t))))

      (PDEBUG "Notes files: " notes-files-annotating)

      (find-file (car notes-files-annotating))
      (my-noter))))

;;; my-noter PDF Mode
;; Minor mode for the pdf file buffer associated with the notes


(defvar my-noter-doc-mode-map (make-sparse-keymap)
  "Keymap while command `my-noter-doc-mode' is active in the pdf file buffer.")


;;; Key-bindings

(define-key my-noter-notes-mode-map (kbd "M-.") #'my-noter-sync-page-current)
(define-key my-noter-notes-mode-map (kbd "M-p") #'my-noter-sync-pdf-page-previous)
(define-key my-noter-notes-mode-map (kbd "M-n") #'my-noter-sync-doc-page-next)

(define-key my-noter-doc-mode-map (kbd "n")     #'my-noter-go-to-next-page)
(define-key my-noter-doc-mode-map (kbd "p")     #'my-noter-go-to-previous-page)
(define-key my-noter-doc-mode-map (kbd "SPC")   #'my-noter-scroll-up)
(define-key my-noter-doc-mode-map (kbd "S-SPC") #'my-noter-scroll-down)
(define-key my-noter-doc-mode-map (kbd "DEL")   #'my-noter-scroll-down)
(define-key my-noter-doc-mode-map (kbd "i")     #'my-noter-add-note)
(define-key my-noter-doc-mode-map (kbd "q")     #'my-noter-quit)
(define-key my-noter-doc-mode-map (kbd "M-.")   #'my-noter-sync-page-current)
(define-key my-noter-doc-mode-map (kbd "M-p")   #'my-noter-sync-pdf-page-previous)
(define-key my-noter-doc-mode-map (kbd "M-n")   #'my-noter-sync-doc-page-next)


;;;###autoload
(define-minor-mode my-noter-doc-mode
  "Minor mode for the document buffer.
Keymap:
\\{my-noter-doc-mode-map}"
  :lighter " ≡"
  :keymap  my-noter-doc-mode-map

  (when my-noter-doc-mode
    (setq my-noter-doc-buffer (buffer-name))))

(define-key doc-view-mode-map (kbd "i") #'my-noter)
(when (boundp 'pdf-view-mode-map)
  (define-key pdf-view-mode-map (kbd "i") #'my-noter))


(provide 'my-noter)

;;; my-noter.el ends here
