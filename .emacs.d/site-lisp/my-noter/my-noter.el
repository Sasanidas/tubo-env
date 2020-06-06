;;; my-noter.el --- A synchronized, Org-mode, document annotator       -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Gonçalo Santos

;; Author: Gonçalo Santos (aka. weirdNox@GitHub)
;; Homepage: https://github.com/weirdNox/my-noter
;; Keywords: lisp pdf my-noter annotate external sync notes documents org-mode
;; Package-Version: 20191020.1212
;; Package-Commit: 9ead81d42dd4dd5074782d239b2efddf9b8b7b3d
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6") (org "9.0"))
;; Version: 1.4.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea is to let you create notes that are kept in sync when you scroll through the
;; document, but that are external to it - the notes themselves live in an Org-mode file. As
;; such, this leverages the power of Org-mode (the notes may have outlines, latex fragments,
;; babel, etc...) while acting like notes that are made /in/ the document.

;; Also, I must thank Sebastian for the original idea and inspiration!
;; Link to the original My-Noter package:
;; https://github.com/rudolfochrist/my-noter

(require '02-functions)
(autoload 'ffap-url-p "ffap" ""  nil)

(defcustom my-noter/root-directory   (expand-file-name "~/Documents/Database")
  "Root directory of my database."
  :type 'String
  :group 'My-Noter)

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
            (if (search-forward-regexp
                 (rx bol "#+NOTER_DOCUMENT: " (group (+? nonl)) eol) nil t)
                (replace-match (concat "#+NOTER_DOCUMENT: " target-file))
              (warn "INTERLEAVE not found in file: %s" note-file)))

          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp
                    (format (rx bol ":NOTER_DOCUMENT: " (group (+? nonl) "%s") eol)
                            (file-name-nondirectory target-file))
                     nil t)
              (replace-match (concat ":NOTER_DOCUMENT: " target-file)))))
      (aif (get-buffer (file-name-nondirectory note-file))
          (with-temp-buffer it
                            (reload-file))))

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
        (org-file-create-dir (expand-file-name "000_notes" my-noter/root-directory)))

    (unless (file-directory-p org-file-create-dir)
      (make-directory org-file-create-dir t))

    (expand-file-name (concat (file-name-base pdf-file-name) ".org")
                      org-file-create-dir)))


(defun my-noter/open-note-file ()
  "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)."
  (interactive)

  (unless buffer-file-name
    (error "Current buffer not open for file"))

  (let* ((current-file buffer-file-name)
         (org-file-name (my-noter/get-note-file current-file)))
    ;; Open the notes org file and enable `interleave-mode'
    (unless (file-exists-p org-file-name)
      (with-temp-file org-file-name
        (insert-file-contents "~/.emacs.d/templates/auto-insert/insert.org")
        (auto-insert--org-mode (file-name-base org-file-name))
        (goto-char (point-max))
        (insert "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"assets/css/style.css\">\n")
        (insert "#+NOTER_DOCUMENT: " current-file "\n")))

    (find-file org-file-name)
    (my-noter-mode)))

;;; Code:
(require 'org)
(require 'org-element)
(require 'cl-lib)

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

;; --------------------------------------------------------------------------------
;; NOTE(nox): User variables
(defgroup my-noter nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom my-noter-property-doc-file "NOTER_DOCUMENT"
  "Name of the property that specifies the document."
  :group 'my-noter
  :type 'string)

(defcustom my-noter-property-note-location "NOTER_PAGE"
  "Name of the property that specifies the location of the current note.
The default value is still NOTER_PAGE for backwards compatibility."
  :group 'my-noter
  :type 'string)

(defcustom my-noter-notes-window-behavior '(start scroll)
  "This setting specifies in what situations the notes window should be created.

When the list contains:
- `start', the window will be created when starting a `my-noter' session.
- `scroll', it will be created when you go to a location with an associated note.
- `only-prev', it will be created when you go to a location without notes, but that
   has previous notes that are shown."
  :group 'my-noter
  :type '(set (const :tag "Session start" start)
              (const :tag "Scroll to location with notes" scroll)
              (const :tag "Scroll to location with previous notes only" only-prev)))

(defcustom my-noter-notes-window-location 'horizontal-split
  "Whether the notes should appear in the main frame (horizontal or vertical split) or in a separate frame.

Note that this will only have effect on session startup if `start'
is member of `my-noter-notes-window-behavior' (which see)."
  :group 'my-noter
  :type '(choice (const :tag "Horizontal" horizontal-split)
                 (const :tag "Vertical" vertical-split)
                 (const :tag "Other frame" other-frame)))

(define-obsolete-variable-alias 'my-noter-doc-split-percentage 'my-noter-doc-split-fraction "1.2.0")
(defcustom my-noter-doc-split-fraction '(0.5 . 0.5)
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

(defcustom my-noter-suggest-from-attachments t
  "When non-nil, my-noter will suggest files from the attachments
when creating a session, if the document is missing."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-separate-notes-from-heading nil
  "When non-nil, add an empty line between each note's heading and content."
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
  :type '(repeat string))

(defcustom my-noter-notes-search-path '("~/Documents/Database/000_notes")
  "List of paths to check (non recursively) when searching for a notes file."
  :group 'my-noter
  :type '(repeat string))

(defcustom my-noter-arrow-delay 0.2
  "Number of seconds from when the command was invoked until the tooltip arrow appears.

When set to a negative number, the arrow tooltip is disabled.
This is needed in order to keep Emacs from hanging when doing many syncs."
  :group 'my-noter
  :type 'number)

(defcustom my-noter-doc-property-in-notes nil
  "If non-nil, every new note will have the document property too.
This makes moving notes out of the root heading easier."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-add-note-no-questions nil
  "When non-nil, `my-noter-add-note' won't ask for a title and will always insert a new note.
The title used will be the default one."
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-kill-frame-at-session-end t
  "If non-nil, `my-noter-kill-session' will delete the frame if others exist on the current display.'"
  :group 'my-noter
  :type 'boolean)

(defcustom my-noter-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'my-noter
  :type 'hook)

(defface my-noter-no-notes-exist-face
  '((t
     :foreground "chocolate"
     :weight bold))
  "Face for modeline note count, when 0."
  :group 'my-noter)

(defface my-noter-notes-exist-face
  '((t
     :foreground "SpringGreen"
     :weight bold))
  "Face for modeline note count, when not 0."
  :group 'my-noter)

;; --------------------------------------------------------------------------------
;; NOTE(nox): Integration with other packages
(defcustom my-noter--check-location-property-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--parse-location-property-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--pretty-print-location-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--convert-to-location-cons-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--doc-goto-location-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--note-after-tipping-point-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--relative-position-to-view-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--get-precise-info-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

(defcustom my-noter--doc-approx-location-hook nil
  "TODO"
  :group 'my-noter
  :type 'hook)

;; --------------------------------------------------------------------------------
;; NOTE(nox): Private variables or constants
(cl-defstruct my-noter--session
  id frame doc-buffer notes-buffer ast modified-tick doc-mode display-name notes-file-path property-text
  level num-notes-in-view window-behavior window-location doc-split-fraction auto-save-last-location
  hide-other closest-tipping-point)

(defvar my-noter--sessions nil
  "List of `my-noter' sessions.")

(defvar-local my-noter--session nil
  "Session associated with the current buffer.")

(defvar my-noter--inhibit-location-change-handler nil
  "Prevent location change from updating point in notes.")

(defvar my-noter--start-location-override nil
  "Used to open the session from the document in the right page.")

(defvar-local my-noter--nov-timer nil
  "Timer for synchronizing notes after scrolling.")

(defvar my-noter--arrow-location nil
  "A vector [TIMER WINDOW TOP] that shows where the arrow should appear, when idling.")

(defvar my-noter--completing-read-keymap (make-sparse-keymap)
  "A `completing-read' keymap that let's the user insert spaces.")

(set-keymap-parent my-noter--completing-read-keymap minibuffer-local-completion-map)
(define-key my-noter--completing-read-keymap (kbd "SPC") 'self-insert-command)

(defconst my-noter--property-behavior "NOTER_NOTES_BEHAVIOR"
  "Property for overriding global `my-noter-notes-window-behavior'.")

(defconst my-noter--property-location "NOTER_NOTES_LOCATION"
  "Property for overriding global `my-noter-notes-window-location'.")

(defconst my-noter--property-doc-split-fraction "NOTER_DOCUMENT_SPLIT_FRACTION"
  "Property for overriding global `my-noter-doc-split-fraction'.")

(defconst my-noter--property-auto-save-last-location "NOTER_AUTO_SAVE_LAST_LOCATION"
  "Property for overriding global `my-noter-auto-save-last-location'.")

(defconst my-noter--property-hide-other "NOTER_HIDE_OTHER"
  "Property for overriding global `my-noter-hide-other'.")

(defconst my-noter--property-closest-tipping-point "NOTER_CLOSEST_TIPPING_POINT"
  "Property for overriding global `my-noter-closest-tipping-point'.")

(defconst my-noter--note-search-no-recurse (delete 'headline (append org-element-all-elements nil))
  "List of elements that shouldn't be recursed into when searching for notes.")

(defconst my-noter--id-text-property 'my-noter-session-id
  "Text property used to mark the headings with open sessions.")

;; --------------------------------------------------------------------------------
;; NOTE(nox): Utility functions
(defun my-noter--get-new-id ()
  (catch 'break
    (while t
      (let ((id (random most-positive-fixnum)))
        (unless (cl-loop for session in my-noter--sessions
                         when (= (my-noter--session-id session) id) return t)
          (throw 'break id))))))

(defmacro my-noter--property-or-default (name)
  (let ((function-name (intern (concat "my-noter--" (symbol-name name) "-property")))
        (variable      (intern (concat "my-noter-"  (symbol-name name)))))
    `(let ((prop-value (,function-name ast)))
       (cond ((eq prop-value 'disable) nil)
             (prop-value)
             (t ,variable)))))

(defun my-noter--valid-session (session)
  (when session
    (if (and (frame-live-p (my-noter--session-frame session))
             (buffer-live-p (my-noter--session-doc-buffer session))
             (buffer-live-p (my-noter--session-notes-buffer session)))
        t
      (my-noter-kill-session session)
      nil)))

(defmacro my-noter--with-valid-session (&rest body)
  (declare (debug (body)))
  `(let ((session my-noter--session))
     (when (my-noter--valid-session session)
       (progn ,@body))))

(defun my-noter--handle-kill-buffer ()
  (my-noter--with-valid-session
   (let ((buffer (current-buffer))
         (notes-buffer (my-noter--session-notes-buffer session))
         (doc-buffer (my-noter--session-doc-buffer session)))
     ;; NOTE(nox): This needs to be checked in order to prevent session killing because of
     ;; temporary buffers with the same local variables
     (when (or (eq buffer notes-buffer)
               (eq buffer doc-buffer))
       (my-noter-kill-session session)))))

(defun my-noter--handle-delete-frame (frame)
  (dolist (session my-noter--sessions)
    (when (eq (my-noter--session-frame session) frame)
      (my-noter-kill-session session))))

(defun my-noter--parse-root (&optional info)
  "Parse and return the root AST.
When used, the INFO argument may be an my-noter session or a vector [NotesBuffer PropertyText].
If nil, the session used will be `my-noter--session'."
  (let* ((arg-is-session (my-noter--session-p info))
         (session (or (and arg-is-session info) my-noter--session))
         root-pos ast)
    (cond
     ((and (not arg-is-session) (vectorp info))
      ;; NOTE(nox): Use arguments to find heading, by trying to find the outermost parent heading with
	  ;; the specified property
      (let ((notes-buffer (aref info 0))
            (wanted-prop  (aref info 1)))
        (unless (and (buffer-live-p notes-buffer) (stringp wanted-prop)
                     (eq (buffer-local-value 'major-mode notes-buffer) 'org-mode))
          (error "Error parsing root with invalid arguments"))

        (with-current-buffer notes-buffer
          (org-with-wide-buffer
           (catch 'break
	         (org-back-to-heading t)
	         (while t
		       (when (string= (org-entry-get nil my-noter-property-doc-file) wanted-prop)
                 (setq root-pos (copy-marker (point))))
               (unless (org-up-heading-safe) (throw 'break t))))))))

     ((my-noter--valid-session session)
      ;; NOTE(nox): Use session to find heading
      (or (and (= (buffer-chars-modified-tick (my-noter--session-notes-buffer session))
                  (my-noter--session-modified-tick session))
               (setq ast (my-noter--session-ast session))) ; NOTE(nox): Cached version!

          ;; NOTE(nox): Find session id text property
          (with-current-buffer (my-noter--session-notes-buffer session)
            (org-with-wide-buffer
             (let ((pos (text-property-any (point-min) (point-max) my-noter--id-text-property
                                           (my-noter--session-id session))))
               (when pos (setq root-pos (copy-marker pos)))))))))

    (unless ast
      (unless root-pos (error "Root heading not found"))
      (with-current-buffer (marker-buffer root-pos)
        (org-with-wide-buffer
         (goto-char (marker-position root-pos))
         (org-narrow-to-subtree)
         (setq ast (car (org-element-contents (org-element-parse-buffer 'greater-element))))
         (when (and (not (vectorp info)) (my-noter--valid-session session))
           (setf (my-noter--session-ast session) ast
                 (my-noter--session-modified-tick session) (buffer-chars-modified-tick))))))
    ast))

(defun my-noter--get-properties-end (ast &optional force-trim)
  (when ast
    (let* ((contents (org-element-contents ast))
           (section (org-element-map contents 'section 'identity nil t 'headline))
           (properties (org-element-map section 'property-drawer 'identity nil t))
           properties-end)
      (if (not properties)
          (org-element-property :contents-begin ast)
        (setq properties-end (org-element-property :end properties))
        (when (or force-trim
                  (= (org-element-property :end section) properties-end))
          (while (not (eq (char-before properties-end) ?:))
            (setq properties-end (1- properties-end))))
        properties-end))))

(defun my-noter--set-text-properties (ast id)
  (org-with-wide-buffer
   (when ast
     (let* ((level (org-element-property :level ast))
            (begin (org-element-property :begin ast))
            (title-begin (+ 1 level begin))
            (contents-begin (org-element-property :contents-begin ast))
            (properties-end (my-noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (add-text-properties (max 1 (1- begin)) begin '(read-only t))
       (add-text-properties begin (1- title-begin) `(read-only t front-sticky t ,my-noter--id-text-property ,id))
       (add-text-properties (1- title-begin) title-begin '(read-only t rear-nonsticky t))
       (add-text-properties (1- contents-begin) (1- properties-end) '(read-only t))
       (add-text-properties (1- properties-end) properties-end
                            '(read-only t rear-nonsticky t))
       (set-buffer-modified-p modified)))))

(defun my-noter--unset-text-properties (ast)
  (when ast
    (org-with-wide-buffer
     (let* ((begin (org-element-property :begin ast))
            (end (my-noter--get-properties-end ast t))
            (inhibit-read-only t)
            (modified (buffer-modified-p)))
       (remove-list-of-text-properties (max 1 (1- begin)) end
                                       `(read-only front-sticky rear-nonsticky ,my-noter--id-text-property))
       (set-buffer-modified-p modified)))))

(defun my-noter--set-notes-scroll (window &rest ignored)
  (when window
    (with-selected-window window
      (my-noter--with-valid-session
       (let* ((level (my-noter--session-level session))
              (goal (* (1- level) 2))
              (current-scroll (window-hscroll)))
         (when (and (bound-and-true-p org-indent-mode) (< current-scroll goal))
           (scroll-right current-scroll)
           (scroll-left goal t)))))))

(defun my-noter--insert-heading (level title &optional newlines-number location)
  "Insert a new heading at LEVEL with TITLE.
The point will be at the start of the contents, after any
properties, by a margin of NEWLINES-NUMBER."
  (setq newlines-number (or newlines-number 1))
  (org-insert-heading nil t)
  (let* ((initial-level (org-element-property :level (org-element-at-point)))
         (changer (if (> level initial-level) 'org-do-demote 'org-do-promote))
         (number-of-times (abs (- level initial-level))))
    (dotimes (_ number-of-times) (funcall changer))
    (insert (org-trim (replace-regexp-in-string "\n" " " title)))

    (org-end-of-subtree)
    (unless (bolp) (insert "\n"))
    (org-N-empty-lines-before-current (1- newlines-number))

    (when location
      (org-entry-put nil my-noter-property-note-location (number-to-string location))

      (when my-noter-doc-property-in-notes
        (my-noter--with-valid-session
         (org-entry-put nil my-noter-property-doc-file (my-noter--session-property-text session))
         (org-entry-put nil my-noter--property-auto-save-last-location "nil"))))

    (run-hooks 'my-noter-insert-heading-hook)))

(defun my-noter--narrow-to-root (ast)
  (when ast
    (save-excursion
      (goto-char (org-element-property :contents-begin ast))
      (org-show-entry)
      (org-narrow-to-subtree)
      (org-cycle-hide-drawers 'all))))

(defun my-noter--get-doc-window ()
  (my-noter--with-valid-session
   (or (get-buffer-window (my-noter--session-doc-buffer session)
                          (my-noter--session-frame session))
       (my-noter--setup-windows my-noter--session)
       (get-buffer-window (my-noter--session-doc-buffer session)
                          (my-noter--session-frame session)))))

(defun my-noter--get-notes-window (&optional type)
  (my-noter--with-valid-session
   (let ((notes-buffer (my-noter--session-notes-buffer session))
         (window-location (my-noter--session-window-location session))
         (window-behavior (my-noter--session-window-behavior session))
         notes-window)
     (or (get-buffer-window notes-buffer t)
         (when (or (eq type 'force) (memq type window-behavior))
           (if (eq window-location 'other-frame)
               (let ((restore-frame (selected-frame)))
                 (switch-to-buffer-other-frame notes-buffer)
                 (setq notes-window (get-buffer-window notes-buffer t))
                 (x-focus-frame restore-frame)
                 (raise-frame (window-frame notes-window)))

             (with-selected-window (my-noter--get-doc-window)
               (let ((horizontal (eq window-location 'horizontal-split)))
                 (setq
                  notes-window
                  (if (window-combined-p nil horizontal)
                      ;; NOTE(nox): Reuse already existent window
                      (let ((sibling-window (or (window-next-sibling) (window-prev-sibling))))
                        (or (window-top-child sibling-window) (window-left-child sibling-window)
                            sibling-window))

                    (if horizontal
                        (split-window-right (ceiling (* (car (my-noter--session-doc-split-fraction session))
                                                        (window-total-width))))
                      (split-window-below (ceiling (* (cdr (my-noter--session-doc-split-fraction session))
                                                      (window-total-height)))))))))

             (set-window-buffer notes-window notes-buffer))
           notes-window)))))

(defun my-noter--setup-windows (session)
  "Setup windows when starting session, respecting user configuration."
  (when (my-noter--valid-session session)
    (with-selected-frame (my-noter--session-frame session)
      (delete-other-windows)
      (let* ((doc-buffer (my-noter--session-doc-buffer session))
             (doc-window (selected-window))
             (notes-buffer (my-noter--session-notes-buffer session))
             notes-window)

        (set-window-buffer doc-window doc-buffer)
        (set-window-dedicated-p doc-window t)

        (with-current-buffer notes-buffer
          (my-noter--narrow-to-root (my-noter--parse-root session))
          (setq notes-window (my-noter--get-notes-window 'start))
          (my-noter--set-notes-scroll notes-window))
        (run-hooks 'my-noter-mode-hook)
        )))
  )

(defmacro my-noter--with-selected-notes-window (error-str &rest body)
  (declare (debug ([&optional stringp] body)))
  (let ((with-error (stringp error-str)))
    `(my-noter--with-valid-session
      (let ((notes-window (my-noter--get-notes-window)))
        (if notes-window
            (with-selected-window notes-window
              ,(if with-error
                   `(progn ,@body)
                 (if body
                     `(progn ,error-str ,@body)
                   `(progn ,error-str))))
          ,(when with-error `(user-error "%s" ,error-str)))))))

(defun my-noter--notes-window-behavior-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-behavior)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (when (listp value) value))))

(defun my-noter--notes-window-location-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-location)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (intern property))
      (when (memq value '(horizontal-split vertical-split other-frame)) value))))

(defun my-noter--doc-split-fraction-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-doc-split-fraction)) ast))
        value)
    (when (and (stringp property) (> (length property) 0))
      (setq value (car (read-from-string property)))
      (when (consp value) value))))

(defun my-noter--auto-save-last-location-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-auto-save-last-location)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (if (intern property) t 'disable))))

(defun my-noter--hide-other-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-hide-other)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (if (intern property) t 'disable))))

(defun my-noter--closest-tipping-point-property (ast)
  (let ((property (org-element-property (intern (concat ":" my-noter--property-closest-tipping-point)) ast)))
    (when (and (stringp property) (> (length property) 0))
      (ignore-errors (string-to-number property)))))

(defun my-noter--doc-approx-location-cons (&optional precise-info)
  (cond
   ((memq major-mode '(doc-view-mode pdf-view-mode))
    (cons (image-mode-window-get 'page) (if (numberp precise-info) precise-info 0)))

   ((eq major-mode 'nov-mode)
    (cons nov-documents-index (if (integerp precise-info)
                                  precise-info
                                (max 1 (/ (+ (window-start) (window-end nil t)) 2)))))

      (t (cons (point) (if (numberp precise-info) precise-info 0)))
   ;; (t (error "Unknown document type %s" major-mode))
   ))

(defun my-noter--doc-approx-location (&optional precise-info force-new-ref)
  (or (run-hook-with-args-until-success 'my-noter--doc-approx-location-hook major-mode
                                            precise-info force-new-ref)
          (my-noter--doc-approx-location-cons precise-info)))

(defun my-noter--location-change-advice (&rest _)
  (my-noter--with-valid-session (my-noter--doc-location-change-handler)))

(defun my-noter--nov-scroll-handler (&rest _)
  (when my-noter--nov-timer (cancel-timer my-noter--nov-timer))
  (unless my-noter--inhibit-location-change-handler
    (setq my-noter--nov-timer (run-with-timer 0.25 nil 'my-noter--doc-location-change-handler))))

(defsubst my-noter--doc-file-property (headline)
  (org-element-property (intern (concat ":" my-noter-property-doc-file)) headline))

(defun my-noter--check-location-property (arg)
  (let ((property (if (stringp arg) arg
                    (org-element-property (intern (concat ":" my-noter-property-note-location)) arg))))
    (when (and (stringp property) (> (length property) 0))
      (or (run-hook-with-args-until-success 'my-noter--check-location-property-hook property)
          (let ((value (car (read-from-string property))))
            (or (and (consp value) (integerp (car value)) (numberp (cdr value)))
                (integerp value)))))))

(defun my-noter--parse-location-property (arg)
  (let ((property (if (stringp arg) arg
                    (org-element-property (intern (concat ":" my-noter-property-note-location)) arg))))
    (when (and (stringp property) (> (length property) 0))
      (or (run-hook-with-args-until-success 'my-noter--parse-location-property-hook property)
          (let ((value (car (read-from-string property))))
            (cond ((and (consp value) (integerp (car value)) (numberp (cdr value))) value)
                  ((integerp value) (cons value 0))))))))

(defun my-noter--pretty-print-location (location)
  (my-noter--with-valid-session
   (or (run-hook-with-args-until-success 'my-noter--pretty-print-location-hook location)
       (format "%s" (cond
                     ((memq (my-noter--session-doc-mode session) '(doc-view-mode pdf-view-mode))
                      (if (or (not (cdr location)) (<= (cdr location) 0))
                          (car location)
                        location))

                     ((eq (my-noter--session-doc-mode session) 'nov-mode)
                      (if (or (not (cdr location)) (<= (cdr location) 1))
                          (car location)
                        location)))))))

(defun my-noter--get-containing-heading (&optional include-root)
  "Get smallest containing heading that encloses the point and has location property.
If the point isn't inside any heading with location property, return the outer heading.
When INCLUDE-ROOT is non-nil, the root heading is also eligible to be returned."
  (my-noter--with-valid-session
   (org-with-wide-buffer
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (let (previous)
        (catch 'break
          (while t
            (let ((prop (my-noter--check-location-property (org-entry-get nil my-noter-property-note-location)))
                  (at-root (equal (my-noter--session-id session)
                                  (get-text-property (point) my-noter--id-text-property)))
                  (heading (org-element-at-point)))
              (when (and prop (or include-root (not at-root)))
                (throw 'break heading))

              (when (or at-root (not (org-up-heading-safe)))
                (throw 'break (if include-root heading previous)))

              (setq previous heading)))))))))

(defun my-noter--doc-get-page-slice ()
  "Return (slice-top . slice-height)."
  (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
         (slice-top (float (nth 1 slice)))
         (slice-height (float (nth 3 slice))))
    (when (or (> slice-top 1)
              (> slice-height 1))
      (let ((height (cdr (image-size (image-mode-window-get 'image) t))))
        (setq slice-top (/ slice-top height)
              slice-height (/ slice-height height))))
    (cons slice-top slice-height)))

(defun my-noter--conv-page-scroll-percentage (scroll)
  (let* ((slice (my-noter--doc-get-page-slice))
         (display-height (cdr (image-display-size (image-get-display-property))))
         (display-percentage (/ scroll display-height))
         (percentage (+ (car slice) (* (cdr slice) display-percentage))))
    (max 0 (min 1 percentage))))

(defun my-noter--conv-page-percentage-scroll (percentage)
  (let* ((slice (my-noter--doc-get-page-slice))
         (display-height (cdr (image-display-size (image-get-display-property))))
         (display-percentage (min 1 (max 0 (/ (- percentage (car slice)) (cdr slice)))))
         (scroll (max 0 (floor (* display-percentage display-height)))))
    scroll))

(defun my-noter--get-precise-info ()
  (my-noter--with-valid-session
   (let ((window (my-noter--get-doc-window))
         (mode (my-noter--session-doc-mode session))
         event)
     (with-selected-window window
       (cond
        ((run-hook-with-args-until-success 'my-noter--get-precise-info-hook mode))

        ((eq mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (cadar (pdf-view-active-region))
           (while (not (and (eq 'mouse-1 (car event))
                            (eq window (posn-window (event-start event)))))
             (setq event (read-event "Click where you want the start of the note to be!")))
           (my-noter--conv-page-scroll-percentage (+ (window-vscroll)
                                                      (cdr (posn-col-row (event-start event)))))))

        ((eq mode 'doc-view-mode)
         (while (not (and (eq 'mouse-1 (car event))
                          (eq window (posn-window (event-start event)))))
           (setq event (read-event "Click where you want the start of the note to be!")))
         (my-noter--conv-page-scroll-percentage (+ (window-vscroll)
                                                    (cdr (posn-col-row (event-start event))))))

        ((eq mode 'nov-mode)
         (if (region-active-p)
             (min (mark) (point))
           (while (not (and (eq 'mouse-1 (car event))
                            (eq window (posn-window (event-start event)))))
             (setq event (read-event "Click where you want the start of the note to be!")))
           (posn-point (event-start event)))))))))

(defun my-noter--show-arrow ()
  (when (and my-noter--arrow-location
             (window-live-p (aref my-noter--arrow-location 1)))
    (with-selected-window (aref my-noter--arrow-location 1)
      (pdf-util-tooltip-arrow (aref my-noter--arrow-location 2))))
  (setq my-noter--arrow-location nil))

(defun my-noter--doc-goto-location (location)
  "Go to location specified by LOCATION."
  (my-noter--with-valid-session
   (let ((window (my-noter--get-doc-window))
         (mode (my-noter--session-doc-mode session)))
     (with-selected-window window
       (cond
        ((run-hook-with-args-until-success 'my-noter--doc-goto-location-hook mode location))

        ((memq mode '(doc-view-mode pdf-view-mode))
         (if (eq mode 'doc-view-mode)
             (doc-view-goto-page (car location))
           (pdf-view-goto-page (car location))
           ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
           ;; so syncing multiple pages was slow
           (when (>= my-noter-arrow-delay 0)
             (when my-noter--arrow-location (cancel-timer (aref my-noter--arrow-location 0)))
             (setq my-noter--arrow-location
                   (vector (run-with-idle-timer my-noter-arrow-delay nil 'my-noter--show-arrow)
                           window
                           (cdr location)))))
         (image-scroll-up (- (my-noter--conv-page-percentage-scroll (cdr location))
                             (window-vscroll))))

        ((eq mode 'nov-mode)
         (setq nov-documents-index (car location))
         (nov-render-document)
         (goto-char (cdr location))
         (recenter)))
       ;; NOTE(nox): This needs to be here, because it would be issued anyway after
       ;; everything and would run my-noter--nov-scroll-handler.
       (redisplay)))))

(defun my-noter--compare-location-cons (comp l1 l2)
  "Compare L1 and L2, which are location cons.
See `my-noter--compare-locations'"
  (cl-assert (and (consp l1) (consp l2)))
  (cond ((eq comp '=)
         (and (= (car l1) (car l2))
              (= (cdr l1) (cdr l2))))
        ((eq comp '<)
         (or (< (car l1) (car l2))
             (and (= (car l1) (car l2))
                  (< (cdr l1) (cdr l2)))))
        ((eq comp '<=)
         (or (< (car l1) (car l2))
             (and (=  (car l1) (car l2))
                  (<= (cdr l1) (cdr l2)))))
        ((eq comp '>)
         (or (> (car l1) (car l2))
             (and (= (car l1) (car l2))
                  (> (cdr l1) (cdr l2)))))
        ((eq comp '>=)
         (or (> (car l1) (car l2))
             (and (= (car l1) (car l2))
                  (>= (cdr l1) (cdr l2)))))
        ((eq comp '>f)
         (or (> (car l1) (car l2))
             (and (= (car l1) (car l2))
                  (< (cdr l1) (cdr l2)))))
        (t (error "Comparison operator %s not known" comp))))

(defun my-noter--compare-locations (comp l1 l2)
  "Compare L1 and L2.
When COMP is '<, '<=, '>, or '>=, it works as expected.
When COMP is '>f, it will return t when L1 is a page greater than
L2 or, when in the same page, if L1 is the _f_irst of the two."
  (cond ((not l1) nil)
        ((not l2) t)
        (t
         (setq l1 (or (run-hook-with-args-until-success 'my-noter--convert-to-location-cons-hook l1) l1)
               l2 (or (run-hook-with-args-until-success 'my-noter--convert-to-location-cons-hook l2) l2))
         (my-noter--compare-location-cons comp l1 l2))))

(defun my-noter--show-note-entry (session note)
  "This will show the note entry and its children.
Every direct subheading _until_ the first heading that doesn't
belong to the same view (ie. until a heading with location or
document property) will be opened."
  (save-excursion
    (goto-char (org-element-property :contents-begin note))
    (org-show-set-visibility t)
    (org-element-map (org-element-contents note) 'headline
      (lambda (headline)
        (let ((doc-file (my-noter--doc-file-property headline)))
          (if (or (and doc-file (not (string= doc-file (my-noter--session-property-text session))))
                  (my-noter--check-location-property headline))
              t
            (goto-char (org-element-property :begin headline))
            (org-show-entry)
            (org-show-children)
            nil)))
      nil t org-element-all-elements)))

(defun my-noter--focus-notes-region (view-info)
  (my-noter--with-selected-notes-window
   (if (my-noter--session-hide-other session)
       (save-excursion
         (goto-char (org-element-property :begin (my-noter--parse-root)))
         (outline-hide-subtree))
     (org-cycle-hide-drawers 'all))

   (let* ((notes-cons (my-noter--view-info-notes view-info))
          (regions (or (my-noter--view-info-regions view-info)
                       (my-noter--view-info-prev-regions view-info)))
          (point-before (point))
          target-region
          point-inside-target-region)
     (cond
      (notes-cons
       (dolist (note-cons notes-cons) (my-noter--show-note-entry session (car note-cons)))

       (setq target-region (or (catch 'result (dolist (region regions)
                                                (when (and (>= point-before (car region))
                                                           (or (save-restriction (goto-char (cdr region)) (eobp))
                                                               (< point-before (cdr region))))
                                                  (setq point-inside-target-region t)
                                                  (throw 'result region))))
                               (car regions)))

       (let ((begin (car target-region)) (end (cdr target-region)) num-lines
             (target-char (if point-inside-target-region
                              point-before
                            (my-noter--get-properties-end (caar notes-cons))))
             (window-start (window-start)) (window-end (window-end nil t)))
         (setq num-lines (count-screen-lines begin end))

         (cond
          ((> num-lines (window-height))
           (goto-char begin)
           (recenter 0))

          ((< begin window-start)
           (goto-char begin)
           (recenter 0))

          ((> end window-end)
           (goto-char end)
           (recenter -2)))

         (goto-char target-char)))

      (t (my-noter--show-note-entry session (my-noter--parse-root)))))

   (org-cycle-show-empty-lines t)))

(defun my-noter--get-current-view ()
  "Return a vector with the current view information."
  (my-noter--with-valid-session
   (let ((mode (my-noter--session-doc-mode session)))
     (with-selected-window (my-noter--get-doc-window)
       (cond ((memq mode '(doc-view-mode pdf-view-mode))
              (vector 'paged (car (my-noter--doc-approx-location-cons))))
             ((eq mode 'nov-mode)
              (vector 'nov
                      (my-noter--doc-approx-location-cons (window-start))
                      (my-noter--doc-approx-location-cons (window-end nil t))))
             (t (error "Unknown document type")))))))

(defun my-noter--note-after-tipping-point (point location view)
  ;; NOTE(nox): This __assumes__ the note is inside the view!
  (let (hook-result)
    (cond
     ((setq hook-result (run-hook-with-args-until-success 'my-noter--note-after-tipping-point-hook
                                                          point location view))
      (cdr hook-result))
     ((eq (aref view 0) 'paged)
      (> (cdr location) point))
     ((eq (aref view 0) 'nov)
      (> (cdr location) (+ (* point (- (cdr (aref view 2)) (cdr (aref view 1))))
                           (cdr (aref view 1))))))))

(defun my-noter--relative-position-to-view (location view)
  (cond
   ((run-hook-with-args-until-success 'my-noter--relative-position-to-view-hook location view))

   ((eq (aref view 0) 'paged)
    (let ((note-page (car location))
          (view-page (aref view 1)))
      (cond ((< note-page view-page) 'before)
            ((= note-page view-page) 'inside)
            (t                       'after))))

   ((eq (aref view 0) 'nov)
    (let ((view-top (aref view 1))
          (view-bot (aref view 2)))
      (cond ((my-noter--compare-locations '<  location view-top) 'before)
            ((my-noter--compare-locations '<= location view-bot) 'inside)
            (t                                                    'after))))))

(defmacro my-noter--view-region-finish (info &optional terminating-headline)
  `(when ,info
     ,(if terminating-headline
          `(push (cons (aref ,info 1) (min (aref ,info 2) (org-element-property :begin ,terminating-headline)))
                 (gv-deref (aref ,info 0)))
        `(push (cons (aref ,info 1) (aref ,info 2)) (gv-deref (aref ,info 0))))
     (setq ,info nil)))

(defmacro my-noter--view-region-add (info list-name headline)
  `(progn
     (when (and ,info (not (eq (aref ,info 3) ',list-name)))
       (my-noter--view-region-finish ,info ,headline))

     (if ,info
         (setf (aref ,info 2) (max (aref ,info 2) (org-element-property :end ,headline)))
       (setq ,info (vector (gv-ref ,list-name)
                           (org-element-property :begin ,headline) (org-element-property :end ,headline)
                           ',list-name)))))

;; NOTE(nox): notes is a list of (HEADING . HEADING-TO-INSERT-TEXT-BEFORE):
;; - HEADING is the root heading of the note
;; - SHOULD-ADD-SPACE indicates if there should be extra spacing when inserting text to the note (ie. the
;;   note has contents)
(cl-defstruct my-noter--view-info notes regions prev-regions reference-for-insertion)

(defun my-noter--get-view-info (view &optional new-location)
  "Return VIEW related information.

When optional NEW-LOCATION is provided, it will be used to find
the best heading to serve as a reference to create the new one
relative to."
  (when view
    (my-noter--with-valid-session
     (let ((contents (org-element-contents (my-noter--parse-root)))
           (preamble t)
           notes-in-view regions-in-view
           reference-for-insertion reference-location
           (all-after-tipping-point t)
           (closest-tipping-point (and (>= (my-noter--session-closest-tipping-point session) 0)
                                       (my-noter--session-closest-tipping-point session)))
           closest-notes closest-notes-regions closest-notes-location
           ignore-until-level
           current-region-info) ;; NOTE(nox): [REGIONS-LIST-PTR START MAX-END REGIONS-LIST-NAME]

       (org-element-map contents 'headline
         (lambda (headline)
           (let ((doc-file (my-noter--doc-file-property headline))
                 (location (my-noter--parse-location-property headline)))
             (when (and ignore-until-level (<= (org-element-property :level headline) ignore-until-level))
               (setq ignore-until-level nil))

             (cond
              (ignore-until-level) ;; NOTE(nox): This heading is ignored, do nothing

              ((and doc-file (not (string= doc-file (my-noter--session-property-text session))))
               (my-noter--view-region-finish current-region-info headline)
               (setq ignore-until-level (org-element-property :level headline))
               (when (and preamble new-location
                          (or (not reference-for-insertion)
                              (>= (org-element-property :begin headline)
                                  (org-element-property :end (cdr reference-for-insertion)))))
                 (setq reference-for-insertion (cons 'after headline))))

              (location
               (let ((relative-position (my-noter--relative-position-to-view location view)))
                 (cond
                  ((eq relative-position 'inside)
                   (push (cons headline nil) notes-in-view)

                   (my-noter--view-region-add current-region-info regions-in-view headline)

                   (setq all-after-tipping-point
                         (and all-after-tipping-point (my-noter--note-after-tipping-point
                                                       closest-tipping-point location view))))

                  (t
                   (when current-region-info
                     (let ((note-cons-to-change (cond ((eq (aref current-region-info 3) 'regions-in-view)
                                                       (car notes-in-view))
                                                      ((eq (aref current-region-info 3) 'closest-notes-regions)
                                                       (car closest-notes)))))
                       (when (< (org-element-property :begin headline)
                                (org-element-property :end   (car note-cons-to-change)))
                         (setcdr note-cons-to-change headline))))

                   (let ((eligible-for-before (and closest-tipping-point all-after-tipping-point
                                                   (eq relative-position 'before))))
                     (cond ((and eligible-for-before
                                 (my-noter--compare-locations '> location closest-notes-location))
                            (setq closest-notes (list (cons headline nil))
                                  closest-notes-location location
                                  current-region-info nil
                                  closest-notes-regions nil)
                            (my-noter--view-region-add current-region-info closest-notes-regions headline))

                           ((and eligible-for-before (equal location closest-notes-location))
                            (push (cons headline nil) closest-notes)
                            (my-noter--view-region-add current-region-info closest-notes-regions headline))

                           (t (my-noter--view-region-finish current-region-info headline)))))))

               (when new-location
                 (setq preamble nil)
                 (cond ((and (my-noter--compare-locations '<= location new-location)
                             (or (eq (car reference-for-insertion) 'before)
                                 (my-noter--compare-locations '>= location reference-location)))
                        (setq reference-for-insertion (cons 'after headline)
                              reference-location location))

                       ((and (eq (car reference-for-insertion) 'after)
                             (< (org-element-property :begin headline)
                                (org-element-property :end   (cdr reference-for-insertion)))
                             (my-noter--compare-locations '>= location new-location))
                        (setq reference-for-insertion (cons 'before headline)
                              reference-location location)))))

              (t
               (when (and preamble new-location
                          (or (not reference-for-insertion)
                              (>= (org-element-property :begin headline)
                                  (org-element-property :end (cdr reference-for-insertion)))))
                 (setq reference-for-insertion (cons 'after headline)))))))
         nil nil my-noter--note-search-no-recurse)

       (my-noter--view-region-finish current-region-info)

       (setf (my-noter--session-num-notes-in-view session) (length notes-in-view))

       (when all-after-tipping-point (setq notes-in-view (append closest-notes notes-in-view)))

       (make-my-noter--view-info
        :notes (nreverse notes-in-view)
        :regions (nreverse regions-in-view)
        :prev-regions (nreverse closest-notes-regions)
        :reference-for-insertion reference-for-insertion)))))

(defun my-noter--make-view-info-for-single-note (session headline)
  (let ((not-belonging-element
         (org-element-map (org-element-contents headline) 'headline
           (lambda (headline)
             (let ((doc-file (my-noter--doc-file-property headline)))
               (and (or (and doc-file (not (string= doc-file (my-noter--session-property-text session))))
                        (my-noter--check-location-property headline))
                    headline)))
           nil t)))

    (make-my-noter--view-info
     ;; NOTE(nox): The cdr is only used when inserting, doesn't matter here
     :notes (list (cons headline nil))
     :regions (list (cons (org-element-property :begin headline)
                          (or (and not-belonging-element (org-element-property :begin not-belonging-element))
                              (org-element-property :end headline)))))))

(defun my-noter--doc-location-change-handler ()
  (my-noter--with-valid-session
   (let ((view-info (my-noter--get-view-info (my-noter--get-current-view))))
     (force-mode-line-update t)
     (unless my-noter--inhibit-location-change-handler
       (my-noter--get-notes-window (cond ((my-noter--view-info-regions view-info) 'scroll)
                                          ((my-noter--view-info-prev-regions view-info) 'only-prev)))
       (my-noter--focus-notes-region view-info)))

   (when (my-noter--session-auto-save-last-location session) (my-noter-set-start-location))))

(defun my-noter--mode-line-text ()
  (my-noter--with-valid-session
   (let* ((number-of-notes (or (my-noter--session-num-notes-in-view session) 0)))
     (cond ((= number-of-notes 0) (propertize " 0 notes " 'face 'my-noter-no-notes-exist-face))
           ((= number-of-notes 1) (propertize " 1 note " 'face 'my-noter-notes-exist-face))
           (t (propertize (format " %d notes " number-of-notes) 'face 'my-noter-notes-exist-face))))))

;; NOTE(nox): From machc/pdf-tools-org
(defun my-noter--pdf-tools-edges-to-region (edges)
  "Get 4-entry region (LEFT TOP RIGHT BOTTOM) from several EDGES."
  (when edges
    (let ((left0 (nth 0 (car edges)))
          (top0 (nth 1 (car edges)))
          (bottom0 (nth 3 (car edges)))
          (top1 (nth 1 (car (last edges))))
          (right1 (nth 2 (car (last edges))))
          (bottom1 (nth 3 (car (last edges)))))
      (list left0
            (+ top0 (/ (- bottom0 top0) 3))
            right1
            (- bottom1 (/ (- bottom1 top1) 3))))))

(defun my-noter--check-if-document-is-annotated-on-file (document-path notes-path)
  ;; NOTE(nox): In order to insert the correct file contents
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

(defsubst my-noter--check-doc-prop (doc-prop)
  (and doc-prop
       (or
        (ffap-url-p doc-prop)
        (and (not (file-directory-p doc-prop)) (file-readable-p doc-prop)))))

(defun my-noter--get-or-read-document-property (inherit-prop &optional force-new)
  (let ((doc-prop
         (and (not force-new)
              (org-entry-get nil my-noter-property-doc-file inherit-prop))))

    (PDEBUG "DOC-PROP:" doc-prop)
    (unless (my-noter--check-doc-prop doc-prop)
      (setq doc-prop nil)

      (when my-noter-suggest-from-attachments
        (require 'org-attach)
        (let* ((attach-dir (org-attach-dir))
               (attach-list (and attach-dir (org-attach-file-list attach-dir))))
          (when (and attach-list (y-or-n-p "Do you want to annotate an attached file?"))
            (setq doc-prop (completing-read "File to annotate: " attach-list nil t))
            (when doc-prop (setq doc-prop (expand-file-name doc-prop attach-dir))))))

      (unless (my-noter--check-doc-prop doc-prop)
        (setq doc-prop (expand-file-name
                        (read-file-name
                         "Invalid or no document property found. Please specify a document path: " nil nil t)))
        (when (or (file-directory-p doc-prop) (not (file-readable-p doc-prop))) (user-error "Invalid file path"))
        ;; (when (y-or-n-p "Do you want a relative file name? ") (setq doc-prop (file-relative-name doc-prop)))
        )

      (org-entry-put nil my-noter-property-doc-file doc-prop))
    doc-prop))

(defun my-noter--other-frames (&optional this-frame)
  "Returns non-`nil' when there is at least another frame"
  (setq this-frame (or this-frame (selected-frame)))
  (catch 'other-frame
    (dolist (frame (visible-frame-list))
      (unless (or (eq this-frame frame)
                  (frame-parent frame)
                  (frame-parameter frame 'delete-before))
        (throw 'other-frame frame)))))

;; --------------------------------------------------------------------------------
;; NOTE(nox): User commands
(defun my-noter-set-start-location (&optional arg)
  "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
  (interactive "P")
  (my-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (my-noter--parse-root))
         (location (my-noter--doc-approx-location 'interactive)))
     (with-current-buffer (my-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil my-noter-property-note-location)
          (org-entry-put nil my-noter-property-note-location
                         (my-noter--pretty-print-location location))))))))

(defun my-noter-set-auto-save-last-location (arg)
  "This toggles saving the last visited location for this document.
With a prefix ARG, delete the current setting and use the default."
  (interactive "P")
  (my-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (my-noter--parse-root))
         (new-setting (if arg
                          my-noter-auto-save-last-location
                        (not (my-noter--session-auto-save-last-location session)))))
     (setf (my-noter--session-auto-save-last-location session)
           new-setting)
     (with-current-buffer (my-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil my-noter--property-auto-save-last-location)
          (org-entry-put nil my-noter--property-auto-save-last-location (format "%s" new-setting)))
        (unless new-setting (org-entry-delete nil my-noter-property-note-location)))))))

(defun my-noter-set-hide-other (arg)
  "This toggles hiding other headings for the current session.
- With a prefix \\[universal-argument], set the current setting permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (my-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (my-noter--parse-root))
          (persistent
           (cond ((equal arg '(4)) 'write)
                 ((equal arg '(16)) 'remove)))
          (new-setting
           (cond ((eq persistent 'write) (my-noter--session-hide-other session))
                 ((eq persistent 'remove) my-noter-hide-other)
                 ('other-cases (not (my-noter--session-hide-other session))))))
     (setf (my-noter--session-hide-other session) new-setting)
     (when persistent
       (with-current-buffer (my-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil my-noter--property-hide-other (format "%s" new-setting))
            (org-entry-delete nil my-noter--property-hide-other))))))))

(defun my-noter-set-closest-tipping-point (arg)
  "This sets the closest note tipping point (see `my-noter-closest-tipping-point')
- With a prefix \\[universal-argument], set it permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (my-noter--with-valid-session
   (let* ((ast (my-noter--parse-root))
          (inhibit-read-only t)
          (persistent (cond ((equal arg '(4)) 'write)
                            ((equal arg '(16)) 'remove)))
          (new-setting (if (eq persistent 'remove)
                           my-noter-closest-tipping-point
                         (read-number "New tipping point: " (my-noter--session-closest-tipping-point session)))))
     (setf (my-noter--session-closest-tipping-point session) new-setting)
     (when persistent
       (with-current-buffer (my-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil my-noter--property-closest-tipping-point (format "%f" new-setting))
            (org-entry-delete nil my-noter--property-closest-tipping-point))))))))

(defun my-noter-set-notes-window-behavior (arg)
  "Set the notes window behaviour for the current session.
With a prefix ARG, it becomes persistent for that document.

See `my-noter-notes-window-behavior' for more information."
  (interactive "P")
  (my-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (my-noter--parse-root))
          (possible-behaviors (list '("Default" . default)
                                    '("On start" . start)
                                    '("On scroll" . scroll)
                                    '("On scroll to location that only has previous notes" . only-prev)
                                    '("Never" . never)))
          chosen-behaviors)

     (while (> (length possible-behaviors) 1)
       (let ((chosen-pair (assoc (completing-read "Behavior: " possible-behaviors nil t) possible-behaviors)))
         (cond ((eq (cdr chosen-pair) 'default) (setq possible-behaviors nil))

               ((eq (cdr chosen-pair) 'never) (setq chosen-behaviors (list 'never)
                                                    possible-behaviors nil))

               ((eq (cdr chosen-pair) 'done) (setq possible-behaviors nil))

               (t (push (cdr chosen-pair) chosen-behaviors)
                  (setq possible-behaviors (delq chosen-pair possible-behaviors))
                  (when (= (length chosen-behaviors) 1)
                    (setq possible-behaviors (delq (rassq 'default possible-behaviors) possible-behaviors)
                          possible-behaviors (delq (rassq 'never possible-behaviors) possible-behaviors))
                    (push (cons "Done" 'done) possible-behaviors))))))

     (setf (my-noter--session-window-behavior session)
           (or chosen-behaviors my-noter-notes-window-behavior))

     (when arg
       (with-current-buffer (my-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if chosen-behaviors
              (org-entry-put nil my-noter--property-behavior (format "%s" chosen-behaviors))
            (org-entry-delete nil my-noter--property-behavior))))))))

(defun my-noter-set-notes-window-location (arg)
  "Set the notes window default location for the current session.
With a prefix ARG, it becomes persistent for that document.

See `my-noter-notes-window-behavior' for more information."
  (interactive "P")
  (my-noter--with-valid-session
   (let* ((inhibit-read-only t)
          (ast (my-noter--parse-root))
          (location-possibilities
           '(("Default" . nil)
             ("Horizontal split" . horizontal-split)
             ("Vertical split" . vertical-split)
             ("Other frame" . other-frame)))
          (location
           (cdr (assoc (completing-read "Location: " location-possibilities nil t)
                       location-possibilities)))
          (notes-buffer (my-noter--session-notes-buffer session)))

     (setf (my-noter--session-window-location session)
           (or location my-noter-notes-window-location))

     (let (exists)
       (dolist (window (get-buffer-window-list notes-buffer nil t))
         (setq exists t)
         (with-selected-frame (window-frame window)
           (if (= (count-windows) 1)
               (delete-frame)
             (delete-window window))))
       (when exists (my-noter--get-notes-window 'force)))

     (when arg
       (with-current-buffer notes-buffer
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if location
              (org-entry-put nil my-noter--property-location
                             (format "%s" location))
            (org-entry-delete nil my-noter--property-location))))))))

(defun my-noter-set-doc-split-fraction (arg)
  "Set the fraction of the frame that the document window will occupy when split.
- With a prefix \\[universal-argument], set it permanently for this document.
- With a prefix \\[universal-argument] \\[universal-argument], remove the setting and use the default."
  (interactive "P")
  (my-noter--with-valid-session
   (let* ((ast (my-noter--parse-root))
          (inhibit-read-only t)
          (persistent (cond ((equal arg '(4)) 'write)
                            ((equal arg '(16)) 'remove)))
          (current-setting (my-noter--session-doc-split-fraction session))
          (new-setting
           (if (eq persistent 'remove)
               my-noter-doc-split-fraction
             (cons (read-number "Horizontal fraction: " (car current-setting))
                   (read-number "Vertical fraction: " (cdr current-setting))))))
     (setf (my-noter--session-doc-split-fraction session) new-setting)
     (when (my-noter--get-notes-window)
       (with-current-buffer (my-noter--session-doc-buffer session)
         (delete-other-windows)
         (my-noter--get-notes-window 'force)))

     (when persistent
       (with-current-buffer (my-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if (eq persistent 'write)
              (org-entry-put nil my-noter--property-doc-split-fraction (format "%s" new-setting))
            (org-entry-delete nil my-noter--property-doc-split-fraction))))))))

(defun my-noter-kill-session (&optional session)
  "Kill an `my-noter' session.

When called interactively, if there is no prefix argument and the
buffer has an annotation session, it will kill it; else, it will
show a list of open `my-noter' sessions, asking for which to
kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length my-noter--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and my-noter--session (not session))
        (setq session my-noter--session)
      (setq session nil)
      (let (collection default doc-display-name notes-file-name display)
        (dolist (session my-noter--sessions)
          (setq doc-display-name (my-noter--session-display-name session)
                notes-file-name (file-name-nondirectory
                                 (my-noter--session-notes-file-path session))
                display (concat doc-display-name " - " notes-file-name))
          (when (eq session my-noter--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))

  (when (and session (memq session my-noter--sessions))
    (setq my-noter--sessions (delq session my-noter--sessions))

    (when (eq (length my-noter--sessions) 0)
      (remove-hook 'delete-frame-functions 'my-noter--handle-delete-frame)
      (advice-remove 'doc-view-goto-page 'my-noter--location-change-advice)
      (advice-remove 'nov-render-document 'my-noter--nov-scroll-handler))

    (let* ((ast   (my-noter--parse-root session))
           ;; (frame (my-noter--session-frame session))
           (notes-buffer (my-noter--session-notes-buffer session))
           (base-buffer (buffer-base-buffer notes-buffer))
           (notes-modified (buffer-modified-p base-buffer))
           (doc-buffer (my-noter--session-doc-buffer session)))

      (dolist (window (get-buffer-window-list notes-buffer nil t))
        (with-selected-frame (window-frame window)
          (if (= (count-windows) 1)
              (when (my-noter--other-frames) (delete-frame))
            (delete-window window))))

      (with-current-buffer notes-buffer
        (remove-hook 'kill-buffer-hook 'my-noter--handle-kill-buffer t)
        (restore-buffer-modified-p nil))
      (kill-buffer notes-buffer)

      (with-current-buffer base-buffer
        (my-noter--unset-text-properties ast)
        (set-buffer-modified-p notes-modified))

      (with-current-buffer doc-buffer
        (remove-hook 'kill-buffer-hook 'my-noter--handle-kill-buffer t))
      (kill-buffer doc-buffer)

      ;; (when (frame-live-p frame)
      ;;   (if (and (my-noter--other-frames) my-noter-kill-frame-at-session-end)
      ;;       (delete-frame frame)
      ;;     (progn
      ;;       (delete-other-windows)
      ;;       (set-frame-parameter nil 'name nil))))
      )))

(defun my-noter-create-skeleton ()
  "Create notes skeleton with the PDF outline or annotations.
Only available with PDF Tools."
  (interactive)
  (my-noter--with-valid-session
   (cond
    ((eq (my-noter--session-doc-mode session) 'pdf-view-mode)
     (let* ((ast (my-noter--parse-root))
            (top-level (org-element-property :level ast))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (my-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type  (alist-get 'type item))
                   (page  (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top   (alist-get 'top item)))
               (when (and (eq type 'goto-dest) (> page 0))
                 (push (vector title (cons page top) (1+ depth) nil) output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type  (alist-get 'type item))
                      (page  (alist-get 'page item))
                      (edges (or (my-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents)
                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight)  "Highlight")
                                      ((eq type 'underline)  "Underline")
                                      ((eq type 'squiggly)   "Squiggly")
                                      ((eq type 'text)       "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type  link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text)

                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push (vector heading-text (cons page top) 'inside nil) output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (my-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (my-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title          (aref data 0)
                     location       (aref data 1)
                     relative-level (aref data 2)
                     contents       (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (my-noter--insert-heading level title)

               (when location
                 (org-entry-put nil my-noter-property-note-location (my-noter--pretty-print-location location)))

               (when my-noter-doc-property-in-notes
                 (org-entry-put nil my-noter-property-doc-file (my-noter--session-property-text session))
                 (org-entry-put nil my-noter--property-auto-save-last-location "nil"))

               (when (car contents)
                 (my-noter--insert-heading (1+ level) "Contents")
                 (insert (car contents)))
               (when (cdr contents)
                 (my-noter--insert-heading (1+ level) "Comment")
                 (insert (cdr contents)))))

           (setq ast (my-noter--parse-root))
           (my-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))))

    (t (user-error "This command is only supported on PDF Tools.")))))


(defun my-noter-insert-precise-note (&optional toggle-no-questions)
  "Insert note associated with a specific location.
This will ask you to click where you want to scroll to when you
sync the document to this note. You should click on the top of
that part. Will always create a new note.

When text is selected, it will automatically choose the top of
the selected text as the location and the text itself as the
title of the note (you may change it anyway!).

See `my-noter-add-note' docstring for more."
  (interactive "P")
  (my-noter--with-valid-session
   (let ((my-noter-add-note-no-questions (if toggle-no-questions
                                                 (not my-noter-add-note-no-questions)
                                               my-noter-add-note-no-questions)))
     (my-noter-add-note (my-noter--get-precise-info)))))

(defun my-noter-add-note-toggle-no-questions ()
  "Insert note associated with the current location.
This is like `my-noter-add-note', except it will toggle `my-noter-add-note-no-questions'"
  (interactive)
  (my-noter--with-valid-session
   (let ((my-noter-add-note-no-questions (not my-noter-add-note-no-questions)))
     (my-noter-add-note))))

(defmacro my-noter--map-ignore-headings-with-doc-file (contents match-first &rest body)
  `(let (ignore-until-level)
     (org-element-map ,contents 'headline
       (lambda (headline)
         (let ((doc-file (my-noter--doc-file-property headline))
               (location (my-noter--parse-location-property headline)))
           (when (and ignore-until-level (<= (org-element-property :level headline) ignore-until-level))
             (setq ignore-until-level nil))

           (cond
            (ignore-until-level nil) ;; NOTE(nox): This heading is ignored, do nothing
            ((and doc-file (not (string= doc-file (my-noter--session-property-text session))))
             (setq ignore-until-level (org-element-property :level headline)) nil)
            (t ,@body))))
       nil ,match-first my-noter--note-search-no-recurse)))

(defun my-noter-sync-prev-page-or-chapter ()
  "Show previous page or chapter that has notes, in relation to the current page or chapter.
This will force the notes window to popup."
  (interactive)
  (my-noter--with-valid-session
   (let ((this-location (my-noter--doc-approx-location 0))
         (contents (org-element-contents (my-noter--parse-root)))
         target-location)
     (my-noter--get-notes-window 'force)

     (my-noter--map-ignore-headings-with-doc-file
      contents nil
      (when (and (my-noter--compare-locations '<  location this-location)
                 (my-noter--compare-locations '>f location target-location))
        (setq target-location location)))

     (my-noter--get-notes-window 'force)
     (select-window (my-noter--get-doc-window))
     (if target-location
         (my-noter--doc-goto-location target-location)
       (user-error "There are no more previous pages or chapters with notes")))))

(defun my-noter-sync-current-page-or-chapter ()
  "Show current page or chapter notes.
This will force the notes window to popup."
  (interactive)
  (my-noter--with-valid-session
   (let ((window (my-noter--get-notes-window 'force)))
     (select-frame-set-input-focus (window-frame window))
     (select-window window)
     (my-noter--doc-location-change-handler))))

(defun my-noter-sync-next-page-or-chapter ()
  "Show next page or chapter that has notes, in relation to the current page or chapter.
This will force the notes window to popup."
  (interactive)
  (my-noter--with-valid-session
   (let ((this-location (my-noter--doc-approx-location most-positive-fixnum))
         (contents (org-element-contents (my-noter--parse-root)))
         target-location)

     (my-noter--map-ignore-headings-with-doc-file
      contents nil
      (when (and (my-noter--compare-locations '> location this-location)
                 (my-noter--compare-locations '< location target-location))
        (setq target-location location)))

     (my-noter--get-notes-window 'force)
     (select-window (my-noter--get-doc-window))
     (if target-location
         (my-noter--doc-goto-location target-location)
       (user-error "There are no more following pages or chapters with notes")))))

(defun my-noter-sync-prev-note ()
  "Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (my-noter--with-selected-notes-window
   "No notes window exists"
   (let ((my-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (my-noter--parse-root)))
         (current-begin (org-element-property :begin (my-noter--get-containing-heading)))
         previous)
     (when current-begin
       (my-noter--map-ignore-headings-with-doc-file
        contents t
        (when location
          (if (= current-begin (org-element-property :begin headline))
              t
            (setq previous headline)
            nil))))

     (if previous
         (progn
           ;; NOTE(nox): This needs to be manual so we can focus the correct note
           (my-noter--doc-goto-location (my-noter--parse-location-property previous))
           (my-noter--focus-notes-region (my-noter--make-view-info-for-single-note session previous)))
       (user-error "There is no previous note"))))
  (select-window (my-noter--get-doc-window)))

(defun my-noter-sync-current-note ()
  "Go the location of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (my-noter--with-selected-notes-window
   "No notes window exists"
   (if (string= (org-entry-get nil my-noter-property-doc-file t) (my-noter--session-property-text session))
       (let ((location (my-noter--parse-location-property (my-noter--get-containing-heading))))
         (if location
             (my-noter--doc-goto-location location)
           (user-error "No note selected")))
     (user-error "You are inside a different document")))
  (let ((window (my-noter--get-doc-window)))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(defun my-noter-sync-next-note ()
  "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
  (interactive)
  (my-noter--with-selected-notes-window
   "No notes window exists"
   (let ((my-noter--inhibit-location-change-handler t)
         (contents (org-element-contents (my-noter--parse-root)))
         next)

     (my-noter--map-ignore-headings-with-doc-file
      contents t
      (when (and location (< (point) (org-element-property :begin headline)))
        (setq next headline)))

     (if next
         (progn
           (my-noter--doc-goto-location (my-noter--parse-location-property next))
           (my-noter--focus-notes-region (my-noter--make-view-info-for-single-note session next)))
       (user-error "There is no next note"))))
  (select-window (my-noter--get-doc-window)))

(define-minor-mode my-noter-doc-mode
  "Minor mode for the document buffer.
Keymap:
\\{my-noter-doc-mode-map}"
  :keymap `((,(kbd   "i")   . my-noter-add-note)
            (,(kbd "C-i")   . my-noter-add-note-toggle-no-questions)
            (,(kbd "M-i")   . my-noter-insert-precise-note)
            (,(kbd   "q")   . my-noter-kill-session)
            (,(kbd "M-p")   . my-noter-sync-prev-page-or-chapter)
            (,(kbd "M-.")   . my-noter-sync-current-page-or-chapter)
            (,(kbd "M-n")   . my-noter-sync-next-page-or-chapter)
            (,(kbd "C-M-p") . my-noter-sync-prev-note)
            (,(kbd "C-M-.") . my-noter-sync-current-note)
            (,(kbd "C-M-n") . my-noter-sync-next-note))

  (let ((mode-line-segment '(:eval (my-noter--mode-line-text))))
    (if my-noter-doc-mode
        (if (symbolp (car-safe mode-line-format))
            (setq mode-line-format (list mode-line-segment mode-line-format))
          (push mode-line-segment mode-line-format))
      (setq mode-line-format (delete mode-line-segment mode-line-format)))))

(define-minor-mode my-noter-notes-mode
  "Minor mode for the notes buffer.
Keymap:
\\{my-noter-notes-mode-map}"
  :keymap `((,(kbd "M-p")   . my-noter-sync-prev-page-or-chapter)
            (,(kbd "M-.")   . my-noter-sync-current-page-or-chapter)
            (,(kbd "M-n")   . my-noter-sync-next-page-or-chapter)
            (,(kbd "C-M-p") . my-noter-sync-prev-note)
            (,(kbd "C-M-.") . my-noter-sync-current-note)
            (,(kbd "C-M-n") . my-noter-sync-next-note)))

(require 'org)
(require 'org-element)
(require 'doc-view)
(require 'image-mode)

;;; If pdf-tools are installed try to use them,
;;; but fail silently.
(require 'pdf-tools nil t)

(defgroup my-noter nil
  "Interleaving text books since 2015."
  :group 'convenience
  :version "25.1")

;; Redefining `doc-view-kill-proc-and-buffer' as `my-noter-pdf-kill-proc-and-buffer'
;; because this function is obsolete in emacs 25.1 onwards.
(define-obsolete-function-alias 'my-noter--pdf-kill-proc-and-buffer 'my-noter-pdf-kill-proc-and-buffer "1.3.0")
(defun my-noter-pdf-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (derived-mode-p 'doc-view-mode)
    (doc-view-kill-proc))
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (kill-buffer (current-buffer))))

(defcustom my-noter-org-notes-dir-list '("~/org/my-noter_notes" ".")
  "List of directories to look into when opening notes org from a pdf file.

The notes file is assumed to have the exact
same base name as the pdf file (just that the file extension is
.org instead of .pdf).

If the notes org file is not found, it is created in the
directory returned on doing `car' of this list (first element of
the list).

The notes file is searched in order from the first list element
till the last; the search is aborted once the file is found.

If a list element is \".\" or begins with \"./\", that portion is
replaced with the pdf directory name.  e.g. \".\" is interpreted
as \"/pdf/file/dir/\", \"./notes\" is interpreted as
\"/pdf/file/dir/notes/\"."
  :type '(repeat directory)
  :group 'my-noter)

(defvar my-noter-org-buffer nil
  "Org notes buffer name.")

(defvar my-noter-doc-buffer nil
  "Name of PDF buffer associated with `my-noter-org-buffer'.")

(defvar my-noter--window-configuration nil
  "Variable to store the window configuration before my-noter mode was enabled.")

(defun my-noter--current-page (&optional window)
  "Return the page number of the current page.

Use WINDOW for optional window properties passed to `image-mode'."
  (image-mode-window-get 'page window))

;;;###autoload
(define-obsolete-variable-alias 'my-noter--pdf-current-page-fn 'my-noter-pdf-current-page-fn "1.3.0")
(defvar my-noter-pdf-current-page-fn #'my-noter--current-page
  "Function to call to display the current page.")

;;;###autoload
(define-obsolete-variable-alias 'my-noter--pdf-next-page-fn 'my-noter-pdf-next-page-fn "1.3.0")
(defvar my-noter-pdf-next-page-fn #'doc-view-next-page
  "Function to call to display the next page.")

;;;###autoload
(define-obsolete-variable-alias 'my-noter--pdf-previous-page-fn 'my-noter-pdf-previous-page-fn "1.3.0")
(defvar my-noter-pdf-previous-page-fn #'doc-view-previous-page
  "Function to call to display the previous page.")

;;;###autoload
(defun my-noter-goto-doc-page (page)
  "Goto PAGE of document.."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)  (pdf-view-goto-page page))
   ((memq major-mode '(doc-view-mode pdf-view-mode))

    (doc-view-goto-page page))

   (t
    (when (>= (point-max) page)
      (forward-char (- page (point)))
      (recenter)))))


;;;###autoload
(define-obsolete-variable-alias
  'my-noter--pdf-scroll-up-or-next-page-fn 'my-noter-pdf-scroll-up-or-next-page-fn "1.3.0")
(defvar my-noter-pdf-scroll-up-or-next-page-fn #'doc-view-scroll-up-or-next-page
  "Function to call for line/page scrolling in upward direction." )

;;;###autoload
(define-obsolete-variable-alias
  'my-noter--pdf-scroll-down-or-previous-page-fn 'my-noter-pdf-scroll-down-or-previous-page-fn "1.3.0")
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

;; (eval-after-load 'pdf-view ; if/when `pdf-tools' is loaded
;;   '(progn
;;      ;; Function wrapper for the macro `pdf-view-current-page'
;;      (setq my-noter-pdf-next-page-fn #'pdf-view-next-page
;;            my-noter-pdf-previous-page-fn #'pdf-view-previous-page
;;            my-noter-pdf-goto-page-fn #'pdf-view-goto-page
;;            my-noter-pdf-scroll-up-or-next-page-fn #'pdf-view-scroll-up-or-next-page
;;            my-noter-pdf-scroll-down-or-previous-page-fn #'pdf-view-scroll-down-or-previous-page)))

(define-obsolete-variable-alias '*my-noter--page-marker* 'my-noter-page-marker "1.3.0")
(make-variable-buffer-local
 (defvar my-noter-page-marker 0
   "Caches the current page while scrolling"))

(define-obsolete-variable-alias
  '*my-noter--multi-pdf-notes-file* 'my-noter-multi-pdf-notes-file "1.3.0")
(make-variable-buffer-local
 (defvar my-noter-multi-pdf-notes-file nil
   "Indicates if the current Org notes file is a multi-pdf notes file."))



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
        (awhen (org-entry-get nil my-noter-property-doc-file t)
          (setq my-noter-multi-pdf-notes-file t)
          it)))))

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


    (my-noter-pdf-mode 1)
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
  (if my-noter-multi-pdf-notes-file
      (my-noter--goto-parent-headline my-noter-property-doc-file)
    (goto-char (point-min))))

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
  (with-current-buffer my-noter-org-buffer
    (let (point
          (window (get-buffer-window (current-buffer) 'visible)))
      (save-excursion
        (widen)
        (my-noter--goto-search-position)
        ;; (when my-noter-multi-pdf-notes-file
        ;;   ;; only search the current subtree for notes. See. Issue #16
        ;;   (my-noter--narrow-to-subtree t))
        (when (re-search-forward (format "^\[ \t\r\]*\:my-noter_page_note\: %s$"
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

(defun my-noter-go-to-next-page ()
  "Go to the next page in PDF.  Look up for available notes."
  (interactive)
  (funcall my-noter-pdf-next-page-fn)
  (my-noter--go-to-page-note (funcall my-noter-pdf-current-page-fn)))

(defun my-noter-go-to-previous-page ()
  "Go to the previous page in PDF.  Look up for available notes."
  (interactive)
  (funcall my-noter-pdf-previous-page-fn)
  (my-noter--go-to-page-note (funcall my-noter-pdf-current-page-fn)))

(defun my-noter-scroll-up ()
  "Scroll up the PDF.  Look up for available notes."
  (interactive)
  (setq my-noter-page-marker (funcall my-noter-pdf-current-page-fn))
  (funcall my-noter-pdf-scroll-up-or-next-page-fn)
  (unless (= my-noter-page-marker (funcall my-noter-pdf-current-page-fn))
    (my-noter--go-to-page-note (funcall my-noter-pdf-current-page-fn))))

(defun my-noter-scroll-down ()
  "Scroll down the PDF.  Look up for available notes."
  (interactive)
  (setq my-noter-page-marker (funcall my-noter-pdf-current-page-fn))
  (funcall my-noter-pdf-scroll-down-or-previous-page-fn)
  (unless (= my-noter-page-marker (funcall my-noter-pdf-current-page-fn))
    (my-noter--go-to-page-note (funcall my-noter-pdf-current-page-fn))))

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
  (if (not my-noter-multi-pdf-notes-file)
      (goto-char (point-max))
    (prog1
        (my-noter--goto-parent-headline my-noter-property-doc-file)
      (org-end-of-subtree))))

(defun my-noter--insert-heading-respect-content (parent-headline)
  "Create a new heading in the notes buffer.

Adjust the level of the new headline according to the
PARENT-HEADLINE.

Return the position of the newly inserted heading."
  (org-insert-heading-respect-content)
  (let* ((parent-level (if my-noter-multi-pdf-notes-file
                           (org-element-property :level parent-headline)
                         0))
         (change-level (if (> (org-element-property :level (org-element-at-point))
                              (1+ parent-level))
                           #'org-promote
                         #'org-demote)))
    (while (/= (org-element-property :level (org-element-at-point))
               (1+ parent-level))
      (funcall change-level)))
  (point))

(defun my-noter--create-new-note (document page &optional selected-text)
  "Create a new headline for the page PAGE."
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
      (outline-show-entry)

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
it in the notes buffer. When the input is empty, a default title
will be generated.

If there are other notes related to the current location, the
prompt will also suggest them. Depending on the value of the
variable `my-noter-closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `my-noter-insert-precise-note' for more
info).

When you insert into an existing note and have text selected on
the document buffer, the variable `my-noter-insert-selected-text-inside-note'
defines if the text should be inserted inside the note."
  (interactive)
  (PDEBUG "INFO: " precise-info)
  (let* ((page (car (my-noter--doc-approx-location-cons)))
         (position (my-noter--go-to-page-note page))
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
    (if position
        (my-noter--switch-to-org-buffer t position)
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

(define-obsolete-function-alias
  'my-noter--sync-pdf-page-previous 'my-noter-sync-pdf-page-previous "1.3.0")
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

(define-obsolete-function-alias
  'my-noter--sync-pdf-page-next 'my-noter-sync-pdf-page-next "1.3.0")
(defun my-noter-sync-pdf-page-next ()
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
    (when my-noter-multi-pdf-notes-file
      (org-show-subtree))
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

;;;###autoload
(define-obsolete-function-alias
  'my-noter--open-notes-file-for-pdf 'my-noter-open-notes-file-for-pdf "1.3.0")

;;;###autoload
(defun my-noter-open-notes-file-for-pdf ()
  "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)."
  (interactive)
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (let* ((pdf-file-name (buffer-file-name))
           (org-file-name-sans-directory (concat (file-name-base pdf-file-name)
                                                 ".org"))
           org-file-create-dir
           (cnt 0)
           try-org-file-name
           (org-file-name (catch 'break
                            (dolist (dir my-noter-org-notes-dir-list)
                              ;; If dir is "." or begins with "./", replace
                              ;; the "." or "./" with the pdf dir name
                              (setq dir (replace-regexp-in-string
                                         "^\\(\\.$\\|\\./\\).*"
                                         (file-name-directory pdf-file-name)
                                         dir nil nil 1))
                              (when (= cnt 0)
                                ;; In the event the org file is needed to be
                                ;; created, it will be created in the directory
                                ;; listed as the first element in
                                ;; `my-noter-org-notes-dir-list'
                                (setq org-file-create-dir dir))
                              (setq cnt (1+ cnt))
                              (setq try-org-file-name (locate-file
                                                       org-file-name-sans-directory
                                                       (list dir)))
                              (when try-org-file-name
                                ;; return the first match
                                (throw 'break try-org-file-name))))))
      ;; Create the notes org file if it does not exist
      (when (null org-file-name)
        (setq org-file-name (if (null my-noter-org-notes-dir-list)
                                (read-file-name "Path: " "~/")
                              (progn
                                (when (null (file-exists-p org-file-create-dir))
                                  (make-directory org-file-create-dir))
                                (expand-file-name org-file-name-sans-directory
                                                  org-file-create-dir))))

        (with-temp-file org-file-name
          (insert "#+NOTER_DOCUMENT: " pdf-file-name)))
      ;; Open the notes org file and enable `my-noter-mode'
      (find-file org-file-name)
      (my-noter-mode))))

(define-obsolete-function-alias 'my-noter--quit 'my-noter-quit "1.3.0")
(defun my-noter-quit ()
  "Quit my-noter mode."
  (interactive)
  (with-current-buffer my-noter-org-buffer
    (widen)
    (my-noter--goto-search-position)
    (when (my-noter--headlines-available-p)
      (my-noter--sort-notes my-noter-sort-order)
      (org-overview))
    (my-noter-mode 0))
  (my-noter-pdf-kill-proc-and-buffer))

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

;;; My-Noter
;; Minor mode for the org file buffer containing notes

(define-obsolete-variable-alias 'my-noter-map 'my-noter-mode-map "1.3.0")
(defvar my-noter-mode-map (make-sparse-keymap)
  "Keymap while command `my-noter-mode' is active in the org file buffer.")

;;; declare my-noter minor mode as obsolete.
;;;###autoload
(define-minor-mode my-noter-mode
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

\\{my-noter-pdf-mode-map}

Keybindings (org-mode buffer):

\\{my-noter-map}"
  :lighter " ≡"
  :keymap  my-noter-mode-map
  (if my-noter-mode
      (condition-case nil
          (progn
            (setq my-noter-org-buffer (buffer-name))
            (setq my-noter--window-configuration (current-window-configuration))
            (my-noter--open-file (my-noter--select-split-function))
            ;; expand/show all headlines if narrowing is disabled
            (when my-noter-disable-narrowing
              (with-current-buffer my-noter-org-buffer
                (my-noter--goto-search-position)
                (if my-noter-multi-pdf-notes-file
                    (org-show-subtree)
                  (outline-show-all))
                (org-cycle-hide-drawers 'all)))
            (my-noter--go-to-page-note 1)
            (message "My-Noter enabled"))
        ('quit
         (my-noter-mode -1)))
    ;; Disable the corresponding minor mode in the PDF file too.
    (when (and my-noter-doc-buffer
               (get-buffer my-noter-doc-buffer))
      (my-noter--switch-to-doc-buffer)
      (my-noter-pdf-mode -1)
      (setq my-noter-doc-buffer nil))
    (set-window-configuration my-noter--window-configuration)
    (setq my-noter--window-configuration nil)
    (setq my-noter-org-buffer nil)
    (message "My-Noter mode disabled")))

(defun my-noter/get-document-path ()
  "Return path of document visited by this buffer."
  (interactive)
  (let ((buffer-file-name (or buffer-file-name
                              (if (eq major-mode 'nov-mode)
                                  (bound-and-true-p nov-file-name))))
        (document-path
         (or buffer-file-name buffer-file-truename
             (if (eq major-mode 'eww-mode)
                 (eww-current-url)

               (error "This buffer does not seem to be visiting any file"))
             )))

    (if (called-interactively-p 'interactive)
        (PDEBUG "Document path: " document-path))
    document-path))

(defun my-noter (&optional arg)
  "Start `my-noter' session.

There are two modes of operation. You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file -----------------------------------------
This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document). If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With 2 prefix universal arguments ARG, ask for a new document,
even if the current heading annotates one.

With a prefix number ARG:
- Greater than 0: Open the document like `find-file'
-     Equal to 0: Create session with `my-noter-always-create-frame' toggled
-    Less than 0: Open the folder containing the document

- Creating the session from the document ---------------------------------------
This will try to find a notes file in any of the parent folders.
The names it will search for are defined in `my-noter-default-notes-file-names'.
It will also try to find a notes file with the same name as the
document, giving it the maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do. The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one."
  (interactive "P")

  (if (eq major-mode 'org-mode)

      ;; Creating session from notes file.
      (progn
        (when (org-before-first-heading-p)
          ;; TODO: check #+NOTER_DOCUMENT:
          (error "`my-noter' must be issued inside a heading"))

        (if (my-noter--get-or-read-document-property (not (equal arg '(4)))
                                                     (equal arg '(16)))
            (my-noter-mode)
          (error "???"))
        )

    ;; Creating the session from the annotated document
    (let* ((buffer-file-name (or buffer-file-name
                                 (if (eq major-mode 'nov-mode)
                                     (bound-and-true-p nov-file-name))))
           (document-path
            (or buffer-file-name buffer-file-truename
                (if (eq major-mode 'eww-mode)
                    (eww-current-url)

                  (error "This buffer does not seem to be visiting any file"))
                ))

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

           ;; NOTE: This is the path that is actually going to be used, and
           ;; should be the same as `buffer-file-name', but is needed for the
           ;; truename workaround
           (document-used-path (expand-file-name document-name document-directory))

           (search-names (append my-noter-default-notes-file-names (list (concat document-base ".org"))))
           notes-files-annotating     ; List of files annotating document
           notes-files                ; List of found notes files (annotating or not)

           (document-location (my-noter--doc-approx-location)) )

      ;; NOTE(nox): Check the search path
      (dolist (path my-noter-notes-search-path)
        (dolist (name search-names)
          (let ((file-name (expand-file-name name path)))
            (when (file-exists-p file-name)
              (push file-name notes-files)
              (when (my-noter--check-if-document-is-annotated-on-file document-path file-name)
                (push file-name notes-files-annotating))))))

      ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
      ;; and it will end up in the correct order
      (dolist (name search-names)
        (let ((directory (locate-dominating-file document-directory name))
              file)
          (when directory
            (setq file (expand-file-name name directory))
            (unless (member file notes-files) (push file notes-files))
            (when (my-noter--check-if-document-is-annotated-on-file document-path file)
              (push file notes-files-annotating)))))

      (setq search-names (nreverse search-names))

      (when (or arg (not notes-files-annotating))
        (when (or arg (not notes-files))
          (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                   search-names nil nil))
                 list-of-possible-targets
                 target)

            ;; NOTE(nox): Create list of targets from current path
            (catch 'break
              (let ((current-directory document-directory)
                    file-name)
                (while t
                  (setq file-name (expand-file-name notes-file-name current-directory))
                  (when (file-exists-p file-name)
                    (setq file-name (propertize file-name 'display
                                                (concat file-name
                                                        (propertize " -- Exists!"
                                                                    'face '(foreground-color . "green")))))
                    (push file-name list-of-possible-targets)
                    (throw 'break nil))

                  (push file-name list-of-possible-targets)

                  (when (string= current-directory
                                 (setq current-directory
                                       (file-name-directory (directory-file-name current-directory))))
                    (throw 'break nil)))))
            (setq list-of-possible-targets (nreverse list-of-possible-targets))

            ;; NOTE(nox): Create list of targets from search path
            (dolist (path my-noter-notes-search-path)
              (when (file-exists-p path)
                (let ((file-name (expand-file-name notes-file-name path)))
                  (unless (member file-name list-of-possible-targets)
                    (when (file-exists-p file-name)
                      (setq file-name (propertize file-name 'display
                                                  (concat file-name
                                                          (propertize " -- Exists!"
                                                                      'face '(foreground-color . "green"))))))
                    (push file-name list-of-possible-targets)))))

            (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                          nil t))
            (set-text-properties 0 (length target) nil target)
            (unless (file-exists-p target) (write-region "" nil target))

            (setq notes-files (list target))))

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
            (org-entry-put nil my-noter-property-doc-file
                           (file-relative-name document-used-path
                                               (file-name-directory (car notes-files)))))
          (setq notes-files-annotating notes-files)))

      (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
        (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                            notes-files-annotating nil t))))

      (with-current-buffer (find-file-noselect (car notes-files-annotating))
        (org-with-wide-buffer
         (catch 'break
           (goto-char (point-min))
           (while (re-search-forward (org-re-property my-noter-property-doc-file) nil t)
             (when (file-equal-p (expand-file-name (match-string 3)
                                                   (file-name-directory (car notes-files-annotating)))
                                 document-path)
               (let ((my-noter--start-location-override document-location))
                 (my-noter))
               (throw 'break t))))))

      )
    )
)

;;; My-Noter PDF Mode
;; Minor mode for the pdf file buffer associated with the notes

(defvar my-noter-pdf-mode-map (make-sparse-keymap)
  "Keymap while command `my-noter-pdf-mode' is active in the pdf file buffer.")

;;;###autoload
(define-minor-mode my-noter-pdf-mode
  "My-Noter view for the pdf."
  :lighter " ≡"
  :keymap  my-noter-pdf-mode-map
  (when my-noter-pdf-mode
    (progn
      (setq my-noter-doc-buffer (buffer-name)))))

;;; Key-bindings

(define-key my-noter-mode-map (kbd "M-.") #'my-noter-sync-page-current)
(define-key my-noter-mode-map (kbd "M-p") #'my-noter-sync-pdf-page-previous)
(define-key my-noter-mode-map (kbd "M-n") #'my-noter-sync-pdf-page-next)

(define-key my-noter-pdf-mode-map (kbd "n")     #'my-noter-go-to-next-page)
(define-key my-noter-pdf-mode-map (kbd "p")     #'my-noter-go-to-previous-page)
(define-key my-noter-pdf-mode-map (kbd "SPC")   #'my-noter-scroll-up)
(define-key my-noter-pdf-mode-map (kbd "S-SPC") #'my-noter-scroll-down)
(define-key my-noter-pdf-mode-map (kbd "DEL")   #'my-noter-scroll-down)
(define-key my-noter-pdf-mode-map (kbd "i")     #'my-noter-add-note)
(define-key my-noter-pdf-mode-map (kbd "q")     #'my-noter-quit)
(define-key my-noter-pdf-mode-map (kbd "M-.")   #'my-noter-sync-page-current)
(define-key my-noter-pdf-mode-map (kbd "M-p")   #'my-noter-sync-pdf-page-previous)
(define-key my-noter-pdf-mode-map (kbd "M-n")   #'my-noter-sync-pdf-page-next)

(define-key doc-view-mode-map (kbd "i") #'my-noter-open-notes-file-for-pdf)
(when (boundp 'pdf-view-mode-map)
  (define-key pdf-view-mode-map (kbd "i") #'my-noter-open-notes-file-for-pdf))


(provide 'my-noter)

;;; my-noter.el ends here
