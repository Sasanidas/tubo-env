;;; counsel-xgtags.el --- gtags facility for Emacs, integrated with counsel.
;;
;; Copyright (c) 2015 Yang Yingchao
;;
;; Author: Yang Yingchao <yangyingchao@gmail.com>
;; Maintainer: Yang Yingchao <yangyingchao@gmail.com>
;; URL: https://github.com/yangyingchao/counsel-xgtags
;; Version: 1.2
;; Created: 2015-05-20
;; Date: 2015-07-19
;; Package-Requires: ((counsel "1.5.6") (cl-lib "0.5"))
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is based on counsel-xgtags (http://home.tiscali.de/mgidde/source/xgtags.el),
;; written by Marco Gidde, and inspired by counsel-gtags by Syohei YOSHIDA,
;; (https://github.com/syohex/emacs-counsel-gtags).
;;
;;; Installation
;;
;; To use counsel-xgtags copy this file to some place where emacs can find it,
;; if necessary add the path you chose to the load-path variable. In
;; your .emacs add the line
;;
;;	(require 'counsel-xgtags)
;;
;; In any buffer you can now toggle counsel-xgtags-mode by calling the
;; interactive command with same name. Since this is a bit annoying,
;; you might choose to turn it on for some specific modes. For c-mode
;; you can add something like the following snippet to your .emacs
;; file. Other modes provide similar hooks.
;;
;;         (add-hook 'c-mode-common-hook
;;                   (lambda ()
;; 		        (counsel-xgtags-mode 1)))
;;
;; After that you can use the predefined keybindings to query your
;; GLOBAL database. Call 'describe-function RE'S counsel-xgtags-mode' to get an
;; overview of those bindings.
;;
;; TODO: Display function name in counsel-xgtags-buffer, something like:
;; /Users/yyc/Work/mysql-server/sql/rpl_handler.cc
;;     [-] int register_trans_observer (class Trans_observer observer,void p)
;;          return transaction_delegate->add_observer(observer, (st_plugin_int *)p);
;;     [-] int register_binlog_storage_observer (class Binlog_storage_observer observer,void p)
;; int result= binlog_storage_delegate->add_observer(observer, (st_plugin_int *)p);

;;
;;; Code:

;; (require 'easymenu)
(require 'cl-lib)
(require 'counsel)

(defgroup counsel-xgtags nil
  "Using gtags and global for crossrefencing"
  :group 'tools)

(defvar counsel-xgtags--completing-history nil)
;;; Faces

(defface counsel-xgtags-match-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight function name in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-match-selected-face
  '((((class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:foreground "magenta" :bold t))
    (t (:bold t)))
  "Face used to highlight selected function name in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-file-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight file name in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-file-selected-face
  '((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used to highlight selected file name in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-line-number-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight line number in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-line-number-selected-face
  '((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red" :bold t))
    (t (:bold t)))
  "Face used to highlight selected line number in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-line-face
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags-line-selected-face
  '((((class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:foreground "black" :bold t))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *xgtags* buffer."
  :group 'counsel-xgtags)

(defface counsel-xgtags--cmd-line-face
  '((t (:inherit diff-added)))
  "Face used to highlight grep command line when no results."
  :group 'counsel-xgtags)


;;; customization

(defcustom counsel-xgtags-read-only nil
  "*When set to true, new files are opened in read only mode."
  :type 'boolean
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-path-style 'root
  "*How to show paths in the selection buffer."
  :type '(radio (const :tag "Root of the current project" root)
                (const :tag "Relative from the current directory" relative)
                (const :tag "Absolute Path" absolute))
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-auto-update-db nil
  "*Controls if auto update database or not."
  :type 'boolean
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-find-multiple-db nil
  "*Function to return multiple dbs.
When bound to a function this function is called with one
argument, namely the current directory, and should return a list of
directories with GTAGS databases.
All databases are searched one after another."
  :type 'function
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-kill-buffers t
  "*Whether to kill buffer after stack entry is popped.
Be careful: even buffers not opened by counsel-xgtags itself will be killed!"
  :type 'boolean
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-select-buffer-name "*xgtags*"
  "*Name to use as the select buffer."
  :type 'string
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-rootdir nil
  "*Root directory of source tree."
  :type 'string
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-update-interval-second 60
  "*Interval used to decide when to update db.
Tags are updated in `after-save-hook' if this seconds is passed from last
update.
Always update if value of this variable is nil."
  :type '(choice (integer :tag "Update interval seconds")
                 (boolean :tag "Update every time" nil))
  :group 'counsel-xgtags)

(defcustom counsel-xgtags-verbose nil
  "Verbose output or not."
  :type 'boolean
  :group 'counsel-xgtags
  )



(defconst counsel-xgtags--symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")
(defconst counsel-xgtags--definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")
(defconst counsel-xgtags--tag-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([^\n]+\\)"
  "Regex matching the current output line for a tag.")
(defconst counsel-xgtags--file-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\n]+\\)"
  "Regex matching the current output line for a tag.")

(defvar counsel-xgtags-minor-mode-text " counsel-xgtags"
  "Text to be shown in the mode line.")

(defvar counsel-xgtags-select-mode-name "xgtags-select"
  "Text to be shown in the mode line.")

(defvar counsel-xgtags-select-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-m" 'counsel-xgtags-select-tag-near-point)
    (define-key keymap "\e." 'counsel-xgtags-select-tag-near-point)
    (define-key keymap (kbd "SPC") 'counsel-xgtags--activate)
    (define-key keymap (kbd "g") 'counsel-xgtags-refresh)
    keymap)
  "Keymap used in counsel-xgtags select mode.")

(defvar counsel-xgtags-mode-map
  (let ((keymap (make-sparse-keymap))
        (sub-keymap (make-sparse-keymap)))
    (define-key keymap "\C-cs" sub-keymap)
    (define-key sub-keymap "c" 'counsel-xgtags-find-reference)
    (define-key sub-keymap "s" 'counsel-xgtags-find-symbol)
    (define-key sub-keymap "g" 'counsel-xgtags-find-pattern)
    (define-key sub-keymap "h" 'counsel-xgtags--activate)
    (define-key sub-keymap "n" 'counsel-xgtags-select-next-tag)
    (define-key sub-keymap "p" 'counsel-xgtags-select-prev-tag)
    (define-key sub-keymap "u" 'counsel-xgtags-pop-stack)
    (define-key sub-keymap "U" 'counsel-xgtags-update-tags)
    (define-key sub-keymap "f" 'counsel-xgtags-find-file)
    (define-key sub-keymap "x" 'counsel-xgtags-switch-to-buffer)
    (define-key sub-keymap "R" 'counsel-xgtags-query-replace-regexp)
    (define-key sub-keymap "R" 'counsel-xgtags--activate)
    keymap)
  "Keymap used in counsel-xgtags minor mode.")

(defvar counsel-xgtags--map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<down>") 'counsel-goto-next-file)
    (define-key map (kbd "M-<up>")   'counsel-goto-precedent-file)
    (define-key map (kbd "C-x C-s")  'counsel-xgtags--run-save-buffer)
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar counsel-xgtags--complete-cmd nil "Nil.")
(defvar counsel-xgtags--complete-pfx nil "Nil.")
(defvar counsel-xgtags--last-update-time 0 "Last update time.")
(defvar counsel-xgtags--tags nil
  "List of current tags.")
(defvar counsel-xgtags--selected-tag nil
  "The currently selected tag.")
(defvar counsel-xgtags--stack nil
  "Stack for tag browsing.")


;;; macros

(defmacro with-xgtags-environment (&rest body)
  "Execute BODY with proper environment."
  `(let ((process-environment (copy-alist process-environment)))
     (when counsel-xgtags-rootdir
       (setenv "GTAGSROOT" counsel-xgtags-rootdir))
     ,@body))
(put 'with-xgtags-environment 'lisp-indent-function 0)

(defmacro* with-xgtags-buffer ((&key buffer save-excursion (read-only t))
                               &rest body)
  "Evaluate in counsel-xgtags buffer."
  (let ((buffer-var (or buffer (gensym "buffer-"))))
    `(let ((,buffer-var (counsel-xgtags--get-buffer)))
       (,(if save-excursion 'save-excursion 'progn)
        (set-buffer ,buffer-var)
        (let ((buffer-read-only ,read-only))
          ,@body)))))
(put 'with-xgtags-buffer 'lisp-indent-function 1)


;;; utilities

(defun counsel-xgtags--list-sans-nil (&rest args)
  "Build a list from ARGS but leave nils out."
  (let ((result nil))
    (dolist (arg args (nreverse result))
      (when arg
        (push arg result)))))

(defun counsel-xgtags--token-at-point ()
  "Return a default tag to search for, based on the text at point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (car (reverse (split-string (buffer-substring-no-properties (car bounds) (cdr bounds)) ":"))))))

(defun counsel-xgtags--file-at-point ()
  "Return a default tag to search for, based on the text at point."
  (or
   (counsel-xgtags-headerp)
   (cond
    ((looking-at "[0-9A-Za-z_\.]")
     (while (looking-at "[0-9A-Za-z_\.]")
       (forward-char -1))
     (forward-char 1)))
   (when (looking-at "[0-9A-Za-z_\.]+")
     (match-string-no-properties 0))))

(defun counsel-xgtags--function-p ()
  "Is it a function?"
  (save-excursion
    (while (and (not (eolp)) (looking-at "[0-9A-Za-z_]"))
      (forward-char 1))
    (while (and (not (eolp)) (looking-at "[ \t]"))
      (forward-char 1))
    (looking-at "(")))

(defun counsel-xgtags--definition-p ()
  "Is it a definition?"
  (save-excursion
    (if (or (and (string-match "\.java$" buffer-file-name)
                 (looking-at "[^(]+([^)]*)[ \t]*{"))
            (bolp))
        t
      (forward-word -1)
      (cond
       ((looking-at "define")
        (forward-char -1)
        (while (and (not (bolp)) (looking-at "[ \t]"))
          (forward-char -1))
        (and (bolp) (looking-at "#")))
       ((looking-at "ENTRY\\|ALTENTRY")
        (bolp))))))

(defun counsel-xgtags--insert-with-face (string face)
  "Insert STRING with FACE into buffer."
  (let ((start (point)))
    (insert string)
    (put-text-property start (point) 'face face)))

(defun counsel-xgtags--update-minor-mode-text ()
  "Update text in mode-line."
  (let ((length (length counsel-xgtags--stack)))
    (if (zerop length)
        (setq counsel-xgtags-minor-mode-text " counsel-xgtags")
      (setq counsel-xgtags-minor-mode-text (format " counsel-xgtags[%d]" length)))))


;;; handling tags and the tags list

(defstruct counsel-xgtags--tag
  query abs-file file line match)

(defun counsel-xgtags--make-tag (&rest args)
  "Generate a TAG with ARGS."
  (let* ((tag (apply 'make-counsel-xgtags--tag args))
         (abs-file (expand-file-name (counsel-xgtags--tag-file tag))))
    (setf (counsel-xgtags--tag-abs-file tag) abs-file)
    tag))

(defun counsel-xgtags--tag-position (tag)
  "Return the position index of TAG in the tag-list."
  (let ((pos 0)
        (found nil)
        (tags counsel-xgtags--tags))
    (while (and tags (not (setq found (eq (car tags) tag))))
      (setq pos (1+ pos))
      (setq tags (cdr tags)))
    (when found
      pos)))

(defun* counsel-xgtags--next-tag (tag &optional (n 1))
  "Returns the N-th tag that follows TAG in the tag list or nil."
  (assert tag nil "No tag in call to counsel-xgtags--next-tag")
  (let ((pos (counsel-xgtags--tag-position tag)))
    (when (and (>= pos 0) (>= (+ pos n) 0))
      (nth (+ pos n) counsel-xgtags--tags))))

(defun counsel-xgtags--insert-tags (tags)
  "Insert TAGS into current buffer.
This function recieves a list of tags, erases the current buffer
and then inserts the tags nicely."
  (erase-buffer)
  (let ((current-file nil))
    (dolist (tag tags)
      (let ((file (counsel-xgtags--tag-file tag)))
        (unless (equal current-file file)
          (when current-file
            (insert "\n"))
          (setq current-file file)
          (counsel-xgtags--insert-tag-file tag)
          (insert "\n")))
      (counsel-xgtags--insert-tag tag)
      (insert "\n"))))

(defun counsel-xgtags--insert-tag-file (tag)
  "Insert the file entry TAG into the current buffer."
  (counsel-xgtags--insert-with-face (counsel-xgtags--tag-file tag) 'counsel-xgtags-file-face))

(defun counsel-xgtags--insert-tag (tag)
  "Insert a single tag TAG at point in the current buffer.a"
  (let ((start (point))
        (query (counsel-xgtags--tag-query tag))
        (line (counsel-xgtags--tag-line tag))
        (match (counsel-xgtags--tag-match tag))
        (selected-p (eq tag counsel-xgtags--selected-tag)))
    (counsel-xgtags--insert-with-face query
                              (if selected-p
                                  'counsel-xgtags-match-selected-face
                                'counsel-xgtags-match-face))
    (insert "[")
    (counsel-xgtags--insert-with-face (number-to-string line)
                              (if selected-p
                                  'counsel-xgtags-line-number-selected-face
                                'counsel-xgtags-line-number-face))
    (when match
      (insert "]\t\t")
      (counsel-xgtags--insert-with-face match (if selected-p
                                          'counsel-xgtags-line-selected-face
                                        'counsel-xgtags-line-face)))
    (put-text-property start (point) 'counsel-xgtags-tag tag)))

(defun counsel-xgtags--update-tag (tag)
  "Search and update TAG.
This function searches the tag TAG in the current buffer and replaces the
current representation with an updated one."
  (let ((region (counsel-xgtags--find-tag-region tag)))
    (when region
      (delete-region (car region) (cdr region))
      (goto-char (car region))
      (counsel-xgtags--insert-tag tag))))

(defun counsel-xgtags--find-tag-region (tag)
  "If TAG is found in the current buffer this functions returns a
list with the start and end positions, otherwise it returns nil"
  (when tag
    (let ((start (text-property-any (point-min) (point-max) 'counsel-xgtags-tag tag)))
      (when start
        (cons start (next-single-property-change start 'counsel-xgtags-tag))))))

(defun counsel-xgtags--select-tag (tag &optional update)
  "Make TAG the selected tag.
If UPDATE is not nil, try to find the previous selected tag and TAG in the
current buffer, update their representation and move point to the beginning of
TAG."
  (let ((old-sel counsel-xgtags--selected-tag))
    (setq counsel-xgtags--selected-tag tag)
    (when update
      (counsel-xgtags--update-tag old-sel)
      (counsel-xgtags--update-tag counsel-xgtags--selected-tag)
      (goto-char (or (car (counsel-xgtags--find-tag-region counsel-xgtags--selected-tag)) (point-min)))
      (when (get-buffer-window (current-buffer))
        (set-window-point (get-buffer-window (current-buffer)) (point))))))

(defun counsel-xgtags--find-tag-near-point (&optional backwards)
  "Find the next selectable tag and move point to its beginning. If
there is none at the current line, step a line forward or backward to
find one."
  (beginning-of-line)
  (while (when (not (get-text-property (point) 'counsel-xgtags-tag))
           (zerop (forward-line (and backwards -1)))))
  (get-text-property (point) 'counsel-xgtags-tag))

(defun counsel-xgtags--follow-tag (tag)
  "Jump to the place that TAG points to."
  (interactive)
  (find-file (counsel-xgtags--tag-abs-file tag))
  (setq buffer-read-only (or buffer-read-only counsel-xgtags-read-only))
  (counsel-xgtags-mode 1)
  (goto-char (point-min))
  (forward-line (counsel-xgtags--tag-line tag))
  (let ((match (counsel-xgtags--tag-query tag))
        (found nil)
        (lines 0))
    (while (and (not found) (< lines 5))
      (let ((start (save-excursion (forward-line (- lines))
                                   (point)))
            (end (save-excursion (forward-line lines)
                                 (end-of-line)
                                 (point))))
        (save-excursion
          (goto-char start)
          (setq found (search-forward-regexp match end t))))
      (setq lines (1+ lines)))
    (when found
      (goto-char (match-beginning 0))
      (recenter)
      )))

(defun counsel-xgtags--map-tags (func)
  "Maps over all tags in the *xgtags* buffer, jumps to the tag and
funcalls FUNC with the match as argument."
  (mapc (lambda (tag)
          (with-xgtags-buffer (:read-only nil)
            (counsel-xgtags--select-and-follow-tag tag)
            (funcall func (counsel-xgtags--tag-query tag))))
        counsel-xgtags--tags))

(defun counsel-xgtags--test-map-tags ()
  (interactive)
  (counsel-xgtags--map-tags
   (lambda (match)
     (message "foo: %s" match)
     (when (search-forward-regexp match
                                  (save-excursion (end-of-line) (point))
                                  t)
       (goto-char (match-beginning 0))
       (set-mark (match-end 0))
       (unless (y-or-n-p "More? ")
         (return-from counsel-xgtags--test-map-tags))))))

;;; handling the context and the context stack

(defstruct counsel-xgtags--context
  type tagname buffer point tags selected-tag)

(defun counsel-xgtags--make-context (tagname &optional option)
  "Create a context object with OPTION."
  (make-counsel-xgtags--context :type option
                             :tagname tagname
                             :buffer (current-buffer)
                             :point (point-marker)
                             :tags counsel-xgtags--tags
                             :selected-tag counsel-xgtags--selected-tag))

(defun counsel-xgtags--stacked-p (buffer)
  "If BUFFER exists on the counsel-xgtags stack."
  (memq buffer (mapcar 'counsel-xgtags--context-buffer counsel-xgtags--stack)))

(defun counsel-xgtags--push-context (tagname &optional option)
  (first (push (counsel-xgtags--make-context tagname option) counsel-xgtags--stack)))

(defun counsel-xgtags--pop-context ()
  "Pop context from stack and return it."
  (pop counsel-xgtags--stack))


;;; handling the selection buffer

(defun counsel-xgtags--get-buffer ()
  "Return the selection buffer.
If it was kill recreate and fill it with the previous query results."
  (let ((buffer (get-buffer counsel-xgtags-select-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create counsel-xgtags-select-buffer-name))
      ;; using with-xgtags-buffer here is not possible because it uses
      ;; counsel-xgtags--get-buffer itself
      (with-current-buffer buffer
      ;; (save-excursion
      ;;   (set-buffer buffer)
        (when counsel-xgtags--stack
          (counsel-xgtags--insert-tags counsel-xgtags--tags))
        (counsel-xgtags-select-mode)))
    buffer))

(defun counsel-xgtags--select-update-mode-text (tag)
  (let ((pos (counsel-xgtags--tag-position tag)))
    (if pos
        (setq mode-name (format "%s[%d/%d]"
                                counsel-xgtags-select-mode-name
                                (1+ pos) (length counsel-xgtags--tags)))
      (setq mode-name (format "%s[%d]"
                              counsel-xgtags-select-mode-name
                              (length counsel-xgtags--tags))))))

(defun counsel-xgtags--update-buffer (context)
  (let ((tags (counsel-xgtags--context-tags context))
        (selected-tag (counsel-xgtags--context-selected-tag context)))
    (counsel-xgtags--update-minor-mode-text)
    (when tags
      (with-xgtags-buffer (:save-excursion t :read-only nil)
        (setq counsel-xgtags--tags tags)
        (counsel-xgtags--insert-tags tags)
        (counsel-xgtags--select-tag selected-tag t)
        (counsel-xgtags--select-update-mode-text selected-tag)))))

(defun counsel-xgtags--select-and-follow-tag (tag)
  "Select TAG and follow the link."
  (when tag
    (with-xgtags-buffer (:read-only nil)
      (counsel-xgtags--select-tag tag t)
      (counsel-xgtags--select-update-mode-text tag))
    (counsel-xgtags--follow-tag tag)))


;;; options of the GLOBAL program

(defconst counsel-xgtags--gtags-options-alist
  '(
    (definition . "%s: Definition not found")
    (file       . "%s: file not found")
    (path       . "%s: path not found")
    (pattern    . "%s: pattern not found")
    (reference  . "%s: reference not found")
    (symbol     . "%s: symbol not found")
    ))

(defconst counsel-xgtags--prompt-alist
  '(
    (definition . "Find Definition: ")
    (path       . "Find File: ")
    (pattern    . "Find Pattern: ")
    (reference  . "Find Reference: ")
    (symbol     . "Find Symbol: ")
    ))

(defconst counsel-xgtags--search-option-alist
  '(
    (definition . "-d")
    (file       . "-f")
    (path       . "-Poa")
    (pattern    . "-g")
    (reference  . "-r")
    (symbol     . "-s")
    ))

(defun counsel-xgtags--option-string (symbol)
  (when symbol
    (assert (assoc symbol counsel-xgtags--search-option-alist) t
            "Unknown gtags option symbol: %s" symbol)
    (cdr (assoc symbol counsel-xgtags--search-option-alist))))

(defun counsel-xgtags--option-error-msg (symbol)
  (let ((opt (assoc symbol counsel-xgtags--gtags-options-alist)))
    (if (and opt (cdr opt))
        (cdr opt)
      "%s: tag not found")))


;;; GLOBAL process handling

(defun counsel-xgtags--tag-directory ()
  (with-temp-buffer
    (if (getenv "GTAGSROOT")
        (getenv "GTAGSROOT")
      (unless (zerop (process-file "global" nil t nil "-p"))
        (error "GTAGS not found"))
      (goto-char (point-min))
      (when (looking-at "^\\([^\r\n]+\\)")
        (let ((tag-path (match-string-no-properties 1)))
          (file-name-as-directory tag-path))))))

(defun counsel-xgtags--base-directory ()
  (let ((dir (cl-case counsel-xgtags-path-style
               (root (counsel-xgtags--tag-directory))
               (otherwise default-directory)))
        (remote (file-remote-p default-directory)))
    (if (and remote (not (file-remote-p dir)))
        (concat remote dir)
      dir)))

(defun counsel-xgtags--call-global (buffer-dir option tagname)
  "In all BUFFER-DIR, call global commands with OPTION for TAGNAME."
  (let ((tags nil))
    (counsel-xgtags--do-in-all-directories
     buffer-dir
     (lambda (dir)
       (let ((counsel-xgtags-rootdir (and dir (file-name-as-directory dir))))
         (with-xgtags-environment
           (with-temp-buffer
             (let ((default-directory (counsel-xgtags--base-directory))
                   (args (counsel-xgtags--list-sans-nil
                          "--cxref"
                          (counsel-xgtags--option-string option)
                          (if (eq counsel-xgtags-path-style 'absolute)
                              "--absolute")
                          tagname)))
               (PDEBUG "Starting global process in directory `%s'" default-directory)
               (PDEBUG (concat "Command line used was:\n\n"
                                 ">>> "
                                 (concat "global" (mapconcat 'identity args " "))
                                 "\n\n"))
               (if (zerop (apply #'call-process "global" nil t nil
                                 args))
                   (setq tags (append tags (counsel-xgtags--collect-tags-in-buffer)))
                 (message (buffer-substring (point-min)(1- (point-max)))))))))))
    tags))

(defun counsel-xgtags--do-in-all-directories (buffer-dir func)
  (let ((dirs (if counsel-xgtags-find-multiple-db
                  (funcall counsel-xgtags-find-multiple-db buffer-dir)
                (list counsel-xgtags-rootdir))))
    (dolist (dir dirs)
      (funcall func dir))))

(defun counsel-xgtags--collect-tags-in-buffer ()
  "This function searches the current buffer for tag items and returns
a list with those."

  (PDEBUG "In directory `%s'" default-directory)
  (PDEBUG (concat "global output was:\n\n"
                    ">>> "
                    (buffer-string)
                    "\n\n"))
  (save-excursion
    (goto-char (point-min))
    (let ((tags nil))
      (while (not (eobp))
        (cond
         ((looking-at counsel-xgtags--tag-regexp)
          (let* ((query (match-string-no-properties 1))
                 (line (string-to-number (match-string-no-properties 2)))
                 (file (match-string-no-properties 3))
                 (match (match-string-no-properties 4)))
            (push (counsel-xgtags--make-tag :query query
                                    :file file
                                    :line line
                                    :match match) tags)))
         ((looking-at counsel-xgtags--file-regexp)
          (let* ((query (match-string-no-properties 1))
                 (line (string-to-number (match-string-no-properties 2)))
                 (file (match-string-no-properties 3)))
            (push (counsel-xgtags--make-tag :query query
                                    :file file
                                    :line line) tags))))
        (forward-line))
      (nreverse tags))))


;;; navigating the selection buffer

(defun counsel-xgtags--select-next-prev-tag (arg)
  "Select the next or previous tag in the previous select buffer."
  (let ((tag (counsel-xgtags--next-tag counsel-xgtags--selected-tag arg)))
    (assert tag nil "The %s of the *xgtags* buffer has been reached"
            (if (> arg 0) "end" "beginning"))
    (counsel-xgtags--select-and-follow-tag tag)))

(defun counsel-xgtags-select-next-tag (&optional arg)
  "Select the next tag in the previous select buffer."
  (interactive "p")
  (counsel-xgtags--select-next-prev-tag arg))

(defun counsel-xgtags-select-prev-tag (&optional arg)
  "Select the previous tag in the previous select buffer."
  (interactive "p")
  (counsel-xgtags--select-next-prev-tag (- arg)))

;;; finding and selecting tags

(defun counsel-xgtags--goto-tag (tagname &optional option)
  "Go find and goto tag (TAGNAME) with OPTION."
  (if tagname
      (let* ((window (selected-window))
             (file-name (buffer-file-name))
             (buffer-dir (and file-name (file-name-directory file-name)))
             (tags (counsel-xgtags--call-global buffer-dir option tagname))
             (num-tags (length tags))
             (type (if counsel-xgtags--stack (counsel-xgtags--context-type (car counsel-xgtags--stack)) nil)))
        (if (= num-tags 0)
            (error (counsel-xgtags--option-error-msg option) tagname)
          (counsel-xgtags--push-context tagname option)
          (counsel-xgtags--update-minor-mode-text)
          (with-xgtags-buffer (:save-excursion t :read-only nil)
            (setq counsel-xgtags--tags tags)
            (counsel-xgtags--insert-tags tags))

          (when (eq type 'file) ;; update selected-tag if needed.
            (if (and counsel-xgtags--selected-tag
                     (string= (counsel-xgtags--tag-abs-file counsel-xgtags--selected-tag)
                              (counsel-xgtags--tag-abs-file (first tags))))
                (let ((old-file (counsel-xgtags--tag-abs-file counsel-xgtags--selected-tag)))
                  (catch 'BREAK
                    (dolist (tag tags)
                      (when (and (string= old-file
                                          (counsel-xgtags--tag-abs-file tag))
                                 (string= (counsel-xgtags--tag-query counsel-xgtags--selected-tag)
                                          (counsel-xgtags--tag-query tag)))
                        (counsel-xgtags--select-tag tag)
                        (throw 'BREAK t)))))))

          (if (= num-tags 1)
              (counsel-xgtags--select-and-follow-tag (first tags))
            ;; (unless (eq type 'file)
            ;;   (counsel-xgtags--select-and-follow-tag (first tags)))
            (counsel-xgtags--activate (buffer-name)))))
    (message "No tag provided...")))

(defun counsel-xgtags-select-tag-near-point ()
  "Select the tag near point and follow it."
  (interactive)
  (counsel-xgtags--select-and-follow-tag (counsel-xgtags--find-tag-near-point)))

(defun counsel-xgtags-refresh ()
  "Refresh current display."
  (interactive)
  (let ((context (counsel-xgtags--pop-context)))
    (assert context nil "The tags stack is empty")
    (let ((type (counsel-xgtags--context-type context))
          (tagname (counsel-xgtags--context-tagname context)))
      (counsel-xgtags--update-minor-mode-text)
      (counsel-xgtags--goto-tag tagname type))))


;;; interactive commands

(defun counsel-xgtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (unless counsel-xgtags-rootdir
    (with-temp-buffer
      (setq counsel-xgtags-rootdir
            (if (zerop (call-process "global" nil t nil "-pr"))
                (file-name-as-directory (buffer-substring (point-min)
                                                          (1- (point-max))))
              default-directory))))
  (let ((input (read-file-name "Visit root directory: "
                               counsel-xgtags-rootdir counsel-xgtags-rootdir t)))
    (unless (equal "" input)
      (assert (file-directory-p input) t "%s is not directory" input)
      (setq counsel-xgtags-rootdir (expand-file-name input)))))

(defun counsel-xgtags--construct-options (type)
  (let ((find-file-p (eq type 'path))
        (options '("global")))
    (unless find-file-p
      (push "--result=grep" options))
    (if (assoc-default type counsel-xgtags--search-option-alist)
        (push (assoc-default type counsel-xgtags--search-option-alist) options))
    (if counsel-xgtags-ignore-case
        (push "-i" options))
    (when (and current-prefix-arg (not find-file-p))
      (push "-l" options))
    (when (getenv "GTAGSLIBPATH")
      (push "-T" options))
    (push "-c" options)
    options))

(defun counsel-xgtags--complete-candidates (input &rest args)
  (if (< (length input) 3)
      (ivy-more-chars)
    (let ((cmd-options (counsel-xgtags--construct-options 'definition))
          ;; (cmd-options (counsel-gtags--command-options 'definition))
          )
      (push input cmd-options)
      (push "-c" cmd-options)
      (counsel--async-command
       (mapconcat #'identity (reverse cmd-options) " ")
       ;; (mapconcat #'identity (cons "global" (reverse cmd-options)) " ")
       )
      nil))
  nil)

(defun counsel-xgtags--read-tagname (type &optional tagname)
  (let ((prompt (assoc-default type counsel-xgtags--prompt-alist)))

    (ivy-read prompt #'counsel-xgtags--complete-candidates
              :initial-input tagname
              :dynamic-collection t
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-gtags)))


(defun* counsel-xgtags--find-with (&key
                           (type 'definition)
                           (get-token 'counsel-xgtags--token-at-point)
                           (dont-confirm t))
  (let* ((tagname (funcall get-token))
         (input (if (and dont-confirm tagname) tagname
                  (counsel-xgtags--read-tagname type tagname))))
    (counsel-xgtags--goto-tag input type)))


(defun counsel-xgtags-query-replace-regexp (to-string)
  "Run over the current *xgtags* buffer and to `query-replace-regexp' for each tag."
  (interactive
   (list (read-from-minibuffer (format "Replace <%s> with: "
                                       (counsel-xgtags--tag-query
                                        (save-excursion
                                          (set-buffer (counsel-xgtags--get-buffer))
                                          (get-text-property (point) 'counsel-xgtags-tag))))
                               nil nil nil
                               query-replace-to-history-variable nil t)))
  (counsel-xgtags--map-tags
   (lambda (match)
     (query-replace-regexp match to-string nil
                           (point)
                           (save-excursion (end-of-line) (point))))))

(defun counsel-xgtags--switch-buffer (other-window jump-to-start-p)
  (with-xgtags-buffer (:buffer buffer)
    (when jump-to-start-p
      (goto-char (point-min)))
    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun counsel-xgtags-switch-to-buffer (&optional jump-to-start-p)
  (interactive "P")
  (counsel-xgtags--switch-buffer nil jump-to-start-p))

(defun counsel-xgtags-switch-to-buffer-other-window (&optional jump-to-start-p)
  (interactive "P")
  (counsel-xgtags--switch-buffer t jump-to-start-p))


;;; definition and support for the minor mode

;; (easy-menu-define counsel-xgtags-menu
;;   counsel-xgtags-mode-map
;;   "xgtags menu"
;;   '("XGtags"
;;     [ "Find Tag" counsel-xgtags-find-definition t ]
;;     [ "Find Tag Reference" counsel-xgtags-find-reference t ]
;;     [ "Find Symbol" counsel-xgtags-find-symbol t ]
;;     [ "Find Pattern" counsel-xgtags-find-pattern t ]
;;     "-----------"
;;     [ "Find Previous Tag" counsel-xgtags-select-prev-tag t ]
;;     [ "Find Next Tag" counsel-xgtags-select-next-tag t ]
;;     [ "Query-replace Tag" counsel-xgtags-query-replace-regexp t ]
;;     "-----------"
;;     [ "Find File" counsel-xgtags-find-file t ]
;;     "-----------"
;;     [ "Parse File" counsel-xgtags-parse-file t ]
;;     [ "Visit Tags Directory" counsel-xgtags-visit-rootdir t ]))

;;;###autoload
(define-minor-mode counsel-xgtags-mode
  "Toggle counsel-xgtags-mode, a minor mode for browsing source code using GLOBAL.

Input tag name and move to the definition.
	\\[xgtags-find-definition]
Input tag name and move to the referenced point.
	\\[xgtags-find-reference]
Input symbol and move to the locations.
	\\[xgtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[xgtags-find-pattern]
Input pattern and move to the top of the file.
	\\[xgtags-find-file]
Get the expression as a tagname around here and move there.
	\\[xgtags-find-definition-from-here]
Move to previous point on the stack.
	\\[xgtags-pop-stack]

Key definitions:
\\{xgtags-mode-map}
Turning on counsel-xgtags-mode calls the value of the variable `xgtags-mode-hook'
with no args, if that value is non-nil."
  :group      'counsel-xgtags
  :init-value nil
  :global     nil
  :keymap     counsel-xgtags-mode-map
  :lighter    counsel-xgtags-minor-mode-text

  (if counsel-xgtags-mode
      (when counsel-xgtags-auto-update-db
        (add-hook 'after-save-hook 'counsel-xgtags-update-tags nil t))
    (when counsel-xgtags-auto-update-db
      (remove-hook 'after-save-hook 'counsel-xgtags-update-tags t))))

;;; definition and support for the selection mode

(define-derived-mode counsel-xgtags-select-mode fundamental-mode counsel-xgtags-select-mode-name
  "Major mode for choosing a tag from tags list.

Select a tag in tags list and move there.
	\\[xgtags-select-tag-near-point]
Move to previous point on the stack.
	\\[xgtags-pop-stack]

Key definitions:
\\{xgtags-select-mode-map}
Turning on counsel-xgtags-select mode calls the value of the variable
`xgtags-select-mode-hook' with no args, if that value is non-nil."
  (setq buffer-read-only t
        truncate-lines t)
  (goto-char (point-min)))

 ;; misc
(defun counsel-xgtags-headerp ()
  "Return header file or nil."
  (save-excursion
    (beginning-of-line)
    (and (looking-at
          "^\\s-*#\\s-*\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")
         (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
(defun counsel-xgtags--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (file-truename buffile))))



;; Database related functions...
(defun counsel-xgtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c"
                 (concat "cd " (counsel-xgtags--tag-directory) " ; gtags --single-update "
                         filename )))

(defun counsel-xgtags-update-current-file()
  (interactive)
  (let ((filename (replace-regexp-in-string (counsel-xgtags--tag-directory) "."
                                            (buffer-file-name (current-buffer)))) )
    (counsel-xgtags-update-single filename)
    (message "Gtags updated for %s" filename)))

(defun counsel-xgtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when counsel-xgtags-mode
    (when (counsel-xgtags--tag-directory)
      (counsel-xgtags-update-current-file))))

(defun counsel-xgtags--read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst counsel-xgtags--how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))

(defun counsel-xgtags--update-tags-command (how-to)
  (cl-case how-to
    (entire-update '("global" "-u"))
    (generate-other-directory (list "gtags" (counsel-xgtags--read-tag-directory)))
    (single-update (list "global" "--single-update" (counsel-xgtags--real-file-name)))))

(defun counsel-xgtags--update-tags-p (how-to interactive-p current-time)
  (and
   (= (call-process "global" nil nil nil "-p") 0)
   (or interactive-p
             (and (eq how-to 'single-update)
                  (buffer-file-name)
                  (or (not counsel-xgtags-update-interval-second)
                      (>= (- current-time counsel-xgtags--last-update-time)
                          counsel-xgtags-update-interval-second))))))

(defmacro counsel-xgtags--make-gtags-sentinel (action)
  `(lambda (process _event)
    (when (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process))
          (message "Success: %s TAGS" ,action)
        (message "Failed: %s TAGS(%d)" ,action (process-exit-status process))))))


;; Functions can be autoloaded...

;;;###autoload
(defun counsel-xgtags-find-definition ()
  "Input tag name and move to the definition."
  (interactive)
  (counsel-xgtags--find-with))

;;;###autoload
(defun counsel-xgtags-find-reference ()
  "Input tag name and move to the referenced point."
  (interactive)
  (counsel-xgtags--find-with :type 'reference))

;;;###autoload
(defun counsel-xgtags-find-symbol ()
  "Find tag that is a reference without a definition."
  (interactive)
  (counsel-xgtags--find-with :type 'symbol))

;;;###autoload
(defun counsel-xgtags-find-pattern ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (counsel-xgtags--find-with :type 'pattern))

;;;###autoload
(defun counsel-xgtags-find-file ()
  "Input pattern and move to the top of the file."
  (interactive)
  (counsel-xgtags--find-with :type 'path))

;;;###autoload
(defun counsel-xgtags-find-header ()
  "Open header file under cursor."
  (interactive)
  (if (counsel-xgtags-headerp)
      (counsel-xgtags--find-with
       :type 'path
       :get-token (lambda ()(counsel-xgtags-headerp))
       :dont-confirm t)
    (error "Not a header file...")))

;;;###autoload
(defun counsel-xgtags-find-definition-from-here ()
  "Get the expression as a tagname around here and move there."
  (interactive)
  (let ((tagname (counsel-xgtags--token-at-point)))
    (when tagname
      (counsel-xgtags--goto-tag tagname (cond ((not (counsel-xgtags--function-p)) 'symbol)
                                      ((counsel-xgtags--definition-p) 'reference)
                                      (t nil))))))
;;;###autoload
(defun counsel-xgtags-pop-stack ()
  "Move to previous point on the stack."
  (interactive)
  (let ((delete (and counsel-xgtags-kill-buffers
                     (not (counsel-xgtags--stacked-p (current-buffer)))))
        (context (counsel-xgtags--pop-context)))
    (assert context nil "The tags stack is empty")
    (when delete
      (kill-buffer (current-buffer)))
    (counsel-xgtags--update-buffer context)
    (switch-to-buffer (counsel-xgtags--context-buffer context))
    (goto-char (counsel-xgtags--context-point context))))

;;;###autoload
(require 'which-func)
(defun counsel-xgtags-parse-file (&optional filename)
  "Input FILENAME, parse it and show object list."
  (interactive)
  (let* ((input (or filename
                   (read-file-name "Parse file: "
                                   nil nil t
                                   (file-name-nondirectory buffer-file-name))))
         (fn (which-function))
         (counsel-xgtags--selected-tag
          (if fn
              (counsel-xgtags--make-tag
               :query (which-function)
               :line 1
               :file input))))
    (assert (not (equal input "")) nil "No file specified")
    (counsel-xgtags--goto-tag (expand-file-name input) 'file)))

;;;###autoload
(defun counsel-xgtags-update-tags ()
  "Update TAG file.
Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'."
  (interactive)
  (let ((how-to (counsel-xgtags--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (counsel-xgtags--update-tags-p how-to interactive-p current-time)
      (let* ((cmds (counsel-xgtags--update-tags-command how-to))
             (proc (apply 'start-file-process "xgtags-update-tag" nil cmds)))
        (if (not proc)
            (message "Failed: %s" (mapconcat 'identity cmds " "))
          (PDEBUG "Starting update xgtags db with:" cmds)
          (set-process-sentinel proc (counsel-xgtags--make-gtags-sentinel 'update))
          (setq counsel-xgtags--last-update-time current-time))))))

 ;; counsel support
(defun counsel-xgtags--format-pased-files (tag)
  "Format parsed files."
  (let ((file (counsel-xgtags--tag-file tag))
        (query (counsel-xgtags--tag-query tag))
        (line (number-to-string (counsel-xgtags--tag-line tag)))
        (match (counsel-xgtags--tag-match tag))
        (selected-p (eq tag counsel-xgtags--selected-tag)))
    (concat
     (propertize line 'face
                 (if selected-p
                     'counsel-xgtags-line-number-selected-face
                   'counsel-xgtags-line-number-face))
     (if (>= (length line) 4)
         "\t" "\t\t")

     (propertize query 'face (if selected-p
                                 'counsel-xgtags-match-selected-face
                               'counsel-xgtags-match-face))
     (when match
       (format "\t\t%s"
               (propertize match 'face (if selected-p
                                           'counsel-xgtags-line-selected-face
                                         'counsel-xgtags-line-face)))))))

(defun counsel-xgtags--format-tag (tag)
  (let ((file (counsel-xgtags--tag-file tag))
        (query (counsel-xgtags--tag-query tag))
        (line (counsel-xgtags--tag-line tag))
        (match (counsel-xgtags--tag-match tag))
        (selected-p (eq tag counsel-xgtags--selected-tag)))
    (concat
     (format "%s:%s\t"
             (propertize file 'face
                         (if selected-p
                             'counsel-xgtags-file-selected-face
                           'counsel-xgtags-file-face))
             (propertize (number-to-string line) 'face
                         (if selected-p
                             'counsel-xgtags-line-number-selected-face
                           'counsel-xgtags-line-number-face)))
     (when match
       (format "\t\t%s"
               (propertize match 'face (if selected-p
                                           'counsel-xgtags-line-selected-face
                                         'counsel-xgtags-line-face)))))))

(defun counsel-xgtags--candidate-transformer (tag &optional escape)
  "Function to format TAG into a readable string.
If ESCAPE is t, try to escape special characters."
  (let* ((context (car counsel-xgtags--stack))
         (str (if (and tag context)
                  (cl-case (counsel-xgtags--context-type context)
                    (file (counsel-xgtags--format-pased-files tag))
                    (otherwise (counsel-xgtags--format-tag tag))))))
    (if (and str escape)
        (rx-to-string str)
      str)))

(defun counsel-xgtags-replace-tag (cand)
  "Replace tag in `CAND' with other tag."
  (interactive)
  (let* ((orig (counsel-xgtags--tag-query cand))
         (to-string (read-from-minibuffer (format "Replace <%s> with: "
                                                  orig)
                                          (upcase-initials orig))))
    (if (and to-string (> (length to-string) 0))
        (progn
          (counsel-xgtags--map-tags
           (lambda (match)
             (replace-regexp match to-string nil
                             (point)
                             (save-excursion (end-of-line) (point)))))
          (save-some-buffers t))
      (error "Empty target provided"))))

(defun counsel-xgtags--persistent-action (cand)
  "description"
  (interactive)
  (counsel-xgtags--select-and-follow-tag (cdr cand)))


(defun counsel-xgtags--fetch-tags ()
  "Search for INPUT in git log."
  (let (al)
    (dolist (tag counsel-xgtags--tags)
      (add-to-list 'al (cons (counsel-xgtags--candidate-transformer tag) tag) t))
    al))

(ivy-set-actions
 'counsel-xgtags
 '(("r" (lambda (x)
          (counsel-xgtags-replace-tag (cdr x)))
    "Replace tagname")))

(defun counsel-xgtags--activate (&optional fn)
  "Active counsel and select proper candidate if FN is provided."
  (interactive)
  (ivy-read "Pattern: " (counsel-xgtags--fetch-tags)
            :action 'counsel-xgtags--persistent-action
            :dynamic-collection nil
            :caller 'counsel-xgtags))

(defun counsel-xgtags--run-save-buffer ()
  "Run grep save results action from `counsel-do-grep-1'."
  (interactive)
  (with-counsel-alive-p
    (counsel-quit-and-execute-action 'counsel-xgtags--save-results)))

(defun counsel-xgtags--save-results (_candidate)
  "Save counsel moccur results in a `counsel-xgtags-select-mode' buffer."
  (let ((buf "*hmxgtags*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (counsel-read-string "BufferName: " buf))
      (cl-loop for b in (counsel-buffer-list)
               when (and (string= new-buf b)
                         (not (y-or-n-p
                               (format "Buffer `%s' already exists overwrite? "
                                       new-buf))))
               do (setq new-buf (counsel-read-string "OccurBufferName: " "*hmoccur ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "-*- mode: counsel-xgtags-select -*-\n\n"
                (format "Results for `%s':\n\n" counsel-input))
        (save-excursion
          (insert (with-current-buffer counsel-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))))
      (counsel-xgtags-mode) (pop-to-buffer buf))
    (message "Results saved in `%s' buffer" buf)))

(defun counsel-xgtags--collect-candidates ()
  (let* ((array (split-string counsel-pattern)))
    (setq counsel-xgtags--complete-pfx
          (cons (or (car array) "") (counsel-xgtags--build-regex (cdr array)))))


  (let* ((cmd (concat counsel-xgtags--complete-cmd " " (car counsel-xgtags--complete-pfx))))
    ;; Start grep process.
    (PDEBUG "Starting global process in directory `%s'" default-directory)
    (PDEBUG "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'counsel-xgtags--cmd-line-face) "\n\n"))
    (prog1
        (start-file-process-shell-command
         "global" counsel-buffer cmd)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process counsel-buffer)
       #'(lambda (process event)
           (counsel-process-deferred-sentinel-hook
            process event (counsel-default-directory))
           (if (string= event "finished\n")
               (with-counsel-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (:eval (format "L%s" (counsel-candidate-number-at-point))) " "
                         (:eval (propertize
                                 (format "[Global process finished - (%s results)]"
                                         (max (1- (count-lines
                                                   (point-min) (point-max)))
                                              0))
                                 'face 'counsel-locate-finish))))
                 (force-mode-line-update))
             (PDEBUG "Error: Find %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun counsel-xgtags--build-regex (patterns)
  "Build a regex for PATTERNS."
  (let ((p patterns) res )
    (while p
      (setq  res (cons (format ".*%s" (pop p)) res)))
    (if res     (mapconcat 'identity (reverse res) ""))))

;;TODO: fix performane issue:
;;      this function was called for every arg, thus consums much time if there are
;;      lots of candidates...
(defun counsel-xgtags--filter-one-by-one (arg)
  "Filter candidates (ARG) one by one."
  (let ((pattern (cdr counsel-xgtags--complete-pfx)) )
    (if (not pattern) arg
      (if (string-match pattern arg)
          arg nil))))

(defun counsel-xgtags--filtered-candidate-transformer (lst src)
  "Filter candidates.."
  (let ((pattern (cdr counsel-xgtags--complete-pfx)))
    (if (not pattern) lst
      (remove-if
       (lambda (x)
         (not (string-match pattern x))) lst))))

(defun counsel-xgtags--help-message ()
  "description")


(provide 'counsel-xgtags)

;;; counsel-xgtags.el ends here
