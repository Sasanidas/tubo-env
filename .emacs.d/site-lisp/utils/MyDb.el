;;; MyDb.el -- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(defcustom mydb/root-directory   (expand-file-name "~/Documents/Database")
  "Root directory of my database."
  :type 'String
  :group 'Mydb)

(defun mydb/do-dispatch-file (item)
  "Dispatch single ITEM."
  (interactive)
  (unless (file-exists-p item)
    (error "File: %s not accessible" item))

  (let* ((checksum (shell-command-to-string (format "md5sum '%s'" item)))
         (target-dir (concat mydb/root-directory "/"
                             (downcase (file-name-extension item)) "/"
                             (substring checksum 0 1)))
         (note-file (mydb/get-note-file item))
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
                 (rx bol "#+INTERLEAVE_PDF: " (group (+? nonl)) eol) nil t)
                (replace-match (concat "#+INTERLEAVE_PDF: " target-file))
              (warn "INTERLEAVE not found in file: %s" note-file)))

          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp
                    (format (rx bol ":NOTER_DOCUMENT: " (group (+? nonl) "%s") eol)
                            (file-name-nondirectory target-file))
                     nil t)
              (replace-match (concat ":NOTER_DOCUMENT: " target-file)))))
      (if (get-buffer (file-name-nondirectory note-file))
          (with-temp-buffer (get-buffer (file-name-nondirectory note-file))
                            (reload-file))))

    (message "%s --> %s" item target-dir)))

(defun mydb/dispatch-file ()
  "Dispatch single ITEM."
  (interactive)
  (cond
   (buffer-file-name

    (mydb/do-dispatch-file buffer-file-name)
    (kill-buffer))

   ((equal major-mode 'dired-mode)
    (mapc 'mydb/do-dispatch-file (dired-get-marked-files))
    (revert-buffer))

   (t (error "Not handled: %S" major-mode))))


(defun mydb/dispatch-directory (&optional directory)
  "Dispatch all files in DIRECTORY."
  (interactive)
  (let ((directory (or directory default-directory)))
    (unless (file-directory-p directory)
      (error "Directory %s not accessible" directory))

    (mapc 'mydb/do-dispatch-file (directory-files-recursively directory ".*"))))

(defun mydb/get-note-file (input)
  "Return note file for INPUT."
  (let ((pdf-file-name input)
        (org-file-create-dir (expand-file-name "000_notes" mydb/root-directory)))

    (unless (file-directory-p org-file-create-dir)
      (make-directory org-file-create-dir t))

    (expand-file-name (concat (file-name-base pdf-file-name) ".org")
                      org-file-create-dir)))


(defun mydb/open-note-file ()
  "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)."
  (interactive)

  (unless buffer-file-name
    (error "Current buffer not open for file"))

  (let* ((current-file buffer-file-name)
         (org-file-name (mydb/get-note-file current-file)))
    ;; Open the notes org file and enable `interleave-mode'
    (unless (file-exists-p org-file-name)
      (with-temp-file org-file-name
        (insert-file-contents "~/.emacs.d/templates/auto-insert/insert.org")
        (auto-insert--org-mode (file-name-base org-file-name))
        (goto-char (point-max))
        (insert "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"assets/css/style.css\">\n")
        (insert "#+INTERLEAVE_PDF: " current-file "\n")))

    (find-file org-file-name)
    (interleave-mode)))

(provide 'MyDb)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; MyDb.el ends here
