;;; sql-indent-mode.el --- minor mode to indent sql codes.
;;;
;;; Commentary:
;;; Based on sql-indent.el, https://github.com/bsvingen/sql-indent.
;;;
;;; Code:

(require 'sql)

(cdsq sql-indent-first-column-regexp
  (rx (*? space)
      (or "select" "update" "insert" "delete" "union" "intersect" "from"
          "where" "into" "group" "having" "order" "set"
          "use" "alter" "create" "drop" "truncate" "begin" "else"
          "end" ")" "delimiter" "source" "limit" "analyze"
          "copy")
      (or eol space))
  "Regexp matching keywords relevant for indentation.
The regexp matches lines which start SQL statements and it matches lines
that should be indented at the same column as the start of the SQL
statement.  The regexp is created at compile-time.  Take a look at the
source before changing it.  All lines not matching this regexp will be
indented by `sql-indent-offset'.")

(cdsq sql-indent-offset 4
  "*Offset for SQL indentation.")

(defun sql-indent-log (fmt &rest args)
  "Write log if sql-indent-debug is t.
Log consists of `FMT' & `ARGS', just like `message'"
  (PDEBUG (apply 'format fmt args)))

(defun sql-indent-is-string-or-comment ()
  "Return nil if point is not in a comment or string; non-nil otherwise."
  (let ((parse-state (syntax-ppss)))
    (or (nth 3 parse-state)             ; String
	(nth 4 parse-state)))           ; Comment
  )

(defun sql-indent-get-last-line-start ()
  "Find the last non-blank line.  Return the beginning position of that line and its indentation."

  (save-excursion
    (forward-line -1)

    (while (and (not (bobp))
		(or
		 (looking-at "^\\s-*$")
		 (sql-indent-is-string-or-comment)) ; Skip comments or strings
		)

      (forward-line -1))
    (list (point) (current-indentation))
    )
  )

(defun sql-indent-level-delta (&optional prev-start prev-indent)
  "Calculate the change in level from the previous non-blank line.
Given the optional parameter `PREV-START' and `PREV-INDENT', assume that to be
the previous non-blank line.
Return a list containing the level change and the previous indentation."

  (save-excursion
    ;; Go back to the previous non-blank line
    (let* ((p-line (cond ((and prev-start prev-indent)
			  (list prev-start prev-indent))
			 ((sql-indent-get-last-line-start))))
	   (curr-start (point-at-bol))
	   (paren (nth 0 (parse-partial-sexp (nth 0 p-line) curr-start)))
       (result
        ;; Add opening or closing parens.
        ;; If the current line starts with a keyword statement (e.g. SELECT,
        ;;    FROM, ...)  back up one level
        ;; If the previous line starts with a keyword statement then add one level
        (list
         (+ paren
            (if (progn (goto-char (nth 0 p-line))
                       (looking-at sql-indent-first-column-regexp))
                1
              0)
            (if (progn (goto-char curr-start)
                       (looking-at sql-indent-first-column-regexp))
                -1
              0)
            )
         (nth 1 p-line))))

      (save-excursion
        (goto-char (point-at-bol))

        ;; Cases to increase indent level.
        (when (and (< (car result) 1)
                   (looking-back
                    (rx (or "THEN" "(") (* space) "
") nil))
          (sql-indent-log "Increasing indent level: from: %d to %d."
                          (car result) (1+ (car result)))
          (setf (car result) (1+ (car result))))

        ;; Cases to decrease indent level.
        (when (and (>= (car result) 0)
                   (or (looking-at-p (rx (* space) (or ")" "--" "#")))
                       ;; (looking-back (rx ";" (* (or space "
;; "))) nil)
;;                        (save-excursion
;;                          (forward-line -1)
;;                          (goto-char (point-at-bol))
;;                          (looking-at-p
;;                           (rx (* nonl) ";"
;;                               (or (* space)
;;                                   (: (or "--" "#") (* nonl))) eol)))
                       ))
          (sql-indent-log "Decreasing indent level.")
          (setf (car result) (1- (car result)))))
      result)))

(defun sql-indent-buffer ()
  "Indent the buffer's SQL statements."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (/= (point) (point-max))
	(forward-line)
	(sql-indent-line)
	(end-of-line))))

(defun sql-indent-line ()
  "Indent current line in an SQL statement."
  (interactive)
  (let* ((pos (- (point-max) (point)))
         (beg (progn (beginning-of-line) (point)))

         (indent-info (sql-indent-level-delta))
         (level-delta (nth 0 indent-info))
         (prev-indent (nth 1 indent-info))
         (this-indent (max 0            ; Make sure the indentation is at least 0
                           (+ prev-indent
                              (* sql-indent-offset
                                 (nth 0 indent-info))))))

    (sql-indent-log "SQL Indent: line: %3d, level delta: %3d; prev: %3d; this: %3d"
                    (line-number-at-pos) level-delta prev-indent this-indent)
    (skip-chars-forward " \t")
    (indent-line-to this-indent)
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;;;###autoload
(define-minor-mode sql-indent-mode
  "A minor mode enabling more intelligent sql indentation."
  :global nil
  (unless (memq major-mode '(sql-mode))
    (error "Sql-indent-mode works for sql-mode only"))

  (if sql-indent-mode
      (set (make-local-variable 'indent-line-function) 'sql-indent-line)
    (kill-local-variable 'indent-line-function)))


(provide 'sql-indent-mode)

;;; sql-indent-mode.el ends here
