;;; sql+.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:
 ;; company for sql.
(yc/eval-after-load
  "company-keywords"
  (add-to-list
   'company-keywords-alist
   '(sql-mode
     "accessible" "add" "all" "alter" "analyze" "and" "as" "asc" "asensitive"
     "before" "between" "bigint" "binary" "bit_and" "bit_or" "bit_xor" "blob"
     "both" "by" "call" "cascade" "case" "cast" "change" "char" "character"
     "check" "cluster" "collate" "column" "compress" "condition" "connect"
     "constraint" "continue" "convert" "count" "create" "cross" "curdate"
     "decimal" "declare" "default" "delayed" "delete" "dense_rank" "desc"
     "describe" "deterministic" "distinct" "distinctrow" "distribute"
     "distributed" "div" "double" "drop" "dual" "each" "else" "elseif" "enclosed"
     "escaped" "exists" "exit" "explain" "extract" "false" "fetch" "float"
     "for" "force" "foreign" "from" "full" "fulltext" "gcexport"
     "gcimport" "gclocal" "gcluster" "gcluster_local" "get" "grant" "group"
     "grouped" "group_concat" "having" "high_priority" "hour_microsecond"
     "hour_minute" "hour_second" "if" "ignore" "in" "index" "infile"
     "initnodedatamap" "inner" "inout" "inpath" "insensitive" "insert" "int"
     "integer" "intersect" "interval" "into"
     "is" "iterate" "join" "key" "keys" "kill" "lag" "lead" "leading" "leave"
     "left" "level" "like" "limit" "limit_storage_size" "linear" "lines" "link"
     "load" "localtime" "localtimestamp" "lock" "long" "longblob" "longtext"
     "loop" "low_priority" "match" "max"
     "mediumblob" "mediumint" "mediumtext" "merge" "mid" "middleint" "min" "minus"
     "minute_microsecond" "minute_second" "mod" "modifies" "natural" "nocopies"
     "nocycle" "not" "now" "no_write_to_binlog" "null" "numeric" "on" "optimize"
     "option" "optionally" "or" "order" "ordered" "out" "outer" "outfile" "over"
     "percent_rank" "position" "precision" "primary" "prior" "procedure" "purge"
     "range" "rank" "read" "reads" "read_write" "real" "references" "refresh"
     "refreshnodedatamap" "regexp" "release" "rename" "repeat" "replace" "require"
     "restrict" "return" "revert" "revoke" "right" "rlike" "row_number" "schema"
     "schemas" "scn_number" "second_microsecond" "select" "self" "sensitive"
     "separator" "set" "show" "smallint" "sort" "spatial" "specific" "sql"
     "sqlexception" "sqlstate" "sqlwarning" "sql_big_result" "sql_calc_found_rows"
     "sql_small_result" "ssl" "start" "starting" "std" "stddev" "stddev_pop"
     "stddev_samp" "straight_join" "substr" "substring" "sum" "sysdate" "table"
     "target" "terminated" "then" "tinyblob" "tinyint" "tinytext" "to" "trailing"
     "trigger" "trim" "true" "undo" "union" "unique" "unlock" "unsigned" "update"
     "usage" "use" "using" "utc_date" "utc_datetime" "utc_time" "utc_timestamp"
     "values" "varbinary" "varchar" "varcharacter" "variance" "varying" "var_pop"
     "var_samp" "when" "where" "while" "with" "write" "xor" "year_month"
     "zerofill" "delimiter")))

(defvar eval-sql/command nil "Command to evaluate sql file.")
(defvar eval-sql/command-verbose-arg nil "Command to evaluate sql file.")
(defvar db/product nil "Product name.")
(defvar db/target-database nil "Target database.")

(defvar company-sql--candidates-cache nil
  "Cache for command arguments to retrieve descriptions for the candidates.")

(defun company-sql-update-candidates ()
  "Update candidates."
  (interactive)
  (puthash sql-product
           (cond
            ((equal sql-product 'postgres)
             (let* ((cmd-prefix (format "psql %s %s %s --no-psqlrc postgres -c "
                                        (if (> (length sql-user) 0)
                                            (format "-U %s" sql-user)
                                          "")
                                        (if (> (length sql-server) 0)
                                            (format "-h %s" sql-server)
                                          "")
                                        (if (= sql-port 0) "" (format "-p %d" sql-port))))
                    (cmd-list '( "'select name from pg_settings;'"
                                 "'select proname from pg_proc;'")))

               (delete-dups
                (mapcar 's-trim (s-split "\n"
                                         (mapconcat
                                          (lambda (X)
                                            (shell-command-to-string (concat cmd-prefix X)))
                                          cmd-list "\n"))))))

            (t
             (warn
              "Can't get candidates for product %s" (symbol-name sql-product))
             ""))
           company-sql--candidates-cache))

(defun company-sql--candidates (prefix)
  "Return candidates for `PREFIX'."

  (unless company-sql--candidates-cache
    (setq company-sql--candidates-cache (make-hash-table :test 'equal)))

  ;; If hash is empty, fill it.
  (unless (gethash sql-product company-sql--candidates-cache)
    (company-sql-update-candidates))

  (all-completions prefix (gethash sql-product company-sql--candidates-cache)))

(cdsq company-sql-modes '(sql-interactive-mode sql-mode))

(defun company-sql (command &optional arg &rest ignored)
  "`company-mode' completion backend for Sql.
Sql is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sql))
    (init (unless (memq major-mode company-sql-modes)
            (error "Not sql-mode")))
    (prefix (and (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-sql--candidates arg))))


;;;###autoload
(defun yc/eval-sql ()
  "Evaluate this file."
  (interactive)

  (defun eval-sql/filter (process msg)
    (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
      (display-buffer (current-buffer))
      (goto-char (point-max))
      (insert msg)))

  (defun eval-sql/sentinel (process event)
    (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
      (display-buffer (current-buffer))
      (goto-char (point-max))
      (insert "\n-- Execution finished..\n\n")
      (read-only-mode 1)
      (goto-char (point-max))))

  (aif (buffer-file-name)
      (progn
        (unless eval-sql/command
          (yc/choose-database)
          (unless eval-sql/command
            (error "Command is not set")))
        (when (buffer-modified-p)
          (if (yes-or-no-p "Buffer modified, save before evaluate? ")
              (save-buffer)))
        (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
          (read-only-mode -1)
          ;; (sql-mode)
          (goto-char (point-max))
          (display-buffer (current-buffer))

          (let* ((command (concat (eval-sql/command) " " it))
                 (process (start-process-shell-command "sql-command" nil command)))
            (insert (format "### EXECUTING COMMAND:\n\n#  %s\n\n" command))
            (set-process-sentinel process 'eval-sql/sentinel)
            (set-process-filter process 'eval-sql/filter))))
    (save-excursion
      (sql-product-interactive))
    (sql-send-buffer)))


(defun yc/choose-dbms ()
  "Choose dbms..."
  (interactive)
  (let ((target (ivy-read "Choose DBMS: " '("postgres" "mysql"))))
    (case (intern target)
      ('postgres
       (progn
         (setq db/product 'postgres)
         (sql-set-product 'postgres))
       )
      ;; todo: others....
      )))

(defun eval-sql/command ()
  "Get command to execute a query.."
  (format eval-sql/command
          (if current-prefix-arg
              ""
            eval-sql/command-verbose-arg)))

(defun yc/choose-database ()
  "Choose database.."
  (interactive)

  (unless (featurep 'sql)
    (require 'sql))

  (unless db/product
    (yc/choose-dbms))

  (let* ((cmd
          (case db/product
            ('postgres (format
                        "psql %s --no-psqlrc -lx | grep Name | awk -F '|' '{print $2}'"
                        (if (= sql-port 0)
                            ""
                          (format "-p %d" sql-port)))))
          ;; todo: others....
          ))
    (setq db/target-database
          (ivy-read "Choose database: "
                    (remove-if (lambda (x)
                                 (= (len x) 0) )
                               (mapcar 's-trim (s-split "\n" (shell-command-to-string cmd)))))))

  (case db/product
    ('postgres (setq eval-sql/command
                     (mapconcat 'identity
                                (list "TERM=xterm"
                                      "psql"
                                      (if (= sql-port 0) "" (format "-p %d" sql-port)) ;; port
                                      "%s" ;; template, left for verbose arg.
                                      "-d" db/target-database        ;; database
                                      "-f"                           ;; file
                                      ) " ")
                     eval-sql/command-verbose-arg "-a"))))


(yc/eval-after-load
  "sql"
  (define-key sql-mode-map (kbd "C-c C-b") 'yc/eval-sql))


(provide 'sql+)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; sql+.el ends here
