;;; logviewer.el -- Simple log viewer.

;; Author: YangYingchao <yangyingchao@gmail.com>
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;;   This is a simple log viewer, with syntax highlight.
;;
;;   To use logviewer, you should put logviewer.el into the top of load-path
;; of emacs, the add following lines into your .emacs:
;; (require 'logviewer)
;;
;;   When log files are huge, it will try to split huge logs into small ones
;; to speed up loading. In that case, you can press "n" & "p" to go to next
;; part (or previous part) to the log file. You can custom variable
;; logviewer-split-line to proper number to control the size of the slice of
;; huge file.
;;

;;; Code:

(require 'hl-line)
(require 'outline)

(defcustom logviewer-log-pattern (rx "." (or "LOG" "log" "Log"))
  "Pattern to decide if a file is a log file."
  :type 'string
  :group 'logviewer
  )

(defcustom logviewer-tmp-dir "/tmp/emacs-logviewer/"
  "Temporary directory to hold splited files."
  :type 'string
  :group 'logviewer
  )

(defcustom  logviewer-split-line 50000
  "Lines when trying to split files."
  :type 'integer
  :group 'logviewer
  )

(defcustom logviewer-fold-long-lines 1024
  "Fold a line if it is too long."
  :type '(radio (const :tag "Show all lines." nil)
                (integer :tag "Fold if longer than:" ))
  :group 'logviewer
  )

;; default mode map, really simple
(defvar logviewer-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map "p"
        (lambda (&optional arg)
          (interactive "^p")
          (logviewer-next-part t (or arg 1))))
      (define-key prefix-map "p"
        (lambda (&optional arg)
          (interactive "^p")
          (logviewer-next-part nil (or arg 1))))
      (define-key prefix-map "R" 'logviewer-reload-file)
      (define-key prefix-map "F" 'logviewer-set-filter)
      (define-key prefix-map "C" 'logviewer-calibrate)
      (define-key prefix-map "L" 'logviewer-toggle-long-lines)
      (define-key prefix-map "q" 'logviewer-quit)
      )
    map)
  "Keymap for logviewer mode.")

(defvar logviewer-font-lock-keywords
  `(
    ;; Date & time.
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "ERROR" "FATAL" "WARNING" "WARN"
                     "error" "fatal" "warning" "warn"
                     )) (or ":" "]")
          (group (** 0 256 not-newline)))
     (1 font-lock-warning-face) (2 font-lock-comment-face))
    (,(rx line-start
          (*? (or space alnum "/" "-")) (+ digit) ":" (+ digit) ":" (+ digit)
          (? "." (+ digit)))
     . font-lock-builtin-face)
    (,(rx bol (group (+ digit))  ":" (+ space) (+? nonl) ":" (+ space))
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (** 0 256 not-newline) (+ digit) ":" (+ digit) ":" (+ digit) (? (or "." "-") (+ digit)))
          (1+ space) (group (+? (or alnum "-" "_"  blank))) (? "["(* digit) "]")":")
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))

    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "info" "INFO" )) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-function-name-face))
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "DEBUG" "debug" )) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-keyword-face) (2 font-lock-doc-face))
    )
  "Keywords of logviewer..")

(defvar-local logviewer--current-file nil
  "Log file current viewed by logviewer.")

(put 'logviewer--current-file 'permanent-local t)

(defvar-local logviewer--current-cache-dir nil
  "Cached dir for current log viewed by logviewer.")
(put 'logviewer--current-cache-dir 'permanent-local t)

(defvar-local logviewer--current-slice
  0
  "Current slice.")
(put 'logviewer--current-slice 'permanent-local t)

(defun split-and-view-log (filename)
  "Split file specified by FILENAME and view the splits."
  (let* ((bname (file-name-nondirectory filename))
         (cache-dir (format "%s/%s" logviewer-tmp-dir bname)))
    (unless (file-exists-p cache-dir)
      (mkdir cache-dir t))
    (let* ((first-slice (format "%s/%04d" cache-dir 0))
           (process
            (start-process "Split-process" "*logviewer*"
                           (executable-find "split")
                           "--suffix-length=4"
                           "-d" "-l"
                           (format "%d" logviewer-split-line)
                           (expand-file-name filename)
                           (format "%s/" cache-dir))))

      (while (not (file-exists-p first-slice))
        (sleep-for 0.5))
      (setq logviewer--current-slice 0)
      (set-buffer (get-buffer-create bname))
      (setq logviewer--current-file filename
            logviewer--current-cache-dir cache-dir)
      (erase-buffer)
      (insert-file-contents first-slice nil)
      (switch-to-buffer bname)
      (logviewer-mode)
      (switch-to-buffer bname)
      (error "See this instead"))))

(advice-add
 'abort-if-file-too-large :around
 (lambda (func &rest args)
   (let ((size (car args))
         (op-type (cadr args))
         (filename (nth 2 args)))
     (if (string-match logviewer-log-pattern filename)
         (if (and (string= op-type "open")
                  (executable-find "split")
                  large-file-warning-threshold size
                  (> size large-file-warning-threshold))
             (if (y-or-n-p
                  (format "LogFile %s is large (%dMB), really %s? "
                          (file-name-nondirectory filename)
                          (/ size 1048576) op-type))
                 (split-and-view-log filename)
               (error "Abort")))
         (apply func args)))))

(defun logviewer-is-tmpfile ()
  "See whether current file is a temporary file or not."
  (if (string-match "log_cache" logviewer--current-file)
      t
    nil))

(defun logviewer-calibrate ()
  "Calibrate data-time of logs."
  (interactive)
  (let* ((fmt-tip "%s (format: h:m:s): ")
         (src-time (completing-read (format fmt-tip "From") nil))
         (dst-time (completing-read (format fmt-tip "To") nil))
         (ts (float-time))
         (r-match-date-time
          (rx bol
              (group (** 2 4 digit) "-" (** 1 2 digit) "-" (** 1 2 digit)
                     (+ space)
                     (** 1 2 digit) ":" (** 1 2 digit) ":" (** 1 2 digit))))
         (r-match-time-only
          (rx bol (* space)
              (group (** 1 2 digit) ":" (** 1 2 digit) ":" (** 1 2 digit))))
         (src (if (string-match r-match-time-only src-time)
                  (float-time
                   (date-to-time (concat "2001-01-01 " (match-string 1 src-time))))
                ts))
         (dst (if (string-match r-match-time-only dst-time)
                  (float-time
                   (date-to-time (concat "2001-01-01 " (match-string 1 dst-time))))

                ts))
         (diff (- dst src)))

    ;; calculate diff.
    (if (= 0 diff)
        (error "Invalid time: %s -- %s" src-time dst-time))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward r-match-date-time nil t)
        (replace-match
         (format-time-string "%Y-%m-%d %H:%M:%S"
                             (seconds-to-time
                              (+ diff (float-time (date-to-time (match-string 1)))))))))))

(defun logviewer-reload-file ()
  "Reload current file."
  (interactive)
  (let ((pt (point)))
    (read-only-mode -1)
    (erase-buffer)
    (insert-file-contents logviewer--current-file nil)
    (read-only-mode 1)
    (goto-char pt)
    (message "Readload finished.")))

(defun logviewer-next-part (next &optional num)
  "View next/previous file.
If NEXT = t, it returns next file, or it returns previous file.
NUM: prefix."
  (interactive "^p")
  (unless logviewer--current-cache-dir
    (error "Not a slice!"))

  (let* ((bname (file-name-nondirectory logviewer--current-file))
         (func (if next '+ '-))
         (index (funcall func logviewer--current-slice (or num 1)))
         (slice (format "%s/%04d" logviewer--current-cache-dir index)))
    (unless (file-exists-p slice)
      (error "%s dost not exist: %s of file reached.." slice
             (if next "Tail"  "Head")))

    (setq logviewer--current-slice index)
    (read-only-mode -1)
    (erase-buffer)
    (insert-file-contents slice nil nil nil t)
    (read-only-mode 1)
    (message (format "Now viewing: %s -- %d" logviewer--current-file index))))


(defconst logviewer-levels
  '("FATAL" "ERROR" "WARRNING" "INFO" "DEBUG"))

(defvar logviewer-filter-level 9 "nil")

(defvar logviewer--overlays nil "Folded overlays.")


(defun get-lvl-str (num)
  "description"
  (let ((x (/ num 2))
        (lst nil))
    (while (>= x 0 )
      (setq x (1- x))
      (add-to-list  'lst (nth x logviewer-levels))
      )
    lst
    )
  )


(defun logviewer-get-filter (lvl)
  "Get filter beyond LVL."
  (if (string= lvl "FATAL")
      (progn
        (setq logviewer-filter-level 1)
        (rx bow "FATAL:"))

    (if (string= lvl "ERROR")
      (progn
        (setq logviewer-filter-level 3)
        (rx bow (or "FATAL" "ERROR") ":"))
      (if (string= lvl "WARRNING")
          (progn
            (setq logviewer-filter-level 7)
            (rx bow (or "FATAL" "ERROR" "WARRNING") ":"))
        (if (string= lvl "INFO")
            (progn
              (setq logviewer-filter-level 9)
              (rx bow (or "FATAL" "ERROR" "WARRNING" "INFO") ":")  ))
        )
      )
    )
  )

(defvar logviewer-filter-list '() "nil")

(defun logviewer-iter (reg-str)
  ""
  (if (search-forward-regexp reg-str (point-max) t)
      (progn
        (let ((pos1)
              (pos2))
          (move-beginning-of-line 1)
          (setq pos1 (point))
          (move-end-of-line 1)
          (setq pos2 (point))
          (cons pos1 pos2)
          (add-to-list 'logviewer-filter-list (cons pos1 pos2))
          (logviewer-iter reg-str)
          )
        )
    nil
      )
  )

(defun logviewer-set-filter ()
  "Set and show result of filter lvl."
  (interactive)
  (setq logviewer-filter-list nil)
  (let ((lvl nil)
        (cur-lvl   logviewer-filter-level ))
    (setq lvl (completing-read "Filter Level: " logviewer-levels))
    (if (string= lvl "DEBUG")
        (outline-flag-region (point-min) (point-max) nil)
      (progn
        (let ((logviewer-filter (logviewer-get-filter lvl))
              (content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (if (< cur-lvl logviewer-filter-level)
              (outline-flag-region (point-min) (point-max) nil)
            )

          (goto-char (point-min))
          (logviewer-iter logviewer-filter)

          (outline-flag-region (point-min) (point-max) t)
          (if (> (length logviewer-filter-list) 0)
              (let ((i 0)
                    (len (length logviewer-filter-list))
                    (frange))
                (while (< i len)
                  (setq frange (nth i logviewer-filter-list))
                  (outline-flag-region (car frange) (1+ (cdr frange)) nil)
                  (setq i (1+ i))))))))))

(defvar-local logviewer--long-line-hidden t "Nil.")
(add-to-invisibility-spec '(invs . t))

(defun logviewer-toggle-long-lines ()
  "Hide or show long lines."
  (interactive)
  (unless logviewer--overlays
    (error "No lines are folded."))
  (mapc (lambda (ov)
          (overlay-put ov 'invisible
                       (if logviewer--long-line-hidden
                           nil 'invs)))
        logviewer--overlays)
  (setq logviewer--long-line-hidden (not logviewer--long-line-hidden)))

(defun logviewer-quit ()
  "Quit logviewer."
  (interactive)
  (when logviewer--current-cache-dir
    (delete-directory logviewer--current-cache-dir t))
  (kill-buffer))

(defun logviewer--hide-by-regex (r)
  "Hide region matching regex R.
R should contains one capture group."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp r nil t)
      (let* ((ov (make-overlay (match-beginning 1) (match-end 1))))
        (overlay-put ov 'invisible 'invs)
        (push ov logviewer--overlays)))))

;;;###autoload
(define-derived-mode logviewer-mode fundamental-mode "Log-Viewer"
  "Major mode for editing Logviewer files
Key definitions:
\\{logviewer-mode-map}"
   ; Setup font-lock mode.
  (set (make-local-variable 'font-lock-defaults) '(logviewer-font-lock-keywords))
  ;; (set-syntax-table logviewer-mode-syntax-table)
  (when logviewer-fold-long-lines
    (make-local-variable 'logviewer--overlays)
    (set (make-local-variable 'logviewer--long-line-hidden) t)
    (unless (numberp logviewer-fold-long-lines)
      (setq logviewer-fold-long-lines 1024))

    (logviewer--hide-by-regex
     (format ".\\{%s\\}\\(.+?\\)$" logviewer-fold-long-lines)
     ;; (rx (repeat logviewer-fold-long-lines not-newline)
     ;;     (group (+? not-newline)) eol)

     )

    ;; special handling for pg log (csv).
    (when (and buffer-file-name
               (string-match-p
                (rx buffer-start (+? nonl) "/pg_log/" (+? nonl)
                    (+? nonl) "." (or "log" "csv")) buffer-file-name))
      (logviewer--hide-by-regex
       (rx bol (= 4 digit) (= 2 (: "-" (= 2 digit))) ;; date
           space (= 2 digit) (= 2 (: ":" (= 2 digit))) "." (+ digit) ;; time
           space (group (= 16 (: (*? (not (any ","))) ",")))))))


  (setq buffer-read-only nil)
  (auto-revert-mode 1)
  (run-hooks 'logviewer-mode-hook)
  (hl-line-mode))

(provide 'logviewer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; logviewer.el ends here
