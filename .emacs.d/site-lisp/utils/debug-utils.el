;;; debug-utils.el -- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(require '02-functions)
(require 'cl)
(require 'realgud)
(require 's)
(require 'ivy)
(require 'swiper)


(defun yc/call-gdb (cmd)
  "Call gdb with CMD."
  (PDEBUG
   "PFX:" current-prefix-arg
   "GDB ARGS" cmd)
  (if current-prefix-arg
      (gdb (concat "gdb -i=mi "  cmd) )
    (realgud:gdb (concat "gdb "cmd))))

(defun yc/directory-files (dir)
  "List files in DIR except . & .. ."
  (mapcar (lambda (x)
            (concat dir x) )
          (remove-if
           (lambda (x)
             (or (equal "." x)
                 (equal ".." x)))
           (directory-files dir
                            nil))))

(defvar debug-proc-cands nil "Nil.")

(defun build-exec-list (string)
  "Build a header of list containing STRING from CANDS."
  (PDEBUG "ARGS" string debug-proc-cands)
  (setq debug-proc-cands (cond
               ((s-blank? string) debug-proc-cands)
               ((s-ends-with? "/" string)
                (PDEBUG "enter")
                (yc/directory-files (if (s-starts-with? "/" string) string
                                      (concat default-directory string))))
               (t

                (remove-if-not (lambda (x) (s-contains? string
                                                        x)) debug-proc-cands)))))
(defun counsel-find-exec (string)
  "Filter header CANDS fiels for STRING."
  (if (s-contains? " " string)
      (let* ((array (split-string string " "))
             (r-match-cand (s-join ".*?" array))
             (new-cands (build-exec-list (car array))))
        (PDEBUG "Format: " r-match-cand)
        (remove-if-not
         (lambda (x)
           ;; filter func...
           (string-match-p r-match-cand x))
         new-cands))
    (build-exec-list string)))

(defun debug-proc (&optional proc)
  "Debug PROC.."
  (interactive)

  (unless (executable-find "gdb")
    (error "Can't find executable gdb"))

  (unless proc
    (setq proc
          (let* ((debug-proc-cands (yc/directory-files default-directory)))
            (ivy-read "Name of process: "
                      'counsel-find-exec
                        :initial-input ""
                        :dynamic-collection t
                        :unwind (lambda ()
                                  (swiper--cleanup))
                        :caller 'counsel-skeleton))))

  (yc/call-gdb
   (aif (if (file-executable-p proc)
            proc
          (executable-find proc))
       it
     (error "Can't find %s" proc))))

(defun attach-proc (&optional proc)
  "Attach to PROC.  If PRIVILEGED is specified, run as root."
  (interactive)

  (unless (executable-find "gdb")
    (error "Can't find GNU debuger (gdb)"))

  (unless proc
    (setq proc (cond
                     ((region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
                     ((and (symbol-at-point)
                           (symbol-name (symbol-at-point))
                           (string-match-p (rx bow (+ digit) eow) (symbol-name (symbol-at-point))))
                      (symbol-name (symbol-at-point))))))

  (let (pid)
    (when proc
      (if (numberp proc) (setq pid (number-to-string proc))
        (if (> (string-to-number proc) 0) (setq pid proc))))

    (unless pid
      (let ((r-match-entry (rx
                            (group (+ digit)) (+ space)
                            (group (+? ascii)) (+ space)
                            (+? ascii) eol))
            (ps_cmd
             (format "ps %s -o pid -o user -o start_time -o command | grep -v 'ps\\|grep'"
                     (format "-u %s" user-login-name)))
            pid-list)

        (with-temp-buffer
          (insert (shell-command-to-string ps_cmd))
          (goto-char (point-min))
          (PDEBUG "PIDS:" (buffer-substring-no-properties (point-min) (point-max)))
          (while (search-forward-regexp ".+?$" nil t)
            (push (match-string 0)  pid-list)))

        (let ((choosen (ivy-read "Choose process: " (nreverse pid-list)
                                 :initial-input proc)) )
          (if (string-match r-match-entry choosen)
              (setq pid (match-string 1 choosen))
            (error "Failed to parse PID")))))

    (unless pid
      (error "Can't find a running %s to attach" proc))

    (yc/call-gdb (concat "-p " pid))))

 ;; utility functions to debug postgresql.
(defun attach-pg-proc (&optional name)
  "Attach to pg process named `NAME'."
  (interactive)
  (let ((ps-cmd (format "ps -u %s -o pid -o user -o start_time -o command | grep '[p]ostgres.*%s'"
                        user-login-name (or name "")))
        (r-match-entry (rx
                        (group (+ digit)) (+ space)
                        (group (+? ascii)) (+ space)
                        (+? ascii) eol))
        (init-input (cond
                     ((region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
                     ((and (symbol-at-point)
                           (symbol-name (symbol-at-point))
                           (string-match-p (rx bow (+ digit) eow) (symbol-name (symbol-at-point))))
                      (symbol-name (symbol-at-point)))))
        pid-list)

    (PDEBUG "PS-COMMAND: " ps-cmd)

    (with-temp-buffer
      (insert (shell-command-to-string ps-cmd))
      (goto-char (point-min))
      (while (search-forward-regexp ".+?$" nil t)
        (push (match-string 0) pid-list)))

    (PDEBUG "PID-LIST: " pid-list)

    (let ((choosen
           (if (= 1 (length pid-list))
               (car pid-list)
             (ivy-read "Choose process: " (nreverse pid-list)
                       :initial-input init-input))))

      (if (string-match r-match-entry choosen)
          (attach-proc (match-string 1 choosen))
        (error "Failed to parse PID")))))


(defun attach-pg-main ()
  "Attach to idle process (waiting for user input)."
  (interactive)
  (attach-pg-proc "-D"))

(defun attach-pg-idle ()
  "Attach to idle process (waiting for user input)."
  (interactive)
  (attach-pg-proc "idle"))

(defun attach-pg-wal ()
  "Attach to idle process (waiting for user input)."
  (interactive)
  (attach-pg-proc "wal"))



(provide 'debug-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; debug-utils.el ends here
