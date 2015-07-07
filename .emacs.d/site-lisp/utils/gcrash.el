;;; gcrash.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(defun addr-2-line (app)
  "Call add2line with executable file set to `APP'."
  (interactive)
  (unless (file-exists-p app)
    (error "Can't find file %s" app))
  (unless (executable-find "addr2line")
    (error "Executable addr2line is not available"))

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx ")[" (group "0x" (+ hex)) "]") nil t)
      (let* ((addr (match-string 1))
             (result (yc/command-output-to-string "addr2line" addr "-e" app)))
        (insert " " result))))

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx (group "0x" (+ hex))       ;; addr
                (+ space)
                (group (*? nonl))          ;; function name
                "("
                (group (+? nonl))          ;; filename
                "+"
                (group "0x" (+ hex))       ;; offset
                ")") nil t)
      (print (point))

      (let* ((addr (match-string 1)))
        (if (string= (file-name-base app) (file-name-base (match-string 3)))
            (let ((result (yc/command-output-to-string "addr2line" addr "-e" app)))
              (replace-match (concat "\\1 \\2 " result))))))))

;;;###autoload
(defun c++filt-buffer ()
  "Call c++filt for current buffer."
  (interactive)
  (let ((exec (executable-find "c++filt"))
        (pmax (point-max))
        )
    (unless exec
      (error "Can't find c++filt"))

    (save-excursion
      (goto-char (point-max))
      (call-process-region (point-min) pmax exec nil t))
    (delete-region (point-min) pmax)))

(defvar gcrash--app-hist nil "Nil.")

;;;###autoload
(defun gcrash-analyze-buffer (&optional app)
  "Analyze current buffer.
It will do several things:
1. `c++filt' is called to demangled function names.
2. `addr2line' is called if APP is provided."
  (interactive)
  (unless app
    (setq app (completing-read "App: " gcrash--app-hist nil nil)))
  (c++filt-buffer)
  (addr-2-line app)
  (save-buffer))

;;;###autoload
(defun uniq-stack ()
  "Make stacks unique."
  (interactive)
  (let ((r-match-host (rx bol (+ "-") (* space)
                          (group  (+? (or alnum "." "-" "_")) )
                          (* space) (+ "-") eol))
        (obuf (get-buffer-create (format "Uniq-Stack of: %s" (buffer-name))))
        (nr-hosts 0) suspect-hosts)

    ;; prepare obuf
    (with-current-buffer obuf
      (read-only-mode -1)
      (erase-buffer))

    (defun uniq-single-host (start end &optional host)
      "Parse and make stack unique for single host. Return t if stack of this host is suspicious."
      (let ((r-match-thread (rx bol "Thread" (* space) (group (+ digit)) (* space)
                                "(Thread" (+? ascii) ":" eol))
            (r-match-suspicious (rx (+? ascii)
                                    (or "segfault" "segment fault" "signal handler called"
                                        "abort" "raise" "__assert_fail")
                                    (? space) (? "()")
                                    (+? ascii)))
            (htable-stack (make-hash-table :test 'equal :size 2048))
            (htable-threads (make-hash-table :test 'equal :size 2048))
            (nr-uniq 0)
            ordered-numbers suspects summary-pos)
        (save-excursion
          (goto-char start)
          (while (and (< (point) end)
                      (search-forward-regexp r-match-thread end t))
            (forward-char)
            (let* ((pos (point))
                   (endp (cond
                          ((search-forward-regexp r-match-thread end t) (point-at-bol))
                          (t end)))
                   (stack (buffer-substring-no-properties pos endp))
                   (num (gethash stack htable-stack 0)))
              (puthash stack (1+ num) htable-stack)
              (goto-char endp))))

        ;; Sort tacks based on number of threads.
        (maphash (lambda (stack repeated)
                   (let ((lst (gethash repeated htable-threads nil)))
                     (puthash repeated (cons stack lst) htable-threads))
                   (add-to-list 'ordered-numbers repeated)
                   ) htable-stack)
        (sort ordered-numbers '>)

        ;; Now insert stacks and highlight suspicious ones.
        (with-current-buffer obuf
          (read-only-mode -1)
          (goto-char (point-max))
          (message "Parsing stack of host: %s" host)
          (insert "\n\n========= Host " (or host "Unknown Host") ", ")
          (setq summary-pos (point))

          (dolist (number ordered-numbers)
            (let ((stack-list (gethash number htable-threads)))
              (dolist (stack stack-list)
                (setq nr-uniq (1+ nr-uniq))
                (insert (format "\nNumber of Threads: %d" number))
                (let (added-to-list)
                  (dolist (str (string-split stack "\n"))
                    (insert (format "\n%s" str))
                    (when (string-match r-match-suspicious str)
                      (unless added-to-list
                        (setq added-to-list t
                              suspects (cons (1+ (line-number-at-pos)) suspects)))
                      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                                   'face `(:underline (:style wave :color "Red1")))))))))
          (goto-char summary-pos)
          (insert (format "Unique Stacks: %d, suspicious lines: " nr-uniq))
          (if suspects
              (progn
                (setq suspects (nreverse suspects))
                (insert (format "%d" (pop suspects)))
                (while suspects
                  (insert (format ", %d" (pop suspects)))))
            (insert "none"))
          (insert ".=========\n"))
        suspects))

    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp r-match-host nil t)
          (let ((host (match-string 1))
                (pos (match-end 0))
                host-next pos-end pos-next)
            (while (< pos (point-max))
              (if (search-forward-regexp r-match-host nil t) ;; ok, find next one
                  (setq host-next (match-string 1)
                        pos-next (point)
                        pos-end (1- (point-at-bol)))
                (setq host-next nil
                      pos-end (point-max)
                      pos-next (point-max)))
              (setq nr-hosts (1+ nr-hosts))
              (if (uniq-single-host pos pos-end host)
                  (setq suspect-hosts (cons host suspect-hosts)))
              (setq host host-next
                    pos pos-next)))
        (uniq-single-host (point-min)
                          (if (search-forward-regexp "^cmd:.*$" nil t)
                              (match-beginning 0)
                            (point-max)))))

    ;; show this buffer.
    (with-current-buffer obuf
      (when (> nr-hosts 0)
        (goto-char (point-min))
        (insert (format "Total hosts: %d" nr-hosts))
        (insert (if suspect-hosts
                    (format ", %d suspicious hosts: %s" (length suspect-hosts)
                            (mapconcat 'identity suspect-hosts ","))
                  ".")))

      (read-only-mode 1)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))



(defmacro string-concact (str &rest args)
  "Concat string STR and ARGS."
  `(setq ,str (funcall 'concat ,str ,@args)))

(defun parse_segfault ()
  "Parse segfault entry."
  (interactive)
  (let ((r-match-segfault
         (rx bol (? "[" (+ (or digit "."))"]" (+ space))
             (group (+? nonl))   ;; 1 -- app name
             "[" (+? digit) "]:" (+ space)
             "segfault at"     (+ space) (group (+ hex))   ;; 2 -- fault addr
             (+ space) "ip"    (+ space) (group (+ hex))   ;; 3 -- instruction pointer
             (+ space) "sp"    (+ space) (group (+ hex))   ;; 4 -- stack pointer
             (+ space) "error" (+ space) (group (+ digit)) ;; 5 -- error
             (+ space) "in"    (+ space) (group (+? nonl)) ;; 6 -- name of app or lib
             "[" (group (+ hex)) "+" (group (+ hex))       ;; 7 -- base addr, 8: size??
             "]" eol))
        (obuf (get-buffer-create (format "SegmentFault of: %s" (buffer-name)))))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp r-match-segfault nil t)
        (let* ((app (match-string 1))
               (addr (match-string 2))
               (ip (match-string 3))
               (sp (match-string 4))
               (err (match-string 5))
               (name (match-string 6))
               (base-addr (match-string 7))
               (msg (format "%s: segfault in %s" app name)))
          (setq addr   (string-to-number addr 16)
                ip   (string-to-number ip 16)
                sp   (string-to-number sp 16)
                err  (string-to-number err)
                base-addr (string-to-number base-addr 16))

          (string-concact msg (format " at adrress 0x%x, " addr))
          (string-concact msg (format "instruction pointer: 0x%x, " ip))
          ;; (string-concact msg (format "stack pointer: 0x%x," sp))
          (string-concact msg (format "fault offset 0x%x, Reason: " (- ip base-addr)))

          (cond
           ((= 1 (logand err 8)) (string-concact msg "use of reserved bit detected."))
           ((= 1 (logand err 16)) (string-concact msg "fault was an instruction fetch."))
           (t
            (string-concact msg (if (= 0 (logand err 1)) "no page found" "protection fault"))
            (string-concact msg " while executing "
                            (if (= 0 (logand err 2)) "read" "write") " operation")
            (string-concact msg " in "
                            (if (= 0 (logand err 4)) "kernel" "user") " mode.")))
          (with-current-buffer obuf
            (insert msg)
            (insert "\n")))))
    (display-buffer obuf)))



(provide 'gcrash)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; gcrash.el ends here
