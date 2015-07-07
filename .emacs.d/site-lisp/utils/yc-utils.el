;;; yc-utils.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(defun yc/move-snapshot ()
  "Choose snapshot, rename it, and move into specified directory."
  (interactive)
  (let* ((flist (directory-files (expand-file-name "~/Desktop/")  t ".*.png"))
         (in-file (ivy-read "Choose a file to copy: "
                            flist))
         (target-dir (read-directory-name "Select target folder to add: "
                                          default-directory
                                          nil
                                          t))
         (target-name (ivy-read "Input new name: "
                                (list (file-name-nondirectory in-file)))))
    (shell-command (format  "mv \"%s\" %s/%s" in-file target-dir target-name))
    )
  )

(defun yc/update-key-binds ()
  "Update keybinds."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx ",(kbd" (+? nonl) ")") nil t)
      (PDEBUG "STR:" (match-string 0))

      (unless (yc/in-comments-p)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (orig (buffer-substring-no-properties beg end))
               (key (eval (eval-sexp-add-defvars (preceding-sexp))))
               (val (with-temp-buffer
                      (print key
                             (current-buffer))
                      (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
               )
          (PDEBUG "aa: " val)
          (kill-region beg end)
          (insert (concat ";; " orig) "\n")
          (indent-for-tab-command)
          (insert val)
          )
        )
      )
    )

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx "," (group "("  (*? nonl) "rx" ))
                                  nil t)
      (replace-match "\\1")
      )
    )

  (indent-region (point-min) (point-max))
  )


(defun yc/compile-rc-files ()
  "Description."
  (interactive)
  (let ((target "~/.emacs.d/rc/compiled.el") )
    (with-temp-file target
      (dolist (fn (directory-files "~/.emacs.d/rc" t (rx buffer-start (+ digit) "-" (+ nonl) ".el")))
        (insert-file-contents fn)))

    (byte-compile-file target)
    )
  )

(defun yc/expand-macro (beg end)
  "Expand macro and put to kill-ring."
  (interactive "rp")
  (let ((str (buffer-substring-no-properties beg end)) )
    (with-temp-buffer
      (insert (format "(print (macroexpand '%s))"
                      str))
      (kill-new (format "%S" (eval (eval-sexp-add-defvars (preceding-sexp))))))))


(defun yc/kill-proc (SIG &optional app)
  "kill process with `SIG', or 9 if SIG not provide."
  (interactive "P")
  (PDEBUG "SIG" SIG)

  (unless SIG
    (setq SIG 9))

  (let (pid cmd)
    (aif (executable-find "kill")
        (push it cmd)
      (error "Can't find killer"))

    (push (format "-%d" SIG) cmd)

    (let ((r-match-entry (rx
                          (group (+ digit)) (+ space)
                          (group (+? ascii)) (+ space)
                          (+? ascii) eol))
          (user (getenv "USER"))
          (ps_cmd
           (format "ps -u %s -o pid -o user -o command | grep -v 'ps\\|grep\\|PID'" user-login-name))
          pid-list)

      (with-temp-buffer
        (insert (shell-command-to-string ps_cmd))
        (goto-char (point-min))
        (while (search-forward-regexp ".+?$" nil t)
          (add-to-list 'pid-list (match-string 0) t)))

      (let ((choosen (ivy-read "Choose process: " pid-list :initial-input app)) )
        (if (string-match r-match-entry choosen)
            (setq pid (match-string 1 choosen))
          (error "Failed to parse PID"))))

    (unless pid
      (error "Can't find a running process to kill"))
    (push pid cmd)

    (PDEBUG "CMD: " cmd)

    (let ((shell-command (mapconcat 'identity (nreverse cmd) " ")) )
      (PDEBUG "shell-command: " shell-command)
      (message "KILLING %s -- %s" pid  (shell-command-to-string shell-command)))))

(defmacro yc/declare-kill-command (cmd)
  "Marco to generate commmand to kill process name of CMD."
  (interactive)
  `(defalias (intern ,(format "yc/kill-%s" cmd))
     (function (lambda (sig)
                 ,(format "Wrapper of yc/kill-proc to kill `%s' with signal SIG (as prefix)."
                          cmd)
                 (interactive "P")
                 (unless sig
                   (setq sig 9))
                 (yc/kill-proc sig ,cmd)))))

(yc/declare-kill-command "gdb")


(defun reload-file ()
  (interactive)
  (save-excursion
    (find-file (buffer-file-name))))

(defun reload-all-files ()
  (interactive)
  (save-excursion
    (let ((fn nil)
          (noconfirm t)
          (cur-buffer (current-buffer)))
      (dolist (buffer (buffer-list))
        (setq fn (buffer-file-name buffer))
        (when (and fn;; reload buffer if it is file or dir.
                   (not (verify-visited-file-modtime buffer)))
          (if (not (file-exists-p fn))
              (when (yes-or-no-p (format "File %s does not exist, delete buffer?" fn))
                (kill-buffer buffer))
            (message (format "Reloading file %s ..." (file-name-nondirectory fn)))
            (if (buffer-modified-p buffer)
                (setq noconfirm nil))
            (switch-to-buffer buffer)
            (revert-buffer t noconfirm))
          ))
      (switch-to-buffer cur-buffer)
      (message "All buffer reloaded..."))))

(defalias 'rlf 'reload-all-files)


;; *********** Fuctions for edit special rc-files quickly ************

(defun edit-emacs ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs"))


;; ******************** Others ***************************************

(defun load-this-file ()
  (interactive)
  (load-file (buffer-name)))

(defun compile-this-file ()
  "Function to byte-compile current file."
  (interactive)
  (byte-compile (buffer-name)))

;;;; dos-unix
(defun dos-unix ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "dos2unix"))
        (let ((fn (buffer-file-name)))
          (call-process "dos2unix" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))))

(defun unix-dos ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "unix2dos"))
        (let ((fn (buffer-file-name)))
          (call-process "unix2dos" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))))


(defun yc/expand-color (color)
  "Expand COLOR of form #FFF into #FFFFFF."
  (mapconcat
   (lambda (X) (make-string (if (= X ?#) 1 2 )X)) color ""))

;;;###autoload
(defun yc/syntax-color-hex ()
  "Show color text of form #fffffff in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))
     ("#[abcdef[:digit:]]\\{3\\}\\>"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (yc/expand-color (match-string-no-properties 0))))))))
  (font-lock-fontify-buffer))

(defun yc/binary-form ()
  "Show binary format of current symbol."
  (interactive)

  (defun int-to-binary-string (i)
    "Convert an integer (I) into it's binary representation in string format."
    (let ((res ""))
      (while (not (= i 0))
        (setq res (concat (if (= 1 (logand i 1)) "1" "0") res)
              i (lsh i -1)))
      (if (string= res "")
          "0" res)))
  (let* ((symbol (symbol-at-point))
         (str (if symbol (symbol-name symbol)))
         number)
    (unless symbol
      (error "Is cursor on a number?"))

    (when (string-match (rx (? (group "0x")) (group (+ hex))) str)
      (let ((base (if (match-string 1 str) 16 10)) ;; supports hex and dec
            (num (match-string 2 str)))
        (setq number (string-to-number num base))))
    (if number
        (message "Binary format of %s is: %s"
                 str (int-to-binary-string number))
      (error "Is cursor on a number??"))))

(defun yc/eval-and-insert ()
  "Evaluate last form and copy result to kill-ring."
  (interactive)
  (let* ((val (eval (eval-sexp-add-defvars (preceding-sexp))))
         (pos (save-excursion (backward-list) (point)))
         (val-str
          (with-temp-buffer
            (print val (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp "\\`[ \t\n\r]+" nil t)
              (replace-match "" t t))
            (goto-char (point-max))
            (delete-char -1)
            (buffer-substring-no-properties (point-min) (point-max)))))

    (insert "\n\n  " val-str)
    (let ((new-pos (point)) )
      (goto-char pos)
      ;; (insert comment-start)
      (indent-region pos (+ new-pos (length comment-start))))))


(defun yc/eval-and-insert-comment ()
  "Evaluate last form and copy result to kill-ring."
  (interactive)
  (let* ((val (eval (eval-sexp-add-defvars (preceding-sexp))))
         (pos (save-excursion (backward-list) (point)))
         (val-str
          (with-temp-buffer
            (print val (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp "\\`[ \t\n\r]+" nil t)
              (replace-match "" t t))
            (goto-char (point-max))
            (delete-char -1)
            (buffer-substring-no-properties (point-min) (point-max)))))

    (insert "\n  " val-str)
    (let ((new-pos (point)) )
      (goto-char pos)
      (insert comment-start)
      (indent-region pos (+ new-pos (length comment-start))))))

;;;###autoload
(defun yc/pack-env ()
  "Pack everything..."
  (interactive)
  (let (pack-name )
    (defun pack-filter (process output)
      (when (string-match (rx "Final Package:" (+ space) (group (+ nonl)) eol)
                          output)
        (setq pack-name (match-string 1 output)))
      (message "PACKENV: %s" output))

    (defun pack-sentinel (process event)
      (unless (featurep 's)
        (require 's))
      (message "Process: %s had the event %s.\n%s"
               process (s-join "" (s-split "\n" event))
               (shell-command-to-string (format "/usr/bin/stat %s" pack-name))))

    (let ((process (start-process-shell-command "PACKING" nil
                                                "~/.emacs.d/tools/pack_emacs_env.sh")))
      (set-process-filter process 'pack-filter)
      (set-process-sentinel process 'pack-sentinel))))

;;;###autoload
(defun yc/unpack-env ()
  "Pack everything..."
  (interactive)

  (let* ((fn (completing-read "Package: " nil nil nil
                              (cond
                               ((file-exists-p "~/emacs_packed.tar")  "~/emacs_packed.tar")
                               ((file-exists-p "~/tmp/emacs_packed.tar")  "~/tmp/emacs_packed.tar")
                               (t "~/emacs_packed.tar"))))
         (default-directory temporary-file-directory)
         (process nil))

    (defun unpack-filter (process output)
      (message "UNPACKENV: %s" output))

    (defun install-sentinel (process event)
      (if (string-match-p "finished" event)
          (progn
            (message "Installation finished, going to quit...")
            (sleep-for 3)
            (kill-emacs))
        (error "Process: %s had the event %s" process event)))

    (defun untar-sentinel (process event)
      (if (string-match-p "finished" event)
          (let ((default-directory (format "%s/emacs_packed" temporary-file-directory)) )
            (aif (start-process-shell-command
                  "UNPACKENV" nil
                  (format "/bin/bash %s/emacs_packed/unpack_emacs_env.sh" temporary-file-directory))
                (progn
                  (set-process-filter it 'unpack-filter)
                  (set-process-sentinel it 'install-sentinel))))
        (error "Process: %s had the event %s" process event)))

    (unless (file-exists-p fn)
      (error "File %s is not accessible" fn))

    (if (file-directory-p "emacs_packed")
        (delete-directory "emacs_packed" t))

    (aif (start-process "UNTAR" nil "tar" "xvf" (expand-file-name fn))
        (progn
          (set-process-filter it 'unpack-filter)
          (set-process-sentinel it 'untar-sentinel))
      (error "Failed to start untar process"))))

(defun yc/run-hooks-est-time (func &rest args)
  "Wrapper of run-hooks."
  (let ((hk (mapconcat
             (lambda (x) (symbol-name x)) args " "))
        (start (current-time))
        spent)
    (apply func args)
    (if (> (setq spent (float-time (time-since start))) 0.5)
        (with-current-buffer (get-buffer-create YC-DEBUG-BUF)
          (goto-char (point-max))
          (princ (format "Hook %s for %s: finished in %.02f seconds\n"
                         hk (or (buffer-file-name) "unkown") spent) (current-buffer))
          )
      )))

(defun update-debug-vars (debug)
  "Update DEBUG variables."
  (setq  YC-DEBUG debug
         debug-on-error debug
         debug-on-quit nil)
  (if YC-DEBUG
      (advice-add 'run-hooks :around #'yc/run-hooks-est-time)
    (advice-remove 'run-hooks  #'yc/run-hooks-est-time))
  (message "Debug turned %s" (if YC-DEBUG "ON" "OFF")))

(defun debug-on ()
  "Turn on debug mode."
  (interactive)
  (update-debug-vars t))

(defun debug-off ()
  "Turn on debug mode."
  (interactive)
  (update-debug-vars nil))


;;;; Add new line before or after current line.
(defun zl-newline nil
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defun zl-newline-up nil
  (interactive)
  (beginning-of-line)
  (newline-and-indent))

(defun up-slightly ()
  (interactive) (scroll-up 3))

(defun down-slightly ()
  (interactive) (scroll-down 3))

(defun yc/insert-line-number ()
  "Insert line number."
  (interactive)
  (let ((number 1))
    (save-excursion
      (when (search-backward-regexp (rx bol (* blank) (group (+ digit)) ".") nil t)
        (setq number (1+ (string-to-number (match-string 1))))))
    (if (and (looking-at "$")
             (not (looking-back "^")))
        (insert  "\n"))
    (insert (format "%d. " number))))

(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun yc/fill-region ()
  "Description."
  (interactive)
  (if (region-active-p)
      (fill-region (region-beginning) (region-end))
    (save-excursion
      (goto-char (point-at-eol))
      (unless (looking-at-p "\n\n")
        (insert "\n")))
    (fill-region (point-at-bol) (point-at-eol)))
  )


;; date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defvar current-year-format "%Y"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defun yc/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun yc/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(defun yc/insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun yc/insert-current-year ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-year-format (current-time))))

(defun yc/insert-current-buffername ()
  "Insert the filename of current buffer."
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(defun yc/copy-current-buffername ()
  "Copy full path of current file."
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (when fn
      (with-temp-buffer
        (insert fn)
        (copy-region-as-kill (point-min) (point-max))))))

(defvar shift-indent-offset 4)

(defun shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun shift-region-right (start end &optional count)
  "Shift region of Python code to the right."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (shift-region start end (prefix-numeric-value
                           (or count shift-indent-offset))))

(defun shift-region-left (start end &optional count)
  "Shift region of Python code to the left."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (shift-region start end (- (prefix-numeric-value
                              (or count shift-indent-offset)))))
(defun sort-uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "r")
  (let ((ref-line nil))
    (sort-lines nil beg end)
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line)))))


(defun uniq-region-internal (beg end)
  "description"
  (let ((ref-line nil))
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line))))
  )
(defun uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "rp")
  (uniq-region-internal beg end))

(defun uniq-remove-dup-lines (beg end)
  "Remove all duplicate lines wherever found in a file, rather than
   just contiguous lines."
  (interactive "r")
  (let ((lines '()))
    (uniq beg end
          (lambda (line) (assoc line lines))
          (lambda (line) (add-to-list 'lines (cons line t))))))

(defun uniq (beg end test-line add-line)
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (if (funcall test-line (thing-at-point 'line))
            (kill-line 1)
          (progn
            (funcall add-line (thing-at-point 'line))
            (forward-line))))
      (widen))))

(defun yc/comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and
current line is not blank and we are not at the end of the line, then
comment current line. Replaces default behaviour of comment-dwim, when it
inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun yc/strip-ws-in-string (src)
  "description"
  (let ((str-array (split-string src split-string-default-separators)))
    (mapconcat 'identity str-array "_")))

(defun yc/update-info ()
  "Update info"
  (interactive)
  (when (member system-type '(windows-nt ms-dos)) ;; do this only for Windows, Linux will do it
    ;; automatically.
    (dolist (dir (append Info-directory-list Info-default-directory-list))
      (let ((cmd (format "bash -c \"cd %s && install-info --dir-file=dir \"" dir)))
        (dolist (fn (directory-files dir t))
          (if (file-directory-p fn)
              (message "Skipping directory: %s" fn)
            (message "Processing file: %s" fn)
            (shell-command (concat cmd (file-name-nondirectory fn)))))))))

(defun yc/file-exists-p (file)
  "Like `file-exists-p' but return expanded path (of `FILE') instead of t."
  (let ((path (expand-file-name file)))
    (if (file-exists-p path)
        path
      nil)))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

(defun yc/compile-rc-files ()
  "Compile all init files."
  (interactive)
  (let ((init-dir (expand-file-name "~/.emacs.d/rc"))
        (file-name nil))
    (when init-dir
      (dolist (file-name (directory-files init-dir t ".*el"))
        (if file-name
            (byte-compile-file file-name))))))

(defun yc/decode-hex-color ()
  "Decode hex color"
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (? (or (: "0" (or "x" "X")) "#"))
                                 (group (= 2 hex)) (group (= 2 hex)) (group (= 2 hex)))))
      (skip-chars-backward "abcdefABCDEF0123456789xX#")
      (when (looking-at r-match-hex-color)
        (message "Color: (%d, %d, %d)"
                 (string-to-number (match-string 1) 16)
                 (string-to-number (match-string 2) 16)
                 (string-to-number (match-string 3) 16))
        ))))

(defun yc/encode-hex-color ()
  "Encode hex color"
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (? (group (+ digit)) "%"))))
      (skip-chars-backward "0123456789, %")
      (when (search-forward-regexp r-match-hex-color)
        (let* ((r (match-string 1))
               (g (match-string 2))
               (b (match-string 3))
               (a (match-string 4))
               (hexcolor (format (concat "#%02X%02X%02X" (if a "%02X" "%s"))
                                 (string-to-number (match-string 1))
                                 (string-to-number (match-string 2))
                                 (string-to-number (match-string 3))
                                 (if (not a) ""
                                   (/ (* 255 (string-to-number a)) 100)))))
          (kill-new hexcolor)
          (message "Original color: %s, HexColor: %s, copied to yank!"
                   (match-string 0)
                   hexcolor))))))

(defun yc/decode-hex-color-region (start end)
  "Decode region of colors"
  (interactive "rp")
  (let ((content (buffer-substring-no-properties start end))
        (r-match-hex-color (rx bol (*? ascii)
                               (group (+? (or alnum "*" ".")))
                               (: ":" (* whitespace)) (or (: "0" (or "x" "X")) "#")
                               (group (= 2 hex)) (group (= 2 hex)) (group (= 2 hex)) eol))
        res ele)

    (defun iter (pos)
      (when (string-match  r-match-hex-color content pos)
        (setq res (cons (format "Color %s: (%d, %d, %d)"
                                (match-string 1 content)
                                (string-to-number (match-string 2 content) 16)
                                (string-to-number (match-string 3 content) 16)
                                (string-to-number (match-string 4 content) 16)) res)
              pos (match-end 0))
        (iter pos)))
    (iter 0)
    (setq res (nreverse res))
    (while (setq ele (pop res))
      (message "%s" ele))))



(defun yc/reload-emacs ()
  "reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))


(defun counsel-list-directory (dir &optional pattern action-func)
  "List files in DIR with `counsel' that look like PATTERN."
  (ivy-set-actions
   'counsel-list-directory
   '(("g" (lambda (x)
            (interactive)
            (let ((default-directory dir))
              (yc/counsel-grep)))
      "grep")
     ("n" (lambda (x)
            (interactive)
            (let ((default-directory dir))
              (let ((file (concat (expand-file-name dir) "/" x)) )
                (counsel-find-file file))))
      "create")

     ))

  (ivy-read "Find file: "
            (let ((dir (expand-file-name (concat dir "/")))
                  cands)

              (dolist (item (directory-files dir))
                (let ((path (concat dir item)) )
                  (cond
                   ((string=  "." item) nil)
                   ((string= ".." item) (push path cands))
                   ((file-directory-p path) (push path cands))
                   (t
                    (when (or (not pattern)
                              (string-match-p pattern item))
                      (push path cands))))))

              (nreverse cands))

            :action (lambda (cand)
                      (interactive)
                      (if (file-directory-p cand)
                          (counsel-list-directory cand pattern)
                        (if action-func
                            (funcall action-func cand)
                          (find-file cand))))

            :caller 'counsel-list-directory))

(defun edit-project ()
  "Edit project configurations."
  (interactive)
  (counsel-list-directory "~/.emacs.d/rc" "^09[0-9]+.*?\.el"))

(defun edit-rcs ()
  "Edit rc files.."
  (interactive)
  (counsel-list-directory "~/.emacs.d/rc" "^[0-9]+.*?\.el"))

(defun edit-elpa ()
  "Edit elpa files.."
  (interactive)
  (counsel-list-directory "~/.emacs.d/elpa" (rx (+? nonl) "-" (+ digit) "." (+ digit))))

(defun edit-template ()
  "Edit templates."
  (interactive)
  (counsel-list-directory "~/.emacs.d/templates" (rx (or alnum "_"))))

(defun edit-snippets ()
  "Edit snippets."
  (interactive)
  (counsel-list-directory (format "~/.emacs.d/templates/yasnippets/%s" major-mode)))


(defun yc/add-subdirs-to-load-path ()
  "Description."
  (interactive)
  (let ((pwd (substring default-directory 0 -1)))
    (unless (member (file-name-base pwd) '(".git" ".svn" "CVS"))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

 ;; xmind

(defun xmind/trans-node (node level)
  "Translate a node."
  (let ((yc/debug-log-limit -1))
    (PDEBUG "NODE: " node
      "CHILDREN: " (gethash "children" node)))

  (let* ((result nil)
         (title (gethash "title" node))
         (tags (gethash "labels" node))
         (children (aif (gethash "children" node)
                       (gethash "attached" it)))
         (txt-indent (make-string  (if (> level 2) (* (- level 2) 2) 0) ? )))

    (PDEBUG "children: " (append children))

    ;; header...
    (push (concat (if (> level 2)
                      (concat (make-string (* (- level 3) 2) ? ) "-")
                    (make-string level ?*))
                  " " title
                  (when tags
                    (concat "		:" (mapconcat 'identity tags ":") ":")))
          result)

    (awhen (gethash "notes" node)
      (push
       (mapconcat (lambda (x)
                    (concat txt-indent x))
                  (s-split "\n" (gethash "content" (gethash "plain" it))) "\n")
  result))

    (seq-doseq (child  children)
      (push (xmind/trans-node child (1+ level)) result))

    (mapconcat 'identity (nreverse result) "\n\n")))

(defun xmind/convert-to-org (&optional input)
  "Description."
  (interactive)
  (let* ((input (or input
                    (counsel-list-directory default-directory ".*\.xmind" 'identity)))
         (temp-dir (concat (temporary-file-directory) (make-temp-name "xmind_")))
         (default-directory temp-dir)
         (json-file (concat temp-dir "/content.json" )))

    (condition-case var
        (progn

          (unless (executable-find "unzip")
            (error "Tool unzip is needed"))

          (make-directory temp-dir)
          (shell-command-to-string (format "unzip \"%s\"" input ))
          (unless (file-exists-p json-file)
            (error "Can't find file: %s" json-file))

          (let* ((json
                  (with-temp-buffer
                    (insert-file-contents  json-file)
                    (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                       :object-type 'hash-table
                                       :null-object nil
                                       :false-object nil)))
                 (root (gethash "rootTopic" (elt json 0)))
                 (title (or (gethash "title" root) "Untitled"))
                 (children (aif (gethash "children" root)
                               (gethash "attached" it)))
                 (org-file (concat (file-name-directory input) title ".org")))

            (unless (= (length json) 1)
              (error "Does not support multiple layers for now"))


            (with-temp-file org-file
              ;; TITLE & Headers
              (insert "#+TITLE: " title "\n")
              (insert "#+OPTIONS:  ^:nil H:7 num:t toc:2 \\n:nil ::t |:t -:t f:t *:t tex:t d:(HIDE) tags:not-in-toc\n")
              (insert "#+STARTUP:  align nodlcheck oddeven lognotestate\n")
              (insert "#+SEQ_TODO: TODO(t) INPROGRESS(i) WAITING(w@) | DONE(d) CANCELED(c@)\n")
              (insert "#+TAGS:     noexport(n) HIGH(h) MEDIUM(m) LOW(l)\n")
              (insert "#+EXCLUDE_TAGS: noexport\n")

              (insert (concat "#+HTML_HEAD: <style type=\"text/css\">"
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "~/.emacs.d/templates/assets/css/style.css"))
                                (catch 'done
                                  (while t
                                    (goto-char (point-min))
                                    (unless (looking-at-p (rx (+? nonl) "\n"))
                                      (throw 'done t))
                                    (while (search-forward-regexp (rx (* space) "\n" (* space)) nil t)
                                      (replace-match " "))))

                                (buffer-substring-no-properties (point-min) (point-max))
                                )

                              " </style>\n\n"
                              ))


              (insert (mapconcat
                       (lambda (x)
                         (xmind/trans-node x 1))
                       children
                       "\n\n"))
              (message "File save to: %s" org-file)))

          )
      (error (message "Faild: %s" var)))

    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))))

 ;; http

(defun http/get-body (url)
  "Return http body from url"
  (let* ((buffer (url-retrieve-synchronously url))
         (r-match-header
          (rx "HTTP/" digit "." digit (+ space)          ;; HTTP/1.1
              (group (+ digit)) (+ space) (+? nonl) "\n" ;; 200 OK
              (group (+? anything))                      ;; All fields
              "\n\n")))

    (with-current-buffer buffer
      (goto-char (point-min))
      (if (search-forward-regexp r-match-header)
          (let* ((code (string-to-number (match-string 1)))
                 (fields-str (match-string 2))
                 (body-begin (+ (match-end 2) 2)))
            (unless (= code 200)
              (error "Http code is not 200, but: %d!, url: %s" code url))

            (buffer-substring-no-properties body-begin (point-max)))
        (error "Failed to parse from contents: "
               (buffer-substring (point-min) (min 512 (point-max))))))))

(defun http/parse-kvps (content)
  "Parse CONTENT into hash table."
  (PDEBUG "Got content: " content)
  (let ((ht (make-hash-table :test 'equal))
        (r-match-kv
         (rx (group (+ (not space))) "="
             (group (or "'" "\""))
             (group (+? nonl) )
             (backref 2)))
        (pos 0))

    (while (string-match r-match-kv content pos)
      (let ((key (match-string 1 content))
            (val (match-string 3 content))
            (end (match-end 0)))

        (PDEBUG "TYPE: " key
          "VAL: " val)
        (puthash key val ht)

        (setq pos end)))
    ht))

(defun http/fix-url (url main-url)
  "Fix url."
  (let* ((r-match-url (rx bol  (group
                                (or "http" "https")
                                "://" (+? nonl) (? ":" (+ digit)))
                          (? "/" (* nonl)) eol))
         (base-url (if (string-match r-match-url main-url)
                       (match-string 1 main-url))))

    (cond
     ((string-match-p (rx bol (or "http" "https") "://") url) url)
     ((s-starts-with-p "/" url)  (concat base-url url))
     ((s-starts-with-p "." url)  (concat (file-name-directory main-url) "/" url))
     (t (error "url: %s" url))
     )
    )

  )


(defun yc/http-save-page (&optional url)
  "Description."
  (interactive)
  (let* ((url (or url
                  (read-from-minibuffer "url: ")))
         (r-match-tag
          (rx (* space) "<"
              (group (or "img" "link"))
              (group (+? nonl))  (? "/") ">"))
         filename)

    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (http/get-body url))

      ;; decide file name
      (goto-char (point-min))
      (if (search-forward-regexp (rx "<title>" (group (+? nonl)) "</title") nil t)
          (setq filename (concat (match-string 1) ".html"))
        (setq filename "yc_tmp_file.html"))


      ;; Embed something if possible...
      (goto-char (point-min))
      (while (search-forward-regexp r-match-tag nil t)
        (PDEBUG "PT: " (point))
        (let* ((begin (match-beginning 0))
               (end (match-end 0))
               (f (PDEBUG "KKK " (buffer-substring-no-properties begin end)))
               (type (match-string 1))
               (content (match-string 2))
               (ht (http/parse-kvps content))
               to-insert)


          (PDEBUG "HANDLING: " type)

          (cond
           ((string= type "link")
            ;; parse link and replace contents

            (unless (looking-at-p "\n")
              (insert "\n"))
            (let* ((rel (gethash "rel" ht)))
              (PDEBUG "REL: " rel)

              (setq to-insert
                    (cond
                     ((string= rel "stylesheet")
                      (let ((css-url (http/fix-url (gethash "href" ht) url)))
                        (format "<style>\n%s\n</style>\n"
                                (http/get-body css-url))))
                     (t
                      (PDEBUG
                        "Unhandled rel: " rel
                        "link: " (gethash "href" ht))
                      nil)))))

           ;; download and embedded it.
           ((string= type "img")
            (let* ((img-url (gethash "src" ht))
                   (ff (PDEBUG "IMG" img-url))
                   (ext (file-name-extension img-url)))

              (unless (or (string-match-p "data:image/.*?;base64" img-url)
                          (not (member (intern ext) '(png jpg gif))))

                (push "<img" to-insert)
                (push (format "src=\"data:image/%s;base64, %s\""
                              ext
                              (base64-encode-string
                               (http/get-body (http/fix-url img-url url))))
                      to-insert)
                ;; append other attributes.
                (maphash (lambda (k v)
                           (unless (string= k "src")
                             (push (format "%s=\"%s\"" k v) to-insert)))
                         ht)

                (push "/>" to-insert)

                (setq to-insert
                      (mapconcat 'identity (nreverse to-insert) " ")))))
           (t

            (progn
              (PDEBUG "Unhandled type: " type)
              nil
              )))

          (when to-insert
            (delete-region begin end)
            (goto-char begin)
            (insert to-insert))
          ))

      (write-file filename))

    (message "File saved to: %s" filename)
    )
  )


(defun yc/debug-variable ()
  "Print variable in debug.."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (YC-DEBUG t)
        (yc/debug-log-limit -1))
    (ivy-read "Describe variable: " obarray
              :predicate #'counsel--variable-p
              :require-match t
              :history 'counsel-describe-symbol-history
              :preselect (ivy-thing-at-point)
              :sort t
              :action (lambda (x)
                        (PDEBUG "VAR: " x
                          "\nVAL: " (symbol-value (intern x)))
                        (display-buffer "*YC-DEBUG*"))
              :caller 'counsel-describe-variable)))

 ;; function to set up compilers.

(cdsq yc/compiler-env
  '(("clang" . (("CC"  . "clang")
                ("CXX" . "clang++")
                ("CPPFLAGS" . nil)
                ("LDFLAGS" . nil)))
    ("gcc"   . (("CC"  . "gcc")
                ("CXX" . "g++")
                ("CPPFLAGS" . nil)
                ("LDFLAGS" . nil)))
    ("clang-asan" . (("CC"  . "clang")
               ("CXX" . "clang++")
               ("CPPFLAGS" . "-fsanitize=address -fno-omit-frame-pointer -DVALGRIND")
               ("LDFLAGS" . "-fsanitize=address -lgcc_s")))

    ("gcc-asan" . (("CC"  . "gcc")
                     ("CXX" . "g++")
                     ("CPPFLAGS" . "-fsanitize=address -fno-omit-frame-pointer -DVALGRIND")
                     ("LDFLAGS" . "-fsanitize=address -lasan")
                     )))
  "Environment variables for compilers.")

(defun yc/choose-compiler ()
  "Choose proper compiler."
  (interactive)
  (ivy-read "Compiler: " yc/compiler-env
            :action
            (lambda (x)
              (with-ivy-window
                (dolist (pair (cdr x))
                  (PDEBUG "ENV: " pair)
                  (setenv (car pair) (cdr pair)))))
            :require-match 'confirm-after-completion
            :caller 'counsel-find-file))

(defun yc/show-compilers ()
  "Display selected compilers.."
  (interactive)
  (message "CC: %s, CXX: %s, CPPFLAGS: %s, LDFLAS: %s"
           (getenv "CC") (getenv "CXX")
           (getenv "CPPFLAGS") (getenv "LDFLAGS")))

(defun yc/update-env-from ()
  "Collect and update environment variables..."
  (interactive)
  (let ((target (read-file-name "Select folder or file to add: "
                                     default-directory nil t)))

    (cond
     ((file-directory-p target)
      (let ((env-list '(("PATH"            . ("bin" "sbin"))
                        ("LD_LIBRARY_PATH" . ("lib" "lib64"))
                        ("PKG_CONFIG_PATH" . ("lib/pkgconfig" "lib64/pkgconfig"))))
            updated)
        (if (s-ends-with-p "/" target)
            (setq target (substring-no-properties target 0 -1)))
        (dolist (p env-list)
          (awhen (mapcar
                  (lambda (x)
                    (yc/file-directory-p (concat target "/" x)))
                  (cdr p))
            (setenv (car p) (concat (mapconcat 'identity it ":") ":" (getenv (car p))))
            (push (car p) updated)))

        (message "Updated variables: %s" (mapconcat 'identity (nreverse updated) ", "))))
     ((file-exists-p target)
      (yc/load-shell-env-from-file target))
     (t (error "??"))))
  (yc/update-exec-path)
)

(defalias 'yc/uef 'yc/update-env-from)


(defvar-local diff-file-regions nil)
(defvar yc/current-diff-buffer nil "Nil.")

(defun yc/ediff-cleanup-and-restore ()
  "Clean up buffers and restory layout..."
  (interactive)

  ;; First, remove it from hook to avoid killing other buffers..
  (remove-hook 'ediff-quit-hook 'yc/ediff-cleanup-and-restore)

  ;; copy file name
  (yc/ediff-copy-file-name-A)

  ;; kill buffers..
  (save-some-buffers t)
  (kill-buffer ediff-buffer-A)
  (kill-buffer ediff-buffer-B)
  (kill-buffer ediff-buffer-C)

  (ws-butler-global-mode 1)

  ;; display original buffer.
  (layout-restore))

(defun yc/view-with-ediff (&optional clear &rest backward)
  "Description."
  (interactive "P")

  (if clear
      (setq diff-file-regions nil))

  ;; For now, simply scan whole file.
  ;; TODO: if file is too large, scan part of it....
  (unless diff-file-regions
    (save-excursion
      (goto-char (point-min))
      (let ((r-match-file
             (rx
              (: bol
                 (= 3 (or "*" "+" "-")) space
                 (group (+? nonl)) ;; file
                 space
                 (: (+? nonl)
                    space
                    (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)  ;; time
                    (? "." (+ digit))
                    space (+? nonl)
                    )
                 "
"                 (= 3 (or "*" "+" "-")) space
                 (group (+? nonl)) ;; file
                 space
                 (: (+? nonl)
                    space
                    (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)  ;; time
                    (? "." (+ digit))
                    space (+? nonl)
                    )
                 "
"
                 ))))

        (while (search-forward-regexp r-match-file nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (file-A (match-string-no-properties 1))
                (file-B (match-string-no-properties 2)))
            (PDEBUG "FILES: " file-A " " file-B)

            (when diff-file-regions
              (setf (caaar diff-file-regions) (1- start)))

            (push (cons (cons nil start) (cons file-A file-B)) diff-file-regions)))

        (setf (caaar diff-file-regions) (1- (point-max)))))

    (unless diff-file-regions
      (error "Failed to parse regions ..."))

    (PDEBUG "DIFF-FILE-REGIONS:" diff-file-regions)
    )

  (let* ((ediff-ignore-similar-regions nil)
         (pt (point))
         (item (catch 'p-found
                 (dolist (item diff-file-regions)
                   (let ((region (car item)))
                     (when (and
                            (>= pt (cdr region))
                            (<= pt (car region)))
                       (throw 'p-found item)))))))

    (if item
        (let* ((files (cdr item))
               (file-A (car files))
               (file-B (cdr files)))

          ;; move cursor to next section if possible...
          (goto-char
           (if backward
               (1- (cdar item))
               (1+ (caar item))))
          (layout-save-current)
          (ws-butler-global-mode -1 )
          (add-hook 'ediff-quit-hook 'yc/ediff-cleanup-and-restore)
          (ediff-files  file-B file-A))

      (error "Can't find proper files to compare at point: %d" (point)))))

(defun yc/get-cpu-number ()
  "Return CPU number."
  (cl-case system-type
    ('gnu/linux
     (let ((cpuinfo (shell-command-to-string "cat /proc/cpuinfo | grep processor|wc -l"))
           (r-match-cpu (rx (+? digit ) eol))
           (cpu-number "1"))
       (if (string-match r-match-cpu cpuinfo)
           (setq cpu-number (match-string 0 cpuinfo)))
       (string-to-number cpu-number)))
    ('darwin
     (let ((cpuinfo (shell-command-to-string "sysctl -n hw.ncpu"))
           (r-match-cpu (rx (+? digit ) eol))
           (cpu-number "1"))
       (if (string-match r-match-cpu cpuinfo)
           (setq cpu-number (match-string 0 cpuinfo)))
       (string-to-number cpu-number)))
    (t 1)))


(defun yc/get-compiling-threads ()
  "Return proper number of threads."
  (if current-prefix-arg
      (if (listp current-prefix-arg)
          (car current-prefix-arg)
        current-prefix-arg)
    (yc/get-cpu-number)))


(defun yc/get-env (env &optional func &rest backups)
  (aif (getenv env)
      it
    (when (not func) (setq func 'identity))
    (catch 'p-found
      (dolist (var backups)
        (when (funcall func var)
          (throw 'p-found var)))
      nil)))

(defun yc/delete-current-wallpaper ()
  "Description."
  (interactive)
  (let* ((content (s-trim (shell-command-to-string "cat /tmp/CURRENT_WALLPAPERS")))
         (lst (s-split " " content)))
    (PDEBUG "CONTENT:" content)
    (if (> (length lst) 1)
        (let ((file
               (ivy-read "Multiple files, choose one:" lst
                         :action (lambda (x)
                                   (interactive)
                                   (delete-file x))))))
        (message "Multiple files: %s.." content)
      (delete-file (car lst))
      (message "File deleted: %s" (car lst))))
  )

(defun yc/kill-file-ln ()
  "Copy filename and line number."
  (interactive)
  (aif (buffer-file-name)
      (kill-new (format "%s:%d" (file-name-nondirectory it)
                        (line-number-at-pos)))))


(provide 'yc-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yc-utils.el ends here
