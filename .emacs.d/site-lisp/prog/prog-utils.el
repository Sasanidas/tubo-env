;;; prog-utils.el --- Brief introduction here. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'use-package)
(require '02-functions)
(require 'cl)
(require 'ivy)
(require 'swiper)
(require 'imenu)
(require 'counsel)



;; (if (string= system-type "darwin")
;;     (progn
;;       (use-package dash-at-point
;;         :commands (dash-at-point))
;;       (defalias 'zeal-at-point 'dash-at-point)
;;       )

;;   (use-package zeal-at-point
;;     :commands (zeal-at-point))
;;   (defalias 'dash-at-point 'zeal-at-point))

(defun yc/doc-at-point (&optional edit-search)
  "Call doc at point.."
  (interactive "P")
  ;; (if (string= system-type "darwin")
  ;;     (dash-at-point edit-search)
  ;;   (zeal-at-point edit-search))
  )

(defun yc/remove-empty-lines (&optional pos)
  "Remove empty lines around this position..."
  (interactive)
  (save-excursion
    (if pos
        (goto-char pos))

    ;; Find next non new-line and non-empty character
    (skip-chars-forward " 	\n")
    (setq pos (point))
    (when
        (<= (skip-chars-backward " 	\n") -2)
      (delete-region (1+ (point)) pos))))


(use-package srecode/find
  :commands (srecode-load-tables-for-mode))

(use-package semantic/util-modes
  :commands (semantic-stickyfunc-tag-to-stick))

(use-package srecode/insert
  :commands (srecode-insert))

(defun yc/insert-single-comment ()
  "Insert signle line of comment using srecode."
  (interactive)
  (condition-case err
      (progn
        (srecode-load-tables-for-mode major-mode)
        (yc/remove-empty-lines (point-min))
        (srecode-insert "declaration:comment-single-line"))
    (error (insert " /* */"))))



(use-package clang-format
  :commands (clang-format-buffer))

;;;###autoload
(defun yc/format-files ()
  "Format all files in `default-directory'."
  (interactive)
  (save-excursion
    (dolist (fn (directory-files default-directory nil
                                 (rx "." (or "c" "cpp" "cc" "cxx" "h" "hpp") eol)))

      (with-current-buffer (find-file-noselect fn)
        (clang-format-buffer)
        (save-buffer)
        (kill-buffer)))))

(defun get-address ()
  "Get address"
  (if (looking-at r-match-addr)
      (let* ((m-data (match-data 1))
             (addr-str (buffer-substring-no-properties (nth 2 m-data) (nth 3 m-data))))
        (string-to-number addr-str 16))))

(defun yc/asm-post-process-objdump ()
  "Post process for asm file generated by `objdump'."
  (PDEBUG "asm post process for objdump")
  (let ((r-match-func  (rx bol  (+ alnum) (+ space) "<" (+ (or "_" alnum)) ">:" eol))
        (r-match-addr  (rx (+ space) (group (+ alnum)) ":" space))
        (r-match-codes (rx ":" (+ space) (* (repeat 2 alnum) space ) (* space)))
        (r-match-offset (rx "+" "0x" (group (+ alnum))  ">"))
        pos )



    ;; first, add a space around "+"
    (save-excursion
      (while (search-forward-regexp r-match-offset nil t)
        (replace-match "+ 0x\\1 >"))
      )

    ;; then, remove instruction codes...
    (save-excursion
      (while (search-forward-regexp r-match-codes nil t)
        (replace-match ":	")))

    ;; last, calculate offset for instruction addresses.
    (save-excursion
      (while (setq pos (search-forward-regexp r-match-func nil t))
        (let* ((pos (1+ pos))
               (end (or (search-forward-regexp r-match-func nil t)
                        (point-max))))
          (goto-char end)
          (setq end (point-at-eol -1)) ;; update end position, we'll go back here later.
          (goto-char pos)
          (aif (get-address)
              (while (<= pos end)
                (goto-char pos)
                (unless (looking-at-p (rx (or (: bol eol)
                                              (: (* space)";" ))))
                  (let* ((addr (get-address))
                         (tmp-string (format "0x%x" (- addr it)))
                         (off-string (format "%s%s" tmp-string
                                             (make-string
                                              (- 7 (length tmp-string)) ? ))))
                    (insert off-string)
                    (setq end (+ end (length off-string)))))
                (setq pos (point-at-bol 2))))
          (goto-char end)
          (forward-line -1))))))

(defun yc/asm-post-process-gdb ()
  "Post process for asm file generated by `gdb'."
  (interactive)
  (PDEBUG "asm post process for gdb")
  (let ((r-match-offset (rx "<" (group "+" (+ digit)  ">:")))
        (r-match-offset-funcall (rx (group "<" (+? (or alnum "_"))) "+"
                                    (group (+ digit)  ">") eol) ))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp r-match-offset nil t)
        ;; (replace-match "< \\1\t\t")
        (let* ((str (match-string 1)))
          (replace-match (format "< %s%s" str (if (> (length str) 5) "" "\t")))
          )
        ))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp r-match-offset-funcall nil t)
        (replace-match "\\1 +\\2")))))

;;;###autoload
(defun yc/asm-post-process ()
  "Add offset to current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond
     ((looking-at-p (rx "Dump of assembler code for function"))
      (yc/asm-post-process-gdb))
     (t (yc/asm-post-process-objdump)))))


(use-package projectile
  :commands (projectile-find-other-file))


;; ================================== My STRING utils ========================
(defun eassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun eassist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))
;; ================================== My STRING utils end ====================

;; ================================== CPP-H switch ===========================
;;;###autoload
(defvar eassist-header-switches '(("h" . ("cpp" "cc" "c" "cxx" "C" "m" "mm"))
				  ("hpp" . ("cpp" "cc" "cxx"))
				  ("cpp" . ("h" "hpp"))
                  ("cxx" . ("h" "hpp"))
				  ("c" . ("h" ))
				  ("C" . ("H"))
				  ("H" . ("C" "CPP" "CC"))
				  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun eassist-switch-h-cpp ()
  "Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (eassist-string-without-last (buffer-name) (length ext)))
         (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
		 when (bufferp (get-buffer b)) return
 		 (if (get-buffer-window b)
 		     (switch-to-buffer-other-window b)
 		   (if (get-buffer-window b t)
 		       (switch-to-buffer-other-frame b)
 		     (switch-to-buffer b))))
           (loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c)))
        (error "There is no corresponding pair (header or body) file.")))
     (t
      (message "It is not a header or body file! See eassist-header-switches
variable.")))))



(defun yc/switch-h-cpp ()
  "Switch between headers and source files."
  (interactive)
      (condition-case error
        (eassist-switch-h-cpp)
      (error (projectile-find-other-file))))

;; ================================== CPP-H switch end =========================


(defun yc/enable-disable-c-block (start end)
  "Enable or disable c blocks(START ~ END) using #if 0/#endif macro."
  (interactive "rp")
  (let* ((sep (rx (* space) "//" (+ space)))
         (if-0-start (concat "#if 0" sep "TODO: Remove this ifdef!\n"))
         (if-0-end   (concat "#endif" sep "End of #if 0"))
         (if-0-end-nl (concat "\n" if-0-end))
         (r-match-if0 (format "%s%s%s" if-0-start(rx (group (+? anything))) if-0-end)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))

        (if (and (looking-at r-match-if0)
                 (search-forward-regexp r-match-if0 end t))
            (replace-match "\\1")
          (goto-char end)
          (insert (if (looking-back "\n" ) if-0-end if-0-end-nl))
          (goto-char start)
          (insert if-0-start)


          (goto-char (point-min))
          (while (search-forward sep nil t)
            (replace-match " // "))))

      (indent-region start end))))

(defun yc/insert-empty-template ()
  "Make header based on srecode."
  (interactive)
  (save-excursion
    (srecode-load-tables-for-mode major-mode)
    (srecode-insert "file:empty")
    (delete-trailing-whitespace)))

(defun yc/header-make ()
  "Make header based on srecode."
  (interactive)
  (progn;save-excursion
    (goto-char (point-min))
    (while (looking-at (or comment-start-skip comment-start))
      (forward-line))
    (condition-case err
        (progn
          (srecode-load-tables-for-mode major-mode)
          (yc/remove-empty-lines (point-min))

          (srecode-insert "file:fileheader")
          (yc/remove-empty-lines (point-max))
          (goto-char (point-max))
          (srecode-insert "file:fileheader_end"))
      (error (srecode-insert "file:filecomment")))
    )
  (delete-trailing-whitespace))

(use-package lsp-mode
  :commands (lsp--cur-workspace-check lsp--imenu-filter-symbols
                                      lsp--capability
                                      lsp--get-document-symbols
                                      lsp--get-symbol-type
                                      lsp--imenu-hierarchical-p
                                      lsp--imenu-symbol-lessp
                                      lsp--position-to-point)
  )

(use-package ccls
  :commands (ccls-file-info))

(defun yc/preprocess-file ()
  "Pre-process current file.."
  (interactive)
  (lsp--cur-workspace-check)
  (PDEBUG "default-directory:" default-directory)
  (-when-let* ((mode major-mode)
               (info (ccls-file-info))
               (args (seq-into (gethash "args" info) 'vector))
               (working-directory default-directory)
               (new-args (let ((i 0) ret)
                           (while (< i (length args))
                             (let ((arg (elt args i)))
                               (cond
                                ((string= arg "-o") (cl-incf i))
                                ((string-match-p "\\`-o.+" arg))
                                ((string-match "\\`-working-directory=\\(.+\\)" arg)
                                 (setq working-directory (match-string 1 arg)))
                                (t (push arg ret))))
                             (cl-incf i))
                           (nreverse ret))))

    (PDEBUG "DIR" (shell-command-to-string "pwd"))

    (with-current-buffer (get-buffer-create
                          (format "*lsp-ccls preprocess %s*" (buffer-name)))
      (pop-to-buffer (current-buffer))
      (with-silent-modifications
        (erase-buffer)
        (insert (format "// Generated by: %s"
                        (combine-and-quote-strings new-args)))
        (insert (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'process-file (car new-args) nil t nil "-E" (cdr new-args)))))
        (delay-mode-hooks (funcall mode))
        (setq buffer-read-only t)))))



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


(use-package imenu
  :commands (imenu--subalist-p imenu--make-index-alist))

(use-package counsel
  :commands (counsel-imenu-categorize-functions counsel-semantic-format-tag
                                                counsel-semantic-tags))

(use-package counsel-xgtags
  :commands (counsel-xgtags-parse-file))

(defvar yc/imenu--overlays nil
  "Store overlays.")

(defun yc/imenu--cleanup ()
  "Clean up the overlays."
  (while yc/imenu--overlays
    (delete-overlay (pop yc/imenu--overlays))))

(defun yc/imenu--add-overlay ()
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds.
WND, when specified is the window."
  (let ((ov  (make-overlay
              (line-beginning-position)
              (1+ (line-end-position)))))
    (overlay-put ov 'face 'swiper-line-face)
    (push ov yc/imenu--overlays)
    ))

(defun yc/counsel-imenu-update-fn()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (yc/imenu--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((regexp-or-regexps (funcall ivy--regex-function ivy-text))
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))

        (dolist (re regexps)
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re))
                 (num (get-text-property 0 'swiper-line-number (ivy-state-current ivy-last))))
            (unless (memq this-command '(ivy-yank-word
                                         ivy-yank-symbol
                                         ivy-yank-char
                                         scroll-other-window))
              (when (cl-plusp num)
                (goto-char num)
                (isearch-range-invisible (line-beginning-position)
                                         (line-end-position))
                (unless (and (>= (point) (window-start))
                             (<= (point) (window-end (ivy-state-window ivy-last) t)))
                  (recenter 3))
                ))
            (yc/imenu--add-overlay)))))))

(defun yc/lsp--imenu-create-hierarchical-index (symbols)
  "Create imenu index for hierarchical SYMBOLS.

SYMBOLS are a list of DocumentSymbol messages.

Return a nested alist keyed by symbol names. e.g.

   ((\"SomeClass\" (\"(Class)\" . 10)
                 (\"someField (Field)\" . 20)
                 (\"someFunction (Function)\" . 25)
                 (\"SomeSubClass\" (\"(Class)\" . 30)
                                  (\"someSubField (Field)\" . 35))
    (\"someFunction (Function)\" . 40))"
  (let ((symbols (lsp--imenu-filter-symbols symbols)))
    (seq-filter
     #'identity
     (seq-map #'yc/lsp--symbol-to-hierarchical-imenu-elem
              (lsp--imenu-filter-symbols symbols)
              ;; (seq-sort #'lsp--imenu-symbol-lessp
              ;;           (lsp--imenu-filter-symbols symbols))
              ))))

(defun yc/counsel-imenu-get-candidates-from (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (progn
    (cl-mapcan (lambda (elm)
                 (PDEBUG "ELM:" elm
                   "PFX: " prefix)
                 (if (imenu--subalist-p elm)
                     (progn
                       (PDEBUG "SUBALIST")
                       (yc/counsel-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        ;; pass the prefix to next recursive call
                        (concat prefix (if prefix ".") (car elm))))
                   (let ((key (concat
                               (when prefix
                                 (if (get-text-property 0 'face prefix)
                                     (format " (%s) " prefix)

                                   (concat
                                    (propertize prefix 'face 'ivy-grep-info)
                                    ": ")))
                               (let ((name (car elm)))
                                 (PDEBUG "SS" (get-text-property 0 'face name)
                                         "KK" (yc/imenu--get-symbol-face prefix))
                                 (if (get-text-property 0 'face name)
                                     name
                                   (propertize name 'face (yc/imenu--get-symbol-face prefix))
                                   )
                                 )
                               )))
                     (list (cons key
                                 (put-text-property
                                  0 1 'swiper-line-number
                                  (cond
                                   ((markerp (cdr elm))
                                    (marker-position (cdr elm)))
                                   ((numberp (cdr elm))
                                    (cdr elm))
                                   ((overlayp (cdr elm))
                                    (overlay-start (cdr elm)))
                                   (t (PDEBUG "??" (cdr elm))
                                      (error "??"))
                                   )
                                   key))))))
               alist)))


(cdsq yc/lsp--symbol-face
  '(;; (1 . "File")
    (2 . font-lock-constant-face) ;; "Module"
    (3 . font-lock-constant-face) ;; "Namespace"
    ;; (4 . "Package")
    (5 . font-lock-type-face) ;;"Class"
    (6 . font-lock-function-name-face) ;; "Method"
    (7 . font-lock-variable-name-face) ;; "Property"
    (8 . font-lock-variable-name-face)
    (9 . font-lock-function-name-face) ;; "Constructor"
    (10 . font-lock-constant-face) ;; "Enum"
    (11 . font-lock-function-name-face) ;; "Interface"
    (12 . font-lock-function-name-face) ;; Function
    (13 . font-lock-variable-name-face) ;; Variables
    (14 . font-lock-constant-face)      ;; Constant
    (15 . font-lock-string-face)        ;;
    ;; (16 . "Number")
    ;; (17 . "Boolean")
    ;; (18 . "Array")
    ;; (19 . "Object")
    ;; (20 . "Key")
    ;; (21 . "Null")
    ;; (22 . "Enum Member")
    (23 . font-lock-type-face) ;; "Struct"
    ;; (24 . "Event")
    ;; (25 . "Operator")
    ;; (26 . "Type Parameter")
    )
  "")

(defun yc/lsp--get-symbol-face (sym)
  "The face for the kind of SYM."
  (-> (gethash "kind" sym)
      (assoc yc/lsp--symbol-face)
      (cdr)

      (or 'default)))


(cdsq yc/imenu--symbol-face
  '(
    ("Packages" . font-lock-constant-face)
    ("Module" . font-lock-constant-face)
    ("Class" . 'ont-lock-type-face) ;;"Class"
    ("Module" . font-lock-constant-face) ;;"Class"
    (6 . font-lock-function-name-face) ;; "Method"
    ("Variables" . font-lock-variable-name-face) ;; "Property"
    ("Variable" . font-lock-variable-name-face) ;; "Property"
    (8 . font-lock-variable-name-face)
    (9 . font-lock-function-name-face) ;; "Constructor"
    (10 . font-lock-constant-face) ;; "Enum"
    ("Functions" . font-lock-function-name-face) ;; "Interface"
    ("Function" . font-lock-function-name-face) ;; "Interface"
    (12 . font-lock-function-name-face) ;; Function
    (13 . font-lock-variable-name-face) ;; Variables
    (14 . font-lock-constant-face)      ;; Constant
    (15 . font-lock-string-face)        ;;
    ;; (16 . "Number")
    ;; (17 . "Boolean")
    ;; (18 . "Array")
    ;; (19 . "Object")
    ;; (20 . "Key")
    ;; (21 . "Null")
    ;; (22 . "Enum Member")
    (23 . font-lock-type-face) ;; "Struct"
    ;; (24 . "Event")
    ;; (25 . "Operator")
    ;; (26 . "Type Parameter")
    )
  "")

(defun yc/imenu--get-symbol-face (sym)
  "The face for the kind of SYM."
  (-> sym
      (assoc yc/imenu--symbol-face)
      (cdr)
      (or 'default)))

(define-inline lsp--point-to-marker (p)
  (inline-quote (save-excursion (goto-char ,p) (point-marker))))

(defun lsp--symbol-get-start-point (sym)
   "Get the start point of the name of SYM.

 SYM can be either DocumentSymbol or SymbolInformation."

   (let* ((location (gethash "location" sym))
          (name-range (or (and location (gethash "range" location))
                          (gethash "selectionRange" sym)))
          (start-point (lsp--position-to-point
                        (gethash "start" name-range))))
     (if imenu-use-markers (lsp--point-to-marker start-point) start-point)))

(defun yc/lsp--symbol-to-hierarchical-imenu-elem (sym)
  "Convert SYM to hierarchical imenu elements.

SYM is a DocumentSymbol message.

Return cons cell (\"symbol-name (symbol-kind)\" . start-point) if
SYM doesn't have any children. Otherwise return a cons cell with
an alist

  (\"symbol-name\" . ((\"(symbol-kind)\" . start-point)
                    cons-cells-from-children))"
  (if (string= "Other" (lsp--get-symbol-type sym))
      nil
    (let* ((start-point (lsp--symbol-get-start-point sym))
           (name (gethash "name" sym))
           (ret (if (seq-empty-p (gethash "children" sym))
                    (cons (concat
                           (propertize (lsp--get-symbol-type sym) 'face 'ivy-grep-info)
                           ": "
                           (propertize name  'face (yc/lsp--get-symbol-face sym))
                           )
                          start-point)
                  (cons   (propertize name  'face (yc/lsp--get-symbol-face sym))
                          (cons (cons
                                 (propertize (lsp--get-symbol-type sym)  'face 'ivy-grep-info)
                                 start-point)
                                (yc/lsp--imenu-create-hierarchical-index (gethash "children" sym)))))))
      (PDEBUG "RET: " ret)
      ret)))

(defun yc/lsp--symbol-to-imenu-elem (sym)
  "Convert SYM to imenu element.

SYM is a SymbolInformation message.

Return a cons cell (full-name . start-point)."
  (PDEBUG "SYM: " sym)

  (let* ((start-point (lsp--symbol-get-start-point sym))
         (name (gethash "name" sym))
         (container (gethash "containerName" sym))
         (ret (cons (if (and lsp-imenu-show-container-name container)
                        (concat container lsp-imenu-container-name-separator name)
                      name)
                    start-point)))

    (PDEBUG "RET2: " ret)
    ret))


(defun yc/counsel-imenu-get-candidates (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (let ((yc/debug-log-limit -1))
    (PDEBUG "ALIST:" alist))

  ;; (yc/counsel-imenu-get-candidates-from alist prefix)

  (condition-case msg
      (yc/counsel-imenu-get-candidates-from alist prefix)
    (error (progn (PDEBUG "FAIL: " msg)    nil)))
  )


(defvar-local yc/cached-tags nil "last cached index.")
(defvar-local yc/document-tags-tick -1 "last tick of modification.")

(defun yc/tags-from-imenu ()
  "Get tags from imenu.
If NO-CACHED is true, do not use cached value."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))

  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items))
         (items (if (eq major-mode 'emacs-lisp-mode)
                    (counsel-imenu-categorize-functions items)
                  items))
         (tags (yc/counsel-imenu-get-candidates items)))

    (if (called-interactively-p 'interactive)
        (let ((yc/debug-log-limit -1))
          (PDEBUG "IMENU-FUNC: " imenu-create-index-function)
          (PDEBUG "TAGS: " tags)))

    (if tags
        (PDEBUG "TAGS from imenu"))
    tags))

(defun yc/tags-from-lsp ()
  "Get tags from imenu.
If NO-CACHED is true, do not use cached value."
  (interactive)
  (let (tags)
    (when (and (bound-and-true-p lsp-mode)
               (lsp--capability "documentSymbolProvider"))

      (PDEBUG "Refreshing tags from LSP..." )
      (condition-case var
          (let* ((symbols (lsp--get-document-symbols))
                 (filted-symbols (lsp--imenu-filter-symbols symbols))
                 (items
                  (if (lsp--imenu-hierarchical-p symbols)
                      (yc/lsp--imenu-create-hierarchical-index symbols)
                    (seq-map (lambda (nested-alist)
                               (cons (car nested-alist)
                                     (seq-map #'yc/lsp--symbol-to-imenu-elem (cdr nested-alist))))
                             (seq-group-by #'lsp--get-symbol-type filted-symbols)))))

            (when (called-interactively-p 'interactive)
              (PDEBUG "SYM: " symbols)
              (PDEBUG "ITEMS" items))

            (setq tags (yc/counsel-imenu-get-candidates items)))

        (error (PDEBUG "ERROR: " var)))

      (if (called-interactively-p 'interactive)
          (let ((yc/debug-log-limit 4096))
            (PDEBUG "LSP-ITEMS: " tags))))

    (if tags
        (PDEBUG "TAGS from LSP"))

    tags))

(defun yc/tags-from-outline ()
  "Get tags from outline."
  (interactive)
  (let (tags)
    (when (member major-mode '(org-mode markdown-mode latex-mode))
      (PDEBUG "Refreshing tags from outline..." )

      (condition-case var
          (let* ((settings (cdr (assq major-mode counsel-outline-settings)))
                 (outlines (counsel-outline-candidates settings)))
            (PDEBUG "OUTLINES:" outlines)
            (dolist (item outlines)
              (let ((key (car item)))
                (push (cons key
                            (put-text-property
                             0 1 'swiper-line-number
                             (marker-position (cdr item)) key)) tags)
                )))

        (error (PDEBUG "ERROR: " var)))

      (if (called-interactively-p 'interactive)
          (let ((yc/debug-log-limit 4096))
            (PDEBUG "LSP-ITEMS: " tags))))
    (if tags
        (PDEBUG "TAGS from outline"))
    (nreverse tags)))

(defun yc/show-methods-dwim ()
  "Show methods found in current file, using any possible way.."
  (interactive)

  ;; for unsupported modes, simply use imenu.
  (if (member major-mode '(pdf-view-mode))
      (imenu-choose-buffer-index)

    ;; reset tags-tick to force refresh tags.
    (if current-prefix-arg
        (setq yc/document-tags-tick -1))

    ;; update tags should happen only when timestamp changes
    (when (or (not yc/cached-tags)
              (not (= yc/document-tags-tick
                  (buffer-chars-modified-tick))))

      (PDEBUG "Refreshing tags..." )
      (setq yc/document-tags-tick (buffer-chars-modified-tick)
            yc/cached-tags
            (sort
             (or (yc/tags-from-lsp)
                 (yc/tags-from-outline)
                 (yc/tags-from-imenu))

                  (lambda (x y)
                    (PDEBUG "X" x)
                    (PDEBUG "Y" x)
                    (<
                     (get-text-property 0 'swiper-line-number (car x))
                     (get-text-property 0 'swiper-line-number (car y))))))

      (if (called-interactively-p 'interactive)
          (let ((yc/debug-log-limit 4096))
            (PDEBUG "TAG ITEMS: " yc/cached-tags))))

    (unless yc/cached-tags
      (error "Failed to get tags"))

    (let ((yc/debug-log-limit -1))
      (PDEBUG "CACHED_TAGS: " yc/cached-tags))

    ;; TODO: should recenter to a tag near current position...
    (let ((position (point))
          res)
      (unwind-protect
              (setq res
                    (ivy-read "tag: " yc/cached-tags
                              :update-fn #'yc/counsel-imenu-update-fn
                              :action (lambda (x)
                                        (recenter 3))
                              :caller 'yc/counsel-imenu))

            (unless res
              (goto-char position))
            (yc/imenu--cleanup)))))


(provide 'prog-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; prog-utils.el ends here
