;;; 02-functions.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Functions that are needed by other init files.
;;; Utility functions, can be added to yc-util.el.
;;; Code:

(defvar YC-DEBUG (getenv "DEBUG") "Set this flag to t to enable debug mode.")
(defconst YC-DEBUG-BUF "*YC-DEBUG*" "Debug buffer of my own.")

 ;; Macros
(defmacro yc/defmacro (name &rest args)
  "Define macro with NAME and ARGS.
This is macro to generate macro, and put it to proper indentation."
  `(progn
     (put (quote ,name) 'lisp-indent-function 'defun)
     (defmacro ,name ,@args)))

(put 'yc/defmacro 'lisp-indent-function 'defun)

(yc/defmacro cdsq (sym val &optional doc)
  "Customize, Define or Set value of SYM to VAL, with DOC as document."
  `(if (boundp ',sym)
       (setq ,sym ,val)
     (defvar ,sym ,val ,doc)))

(yc/defmacro aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(yc/defmacro awhen (test &rest body)
  "When TEST is non-nil, execute BODY, with it bound to result of TEST."
  `(let ((it ,test))
     (when it
       ,@body)))

(defvar yc/debug-log-limit 1024 "Nil.")

(defun yc/debug-log (&rest msgs)
  "Output MSGS with FILE and LINE."
  (let ((buf (get-buffer-create YC-DEBUG-BUF))
        (last  (if msgs
                   (nth (1- (length msgs)) msgs)
                   ))
        pos-start
        )
    (save-excursion
      (with-current-buffer buf
        (goto-char (point-max))
        (princ "\n" buf)
        (princ (format-time-string  "%a %b %d %H:%M:%S %Z %Y" (current-time)) buf)
        (princ " ======>\n" buf)
        (setq pos-start (point))
        (princ (pop msgs) buf)

        (while msgs
          (princ " " buf)

          (let ((count 0))
            (princ (pop msgs) (lambda (x)
                                (when (or (< yc/debug-log-limit 0)
                                          (< count yc/debug-log-limit))
                                  (insert x)
                                  (setq count (1+ count))
                                    )
                                ))
            )
          )
        (princ "\n" buf)

        ))
    last))

(defun PDEBUG (&rest args)
  "Write log, if YC-DEBUG is non-nil."
  (when YC-DEBUG
    (apply 'yc/debug-log args)))

(yc/defmacro yc/eval-after-load (name &rest args)
  "Macro to set expressions in `arg` to be executed after `name` is loaded."
  `(eval-after-load ,name
     ',(append (list 'progn
                     `(let ((ts (current-time)))
                        (message "Loading configuration for: %s..." ,name)
                        ,@args
                        (message "Configuration for %s finished in %.2f seconds" ,name
                                 (float-time (time-since ts ))))))))

 ;; Functions
(defun yc/get-key-code (key &optional recursive)
  "Return code of KEY."
  (cond
   ((vectorp key) key)
   ((stringp key) (read-kbd-macro key))
   ((not recursive)   (yc/get-key-code (eval key) t))
   (t (signal 'wrong-type-argument (list 'array key))))
  )

(defun yc/set-keys (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let ((map (if keymap keymap (current-global-map)))
        (prefix (if key-prefix (concat key-prefix " ") "")))

    (dolist (element key-alist)
      (let ((key (yc/get-key-code (car element)))
            (def (cdr element)))
        (define-key map key def)))))

(defun yc/unset-keys (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))


(define-key ctl-x-map "v" nil)

;;;; functions to setup platform depadent settings.
(defalias 'string-split 'split-string)


(defcustom available-width '(78 82 86 92 98)
  "Available column width for Emacs to choose.."
  :group 'user)

(defun yc/calc-column-width (x-width)
  "Calculate column based on X-WIDTH."
  (let* ((r-match-font
          (rx "-" (+? (or alpha "-")) "*-"
              (group (+? digit)) "-*" (+ (or alpha "-" "*"))))
         (font-string (face-font 'default))
         (pos 0)
         (font-width
          (if (string-match r-match-font font-string)
              (string-to-number (match-string 1 font-string))
            12))
         (width-list (sort available-width '>))
         (target (/ x-width (+ 3 font-width)))
         width)
    (PDEBUG "target" target)
    (or (catch 'width
          (while (setq width (pop width-list))
            (when (<= width target)
              (PDEBUG "Got proper width" width)
              (throw 'width width))))
        78)))


(defun yc/setup-column()
  "setup column width, fill-column can be overwriten by 100-private.el"
  (let ((x-width (x-display-pixel-width)))
    (when x-width
      (setq-default fill-column (yc/calc-column-width x-width)))))


(defun pprintfonts (fonts)
  (dolist (f fonts)
    (message "%s" f))
  fonts)

(defun yc/search-fonts (&optional font)
  (interactive)
  (let* ((target (or font (completing-read "Fonty Name: " nil)))
         (answer (pprintfonts
                  (remove-if
                   (lambda (x)
                     (not (string-match-p (concat ".*" target ".*") x)))
                   (font-family-list)

                   ;; #'null
                   ;;           (cl-loop for f in (font-family-list)
                   ;;                    collect (when (search target f)
                   ;;                              f))
                             ))))

    (if (called-interactively-p)
        (message "Target: %s, fond: %s" target answer))
    answer))

;; what-cursor-position to describe font under cursor,  with a prefix argument
;; shows the face under point, among other information.
;; keyboard shortcut is C-u C-x =

(defun yc/set-font (font points)
  (awhen (car (yc/search-fonts font))
    (PDEBUG "Set font: " it)
    (when (eq system-type 'gnu/linux)
      (setq points (min 11 points)))

    (set-face-attribute 'default nil :family it)
    (set-face-attribute 'default nil :height (* points  10))
    it))

(defun yc/set-cjk-font (font points)
  (PDEBUG "Set font CJK: " font)
  (awhen (car (yc/search-fonts font))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font nil;; (frame-parameter frame 'font)
                        charset
                        (font-spec :family it
                                   :size points)))
    it))


(defun yc/setup-font ()
  (when window-system
    (let ((font (catch 'p-found
                  (dolist (p '(;;("Menlo" . 14)
                               ("Monaco" . 13)
                               ("Cascadia Mono" . 11)
                               ("Inconsolata" . 14 )))

                    (if (yc/set-font (car p) (cdr p))
                        (throw 'p-found (car p))))))
          (cjk-font (catch 'p-found
                      (dolist (p '(( "Hiragino Sans GB"    . 16 )
                                   ( "WenQuanYi Micro Hei" . 18 )
                                   ( "Microsoft YaHei"     . 18 )))

                        (if (yc/set-cjk-font (car p) (cdr p))
                            (throw 'p-found (car p)))))))
      (message "Font set to: %s, CJK-Font: %s" font cjk-font))))

(defun yc/setup-display (&optional frame)
  "Setup display, including: font, colortheme, and fill-column."
  (interactive)
  (if window-system
    (condition-case error
        (progn
          (yc/setup-font)
          (yc/setup-column)
          (setq-default header-line-format nil)
          )
      (error (message "set display failed.")))
    (setq-default header-line-format '("%f")))
  (load-theme 'tb-dark))

(defun yc/server-create-window-system-frame (func &rest args)
  "Advice for `server-create-window-system-frame'.
Call FUNC with ARGS."
  (yc/setup-display))

(yc/eval-after-load
  "server"
  (advice-add 'server-create-window-system-frame :after #'yc/server-create-window-system-frame))



(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(yc/set-keys
 (list
  (cons (kbd "C-,") 'backward-page)
  (cons (kbd "C-.") 'forward-page)
  (cons (kbd "C->") 'end-of-buffer)
  (cons (kbd "C-<") 'beginning-of-buffer)
  (cons (kbd "C-w") 'kill-region)
  (cons [(meta ?/)] 'hippie-expand)
  (cons [f4] 'goto-line)
  (cons (kbd "C-x C") 'kill-emacs)

  ;; special mappings for iTerm2...
  (cons (kbd "M-[ 1 ; 5 l") 'backward-page)
  (cons (kbd "M-[ 1 ; 5 n") 'forward-page)
  (cons (kbd "M-[ 1 ; 6 l") 'beginning-of-buffer)
  (cons (kbd "M-[ 1 ; 6 n") 'end-of-buffer)
  (cons (kbd "M-[ 1 ; 6 n") 'end-of-buffer)
))



 ;; Advice
(defun yc/try-install-package (pkg)
  "Try to install PKG.
eg: pkg: A-B-C, will try to install following packages:
A-B-C
A-B-C-mode
A-B-Cmode

A-B
A-B-mode
A-Bmode

A
A-mode
Amode.
"
  (let ((cmps (reverse (split-string (if (stringp pkg) pkg (symbol-name pkg)) "-")))
        r-pkgs)
    (while cmps
      (let ((tmp (mapconcat 'identity (reverse cmps) "-")))
        (push tmp r-pkgs)
        (push (concat tmp "-mode")  r-pkgs)
        (push (concat tmp "mode")  r-pkgs)
        (pop cmps)))

    (let ((pkgs (reverse r-pkgs)))
      (catch 'p-installed
        (while pkgs
          (condition-case var
              (aif (car pkgs)
                  (progn
                    (message "Trying to install %s." it)
                    (package-install (intern it))
                    (throw 'p-installed t)))
            (error (progn
                     (message "%s -- %s" (car var) (cdr var))
                     (pop pkgs)))))))))

(defun yc/install-package-on-error (func &rest args)
  "Apply FUNC with ARGS.
And install necessary packages if there are errors while executing FUNC."
  (interactive)
  (condition-case err (apply func args)
    (file-error
     (let ((msg (prin1-to-string err)))
       (message "Failed to open file: %s" msg)
       (if (string-match ".*Cannot open load file.*" msg)
           (if (listp err)
               (let* ((package-name (nth (1- (length err)) err))
                      (fmt "Package %s can not be loaded, install it with ELPA? "))
                 (if package-name
                     (when (yes-or-no-p (format fmt package-name))
                       (yc/try-install-package package-name)
                       (set-auto-mode))))))))))

(advice-add  'command-execute :around #'yc/install-package-on-error)

(advice-add 'run-hooks :around #'yc/install-package-on-error)

(eval-after-load
  "timer"
  (advice-add 'timer-event-handler :around #'yc/install-package-on-error))

(defun yc/in-comments-p ()
  "Check if current point is in comment."
  (interactive)
  (let ((res   (nth 4 (syntax-ppss))))
    (when (called-interactively-p 'interactive)
      (message "%sInside comments."
               (if res "" "Not ")))
    res))

(defun yc/in-string-p ()
  "Check if current point is in comment."
  (interactive)
  (let ((res   (nth 3 (syntax-ppss))))
    (when (called-interactively-p 'interactive)
      (message "%sInside string ."
               (if res "" "Not ")))
    res))

(defun yc/in-comment-or-string-p ()
  "Check if current point is in comment or in string."
  (interactive)
  (let* ((in-comment (nth 4 (syntax-ppss)))
         (in-string (nth 3 (syntax-ppss)))
         (res   (or in-comment in-string)))
    (when (called-interactively-p 'interactive)
      (message "%sIn comment, %sin string."
               (if in-comment "" "Not ")
               (if in-string "" "Not ")))
    res))


(defun yc/disable-trailling-spaces ()
  "Don't show trailing white spaces."
  (interactive)
  (setq show-trailing-whitespace nil))

(defun yc/update-exec-path ()
  "Description."
  (interactive)
  (mapcar (lambda (x)
            (add-to-list 'exec-path x))
          (split-string (getenv "PATH") ":" )))

(defun yc/load-envs (envs)
  "Load ENVS."
  (dolist (env envs)
    (let ((kv (split-string env "=")))
      (if (not (= (length kv) 2))
          (PDEBUG "ENV entry skipped: " env)
        (let ((k (nth 0 kv))
              (v (nth 1 kv)))
          (PDEBUG "Setting env: " k "=" v)
          (setenv k v))))))

(defun yc/load-shell-env-from-cmd (cmd)
  "Parse and load shell environment variables from result of CMD."
  (yc/load-envs
   (split-string (shell-command-to-string cmd) "\n" t)))

(defun yc/load-shell-env-from-file (&optional file)
  "Load environment variables from FILE.."
  (interactive)
  (unless file
    (if (called-interactively-p 'interactive)
        (setq file (ivy-read "Load from file: " 'read-file-name-internal
                             :matcher #'counsel--find-file-matcher
                             :require-match 'confirm-after-completion
                             :history 'file-name-history
                             :caller 'counsel-find-file))))
  (unless file
    (error "Input file is nil"))

  (unless (file-exists-p file)
    (error "File %s not accessible" file))

  ;; let's clear some environment variables marked as unset...
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx bol (* space) bow "unset" eow (+ space)
                (group (+? (not space))) (* space) eol) nil t)
      (let ((var (match-string 1)))
        (PDEBUG "unset: " var)
        (setenv var  nil))))

  (yc/load-shell-env-from-cmd
   (format "zsh --login -i -c 'source %s && env'" file)))

(defun yc/load-shell-env ()
  "Load environment variables."
  (interactive)
  (yc/load-shell-env-from-cmd "zsh --login -i -c env")
  (yc/update-exec-path))

(add-hook 'emacs-startup-hook (lambda ()
                                (yc/setup-display)
                                (yc/load-shell-env)))

(defun yc/file-directory-p (x)
  "Similar to `file-directory-p', but return X unchanged if x is a directory."
  (if (file-directory-p x)
      x)
  )

(defun yc/file-exists-p (x)
  "Similar to `file-exist-p', but return X unchanged if x is a directory."
  (if (file-exists-p x)
      x)
  )

(defun yc/run-with-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.
Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))
 
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

(provide '02-functions)
;;; 02-functions.el ends here
