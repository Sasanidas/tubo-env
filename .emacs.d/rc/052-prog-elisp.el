;;; 052-prog-elisp.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;; Lisp mode.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "("
         (group (or (: "yc/" (+ (or alnum "-" "_")))
                    "add-to-list" "try-require" "add-hook" "autoload"
                    "yc/eval-after-load" "try-require-autoloads"
                    "fboundp" "boundp" "featurep" "cdsq"
                    "cds" "dsq" "sif" "aif" "font-lock-add-keywords"
                    "csq" "cdsq"  "defun*" "defmacro*" "define-error"))
         (? (+ (or space "\n"))
            (group (* (or alnum "-" "_" "'" "/" "\"")))))
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face nil t))))

(define-skeleton skeleton-require
  "generate req<>" ""
  > "(require '"
  (completing-read
   "Require File:"
   (apply
    'append
    (mapcar
     (lambda (dir)
       (when (file-exists-p dir)
         (mapcar 'file-name-sans-extension
                 (directory-files dir nil (rx (+? (or alnum "_" "+" "-")) (? (or ".el" ".gz")))))
         ))
     load-path)))
  ")")

(defun yc/insert-key-sequence-kbd ()
  "Insert key sequence (with kbd function)."
  (interactive)
  (let ((key (read-key-sequence "Stoke:")) )
    (insert (format "(kbd \"%s\")" (key-description key)))))

(defun yc/insert-key-sequence ()
  "Insert key sequence."
  (interactive)
  (yc/insert-key-sequence-kbd)
  (yc/eval-and-insert-comment))

(defun my-lisp-hook ()
  "Hook to run for Lisp mode."
  (set (make-local-variable 'header-field-list)
       '(lisp_desc blank copyright blank author blank n_emacs gpl
                   blank e_comment blank))

  (set (make-local-variable 'autopair-skip-whitespace) 'chmop)
  (eldoc-mode 1)
  (yc/add-company-backends company-elisp)
  (setq c-basic-offset 8
        tab-width 8))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("req" "" skeleton-require 1)))

(put 'add-hook 'lisp-indent-function 'defun)

(defun yc/byte-compile-current-elisp ()
  "Byte compile Lisp file."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (or (string-match-p (rx ".emacs.d/rc/" (+ nonl) ".el")
                            buffer-file-name)
            (not (string-match-p (rx (+? nonl) ".el") buffer-file-name)))
      (PDEBUG "Byte compile ignored for file: " buffer-file-name)
    (byte-compile-file buffer-file-name))))

(use-package  elisp-mode
  :mode (((rx "." (or "el" "sexp") eol) . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map
              (;; (kbd "C-c M-k")
               [3 134217835] . yc/insert-key-sequence-kbd)
              (;; (kbd "C-c M-K")
               [3 134217803] . yc/insert-key-sequence)
              )
  :hook ((emacs-lisp-mode . my-lisp-hook)
         (lisp-mode my-lisp-hook))
  :config
  (add-hook 'after-save-hook 'yc/byte-compile-current-elisp)
  )

 ;; native compile..

(when (fboundp 'native-compile-async)
  (defun yc/byte-compile-file-adv (&rest args)
    "Advice for 'byte-compile-file'.
Call FUNC which is 'byte-compile-file with ARGS."
    (let ((file (car args)) )
      (native-compile-async (car args) (file-directory-p file))))
  (advice-add 'byte-compile-file :after #'yc/byte-compile-file-adv)


  (defun yc/native-compile-file ()
    "Description."
    (interactive)
    (native-compile-async
     (counsel-list-directory default-directory)
     nil))

  (defun yc/native-compile-dir (&optional dir)
    "Description."
    (interactive)
    (unless dir
      (setq dir (let* ((suggestion default-directory)
                       (choices (list
                                 (format "    Choose directory %s" suggestion)
                                 "    Choose by selecting directory interactively."))
                       (action-index (cl-position
                                      (completing-read "Select directory: "
                                                       choices
                                                       nil
                                                       t)
                                      choices
                                      :test 'equal))
                       (project-root (case action-index
                                       (0 suggestion)
                                       (1 (read-directory-name "Select workspace folder to add: "
                                                               (or suggestion default-directory)
                                                               nil
                                                               t))
                                       (t nil))))
                      project-root)))
    (PDEBUG "Native compile: " dir)
    (native-compile-async (expand-file-name dir) t)))

(provide '052-prog-elisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 052-prog-elisp.el ends here
