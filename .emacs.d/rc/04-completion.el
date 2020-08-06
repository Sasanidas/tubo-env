;;; 04-completion.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;; hippie settings
(custom-set-variables
 '(hippie-expand-try-functions-list
   '(
     try-expand-dabbrev
     try-expand-dabbrev-visible
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-expand-whole-kill)))

 ;; ************** Autoinsert templates *****************
(defun insert-today ()
  "Insert today's date into buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun yc/auto-update-template (a b)
  "Replace A with B."
  (PDEBUG "ENTER: " a "--> " b)

  (save-excursion
    (goto-char (point-min))
    (while (search-forward (format "(>>%s<<)" a) nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match b t)))))

(defun auto-replace-file-name ()
  (when buffer-file-name
    (yc/auto-update-template "FILE" (file-name-nondirectory buffer-file-name))))

(defun auto-replace-file-name-no-ext ()
  (when buffer-file-name
    (yc/auto-update-template "FILE_NO_EXT"
                             (file-name-sans-extension
                              (file-name-nondirectory buffer-file-name)))))


(defun auto-replace-date ()
  (yc/auto-update-template "DATE" (format-time-string "%Y-%m-%d" (current-time))))

(defun auto-replace-timestamp ()
  (yc/auto-update-template "TIMESTAMP" (format-time-string "%s" (current-time))))

(defun auto-replace-user ()
  (yc/auto-update-template "USER" user-full-name))

(defun auto-replace-email ()
  (yc/auto-update-template "EMAIL" user-mail-address))

(defun auto-update-defaults ()
  (auto-replace-file-name)
  (auto-replace-file-name-no-ext)
  (auto-replace-date)
  (auto-replace-timestamp)
  (auto-replace-user)
  (auto-replace-email))

(use-package autoinsert
  :commands (auto-insert)
  :hook ((find-file . auto-insert))
  :custom
  (auto-insert-directory "~/.emacs.d/templates/auto-insert/")
  (auto-insert 'other)
  (auto-insert-query nil)
  (auto-insert-alist
   `(
    ("\\.dot$" . ["insert.dot" auto-update-defaults])
    ("\\.ebuild$" . ["insert.ebuild" auto-update-defaults])
    ("\\.el$" . ["insert.el" auto-update-defaults])
    ("\\.gjs$" . ["insert.gjs" auto-update-defaults])
    ("\\.h$"   . ["header.h"])
    ("\\.i$" . ["insert.i" auto-replace-file-name-no-ext])
    ("\\.py$" . ["insert.py" auto-update-defaults])
    ("\\.rb$" . ["insert.rb" auto-update-defaults])
    ("\\.sh$" . ["insert.sh" auto-update-defaults])
    ("\\.swig$" . ["insert.i" auto-replace-file-name-no-ext])
    ("\\.tex$" . ["insert.tex" auto-update-defaults])
    ("\\.yy$" . ["insert.yy" auto-update-defaults])
    ("\\.ccls$" . ["insert.ccls" auto-update-defaults])
    ("\\.org$" . ["insert.org" auto-insert--org-mode])
    ("\\.gp$" . ["insert.gp" auto-update-defaults])
    (,(rx "." (or "perl" "pl") eol)
     . ["insert.pl" auto-update-defaults])
    (,(rx "yasnippets" (? "-private") "/")
      . ["insert.snip" auto-update-defaults])
    ))
  )

;; ******************** Yasnippet ****************************
(defun yas-with-comment (&rest strings)
  "Insert string STR as comment."
  (let* ((c-start comment-start)
        (c-end comment-end)
        (c-add comment-add)
        (str (s-join (concat "\n" (comment-padright c-start c-add ))
                     strings)))
    (PDEBUG "STR" str)
    (with-temp-buffer
      (if (yc/in-comments-p)
          (insert str)
        (insert (comment-padright c-start c-add ) str comment-end))

      (PDEBUG "CONTENT-BEFORE:" (buffer-string))
      (auto-update-defaults)
      (PDEBUG "CONTENT-AFTER:" (buffer-string))

      (buffer-substring-no-properties (point-min) (point-max)))))


(defun yc/new-snippet (name)
  "Create snippet for current mode."
  (interactive "sSnippet Name: ")
  (let* ((mode-mapping
          (list (cons 'lisp-interaction-mode 'emacs-lisp-mode)
                (cons 'c-mode 'cc-mode)))
         (mode (or (cdr (assq major-mode  mode-mapping)) major-mode))
         (priv (if (yes-or-no-p "Is this public snippet?") "" "-private"))
         (dirname (expand-file-name
                   (format "~/.emacs.d/templates/yasnippets%s/%s" priv (symbol-name mode))))
         (filename (concat dirname "/" name)))
    (unless (file-directory-p dirname) (mkdir dirname t))
    (find-file filename)))

(defun yc/format-snippets-after-expand ()
  "Format expanded snippets."
  (if (and
       (bound-and-true-p lsp-mode)
       (or (lsp--capability "documentRangeFormattingProvider")
           (lsp--registered-capability "textDocument/rangeFormatting")))

      ;; lsp is available, use lsp-format...
      (save-excursion
        (lsp-format-region yas-snippet-beg yas-snippet-end))

    ;; format based on modes...
    (cond
     ((member major-mode '(c-mode c++-mode objc-mode))
      (unless (string-match (rx (or (: "int" (+ space) "main(")
                                    "__asm__"))
                            (buffer-substring-no-properties yas-snippet-beg yas-snippet-end))

        (unless (fboundp 'clang-format-region)
          (require 'clang-format))

        (save-excursion
          (clang-format-region yas-snippet-beg yas-snippet-end))

        (when (and (not (looking-back (rx (or (: bol (* space))
                                              (: (or ")""}"";"))))))
                   (< (point) yas-snippet-end))
          (newline-and-indent)
          (if (looking-at "\n\n") (kill-line)))))
     (t (indent-region yas-snippet-beg yas-snippet-end)))))

(use-package yasnippet
  :ensure t
  :custom
  (yas-verbosity 0)
  (yas-triggers-in-field nil)
  (yas-wrap-around-region t)
  (yas-indent-line 'auto)
  (warning-suppress-types
   '((yasnippet backquote-change) (yasnippet re-marker)))
  (yas-snippet-dirs '("~/.emacs.d/templates/yasnippets"
                      "~/.emacs.d/templates/yasnippets-private"))
  (yas-prompt-functions '(yas-completing-prompt))

  :mode (((rx (or (: ".emacs.d/templates/yasnippets"
                      (? "-private") "/"
                      (+ alnum)
                      (+? ascii)
                      "/")
                  (: ".snippet" eow))) . snippet-mode))
  :hook ((after-init . yas-global-mode)
         (snippet-mode . (lambda () (setq show-trailing-whitespace t)))
         (minibuffer-inactive-mode . (lambda () (yas-minor-mode 1)))

         ;; Format inserted codes after yas-expand.
         (yas-after-exit-snippet . yc/format-snippets-after-expand)))

 ;; company mode..
(use-package company
  :ensure t
  :commands (global-company-mode)
  :bind (([(meta ?/)] . company-complete-common ))
  :bind (:map company-active-map
              ([tab] . company-complete)
              (;; ,(kbd "TAB")
               "	". company-complete))
  :custom
  (company-backends '((company-files company-capf company-dabbrev company-abbrev :with company-yasnippet)))
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.25)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-show-numbers  t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))

  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  :hook (after-init . global-company-mode)
  :config
  (progn
    (setq company-async-timeout 10)
    (advice-add
     'company--should-begin :around
     (lambda (func &rest args)
       (if     (and (> (point) (point-min))
                    (looking-back (rx (>= 2 (or alnum "_"))) (- (point) 2) nil)
                    (looking-at (rx (or eow eol))))
           (apply func args)
         nil)))))

(yc/defmacro yc/add-company-backends-with-yasnippet (&rest backends)
  `(set (make-local-variable 'company-backends)
        (push ',(append backends '(company-yasnippet :separate)) company-backends)))

(yc/defmacro yc/add-company-backends (&rest backends)
  `(set (make-local-variable 'company-backends)
        (push ',backends company-backends)))

(provide '04-completion)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 04-completion.el ends here
