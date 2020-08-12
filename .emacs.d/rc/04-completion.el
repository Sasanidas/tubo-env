;;; 04-completion.el -- Brief introduction here. -*- lexical-binding: t; -*-

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
  "Insert today's date into buffer."
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

(defun auto-update-defaults ()
  "Update inserted template."

  (when buffer-file-name
    (let ((fn (file-name-nondirectory buffer-file-name)))
      (yc/auto-update-template "FILE" fn)
      (yc/auto-update-template "FILE_NO_EXT" (file-name-sans-extension fn))))

  (yc/auto-update-template "DATE" (format-time-string "%Y-%m-%d" (current-time)))
  (yc/auto-update-template "TIMESTAMP" (format-time-string "%s" (current-time)))
  (yc/auto-update-template "USER" user-full-name)
  (yc/auto-update-template "EMAIL" user-mail-address)
)

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
    ("\\.i$" . ["insert.i" auto-update-defaults])
    ("\\.py$" . ["insert.py" auto-update-defaults])
    ("\\.rb$" . ["insert.rb" auto-update-defaults])
    ("\\.sh$" . ["insert.sh" auto-update-defaults])
    ("\\.swig$" . ["insert.i" auto-update-defaults])
    ("\\.tex$" . ["insert.tex" auto-update-defaults])
    ("\\.yy$" . ["insert.yy" auto-update-defaults])
    ("\\.ccls$" . ["insert.ccls" auto-update-defaults])
    ("\\.org$" . ["insert.org" auto-insert--org-mode])
    ("\\.gp$" . ["insert.gp" auto-update-defaults])
    (,(rx "." (or "perl" "pl") eol)
     . ["insert.pl" auto-update-defaults])
    (,(rx "yasnippets" (? "-private") "/")
      . ["insert.snip" auto-update-defaults])))
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
  :bind (([(meta ?/)] .
          (lambda ()
            (interactive)
            (call-interactively
             (if (yc/in-comment-or-string-p)
                 'company-dabbrev
                 'company-complete
                 )
             ))
          ))
  :bind (:map company-active-map
              ([tab] . company-complete)
              (;; ,(kbd "TAB")
               "	". company-complete))
  :custom
  (company-backends '((company-capf :separate company-yasnippet)))
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.25)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-show-numbers  t)
  (company-require-match 'never)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))

  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)
  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  ;; disable auto-complete, or " " will trigger complete.
  (company-auto-complete nil)

  :hook ((after-init . global-company-mode))
)

(defmacro yc/add-company-backends (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))
."
  (declare (indent defun))

  `(dolist (mode (doom-enlist ,modes))
     (let* ((mode-name (symbol-name mode))
            (func-name (format "yc/set-company-backends-%s" mode-name)))
       (defalias (intern func-name)
         (function (lambda (&rest args)
                     (set (make-local-variable 'company-backends)
                          (list (append ,(push 'list backends)
                                        '(company-capf :with company-yasnippet)))))))
               (add-hook (intern (concat (symbol-name mode) "-hook"))
                 (intern func-name)))))

(provide '04-completion)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 04-completion.el ends here
