;;; 054-prog-debug.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;;; company backend for gdb

(cdsq gdb-kwlist nil "List of gdb keywords")

(defun company-gdb--candidates (prefix)
  "Return candidates for `PREFIX'."
  (interactive)
  (all-completions prefix
                   (or gdb-kwlist
                       (let ((output (shell-command-to-string "gdb --batch -ex \"help all\"")))
                         (dolist (item (s-split "\n" output))
                           (when (s-contains? " -- " item)
                             (let ((iitem (car (s-split "--" item))))
                               (dolist (subitem (s-split " " iitem))
                                 (add-to-list 'gdb-kwlist subitem )))))
                         gdb-kwlist))))

(defun company-gdb (command &optional arg &rest ignored)
  "Comapny backend for gdb."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-gdb))
    (init t)
    (prefix (or (company-grab-symbol) 'stop))
    (candidates (company-gdb--candidates arg))))

 ;;;;;;;; configurations  about gdb;
(use-package gdb-mi
  :commands (gdb)
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gdb-non-stop-setting nil)
  (gdb-show-threads-by-default t)
  (gdb-switch-when-another-stopped nil)
  (gdb-speedbar-auto-raise t)
  :hook ((gud-mode . (lambda ()
                       (set (make-local-variable 'company-backends)
                            '(company-files company-gtags)))))
  :config
  (progn
    (yc/set-keys (list
                  (cons (kbd "<f5>") 'gud-go)
                  (cons (kbd "<S-f5>") 'gud-until)
                  (cons (kbd "<f6>") 'gud-next)
                  (cons (kbd "<f7>") 'gud-step)
                  (cons (kbd "<f8>") 'gud-finish)
                  (cons (kbd "<f9>") 'gud-break)
                  (cons (kbd "M-S-SPC") 'gdb-toggle-breakpoint)
                  (cons "\C-c\C-c" 'comint-interrupt-subjob)
                  (cons "\C-c\C-z" 'gdb-io-stop)
                  (cons "\C-c\C-\\" 'gdb-io-quit)
                  (cons "\C-c\C-p" 'gud-print)
                  (cons (kbd "<S-f1>") 'gud-print)
                  (cons (kbd "<S-down>") 'gud-down)
                  (cons (kbd "<S-up>") 'gud-up))
                 gud-minor-mode-map)))

(defun yc/gdb-setup-windows (&rest args)
  "Advice for `gdb-setup-windows'."
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left)))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    ;; (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))) ;; local/register
    ;;   (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3))
    (select-window win0)))

(yc/eval-after-load
  "gdb-mi"
  (advice-add 'gdb-setup-windows :override #'yc/gdb-setup-windows))

 ;; realgud

(defun yc/realgud-find-file (marker filename directory)
  "Description."
  (interactive)
  nil)

(defun yc/realgud:cmd-eval-dwim-adv (&rest args)
  "Advice for 'realgud:cmd-eval-dwim'.
Call FUNC which is 'realgud:cmd-eval-dwim with ARGS."
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'realgud:cmd-eval-region)
    (deactivate-mark))
   ((and (not current-prefix-arg)
         (thing-at-point 'symbol))
    (realgud:cmd-run-command
     (thing-at-point 'symbol)
   "eval"))

   (t
    (call-interactively #'realgud:cmd-eval)
    (if (region-active-p)
        (deactivate-mark)))))

(use-package realgud
  :pin melpa
  :commands (realgud:gdb realgud:gdb-pid)
  :hook ((realgud-short-key-mode . setup-prog-keybindings)
         (realgud:gdb-track-mode . (lambda ()
                                     (add-to-list 'company-backends 'company-gdb))))
  :custom
  (realgud-safe-mode nil)
  (realgud-file-find-function 'yc/realgud-find-file)
  :config
  (advice-add 'realgud:gdb-track-mode-hook :around #'yc/realgud:gdb-track-mode-hook-adv)
  (advice-add 'realgud:cmd-eval-dwim :override #'yc/realgud:cmd-eval-dwim-adv)
)

(use-package realgud-lldb
  :pin melpa
  :commands (realgud--lldb)
  )

(defalias 'realgud:lldb 'realgud--lldb)
(defalias 'lldb 'realgud--lldb)

(defun yc/realgud:gdb-track-mode-hook-adv (func &rest args)
  "Advice for 'realgud:gdb-track-mode-hook'.
Call FUNC which is 'realgud:gdb-track-mode-hook with ARGS."
  (realgud-track-mode-setup 't))


;; Function to debug process, either attaching to a running one, or start a new one.
(use-package debug-utils
  :commands (;; attach-pg-idle attach-pg-wal attach-pg-main attach-pg-proc
             debug-proc attach-proc attach-proc-su
                        yc/kill-gdb-buffers))


(provide '054-prog-debug)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 054-prog-debug.el ends here
