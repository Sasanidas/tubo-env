;;; 054-prog-debug.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

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
(use-package realgud
  :commands (realgud:gdb realgud:gdb-pid)
  :hook ((realgud-short-key-mode . setup-prog-keybindings))
  :custom
  (realgud-safe-mode nil)
  (realgud-file-find-function (lambda (&rest args) nil))
  :config
  (defadvice! yc/realgud:gdb-track-mode-hook-adv (&rest args)
    "Setup realgud-track-mode, to make sure comint-key-map works."
    :override #'realgud:gdb-track-mode-hook
    (realgud-track-mode-setup 't))

  (defadvice! yc/realgud:cmd-eval-dwim-adv (&rest args)
    "ORIG-FUNC is called with ARGS."
    :override #'realgud:cmd-eval-dwim
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

    (yc/add-company-backends 'realgud:gdb-track-mode 'company-gdb 'company-dabbrev))

(use-package realgud-lldb
  :commands (realgud--lldb))

(use-package realgud-gdb
  :config
  (require 'realgud))


(defalias 'realgud:lldb 'realgud--lldb)
(defalias 'lldb 'realgud--lldb)


;; Function to debug process, either attaching to a running one, or start a new one.
(use-package debug-utils
  :commands (;; attach-pg-idle attach-pg-wal attach-pg-main attach-pg-proc
             debug-proc attach-proc attach-proc-su
                        yc/kill-gdb-buffers company-gdb))


(provide '054-prog-debug)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 054-prog-debug.el ends here
