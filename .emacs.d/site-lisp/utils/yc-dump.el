;;; yc-dump.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

;; load configurations..
;; (load-file (expand-file-name "~/.emacs"))

;; (dolist (package '(use-package company ivy counsel org lsp-mode
;;                     use-package swiper  highlight-parentheses
;;                     ws-butler flyspell magit  flycheck))
;;   (require package))

;; dump image

(defun yc/do-dump ()
  "Do dump."
  (dump-emacs-portable "~/.emacs.d/emacs.pdmp"))

 (dump-emacs-portable "~/.emacs.d/emacs.pdmp")

(defun yc/dump-emacs ()
  "Start another process to dump Emacs."
  (interactive)
  (let* ((buf (switch-to-buffer "*dump*"))
         (process (start-process
                   "dump-emacs" "*dump*" "emacs" "-batch"
                   "-Q" ;; for now, do not load anything, otherwise: emacs: Trying to load incoherent dumped .eln
                   "-l" (expand-file-name "~/.emacs.d/site-lisp/utils/yc-dump.el")
                   ;; "--execute \"(yc/do-dump)\""
                   ))
         )

    (set-process-sentinel process (lambda (proc event)
                                    (when (equal 'exit (process-status proc))
                                      (when (= 0 (process-exit-status proc))
                                        (message "OK")
                                        ))))


    )
  )

(defun yc/config-emacs ()
  "Configure emacs with current system-configuration-options"
  (interactive)
  (with-current-buffer (eshell)
    (insert "./autogen.sh && ./configure ")
    (insert system-configuration-options)
    (eshell-send-input)
    )
  )

(provide 'yc-dump)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yc-dump.el ends here
