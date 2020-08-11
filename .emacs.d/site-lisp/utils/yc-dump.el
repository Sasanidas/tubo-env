;;; yc-dump.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

;; dump image
(require 'yc-utils)

(defun yc/do-dump ()
  "Do dump."
  (dump-emacs-portable "~/.emacs.d/emacs.pdmp"))

 ;; (dump-emacs-portable "~/.emacs.d/emacs.pdmp")

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
  "Configure emacs with current system-configuration-options."
  (interactive)
  (let ((source-directory
         (if (and (file-exists-p ".git")
                  (file-exists-p "autogen.sh"))
             default-directory
           (yc/choose-directory) ))

        ;; tweak current options.
        (options (if (string-match (rx  (group bol (*? nonl))
                             "'CFLAGS=" (group (+? nonl))  "'"
                             (group (*? nonl)) eol)
                        system-configuration-options)
          (let* ((p1 (match-string 1 system-configuration-options))
                 (p2 (match-string 2 system-configuration-options))
                 (p3 (match-string 3 system-configuration-options))
                 (cflags (string-split p2))
                 )
            (PDEBUG "P1:" p1 "P2" p2"p3" p3
                    "FLAGS¨:"  (member "-O2" cflags))

            (unless (member "-O2" cflags)
              (push "-O2" cflags))

            (unless (member "-g" cflags)
              (push "-g" cflags))

            (unless (member "-pipe" cflags)
              (push "-pipe" cflags))

            (unless (member "-march=native" cflags)
              (push "-march=native" cflags))

            (concat p1 " 'CFLAGS=" (mapconcat 'identity cflags " ") "' " p3))

        (concat  "'CFLAGS=-pipe -g -O2 -march=native'  "
                 system-configuration-options))))

    (with-current-buffer (eshell)
      (insert (format "cd %s; " source-directory))
      (insert "./autogen.sh && ./configure  ")
      (insert " ")
      (insert options)
      (eshell-send-input))))

(provide 'yc-dump)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yc-dump.el ends here
