;;; sql-utils.el -- Brief introduction here.

;;; Commentary:

;;; Code:

(defun eshell/restart_pg (&optional datadir)
  "Restart PG."
  (interactive)
  (unless datadir
    (error "Usage: restart_pg datadir"))

  (aif (executable-find "pg_ctl")
      (let* ((stop-command (format "%s stop -D %s" it datadir))
             (start-command (format "%s start -D %s" it datadir))
             (final-command (format "echo Stopping via '%s'.;%s;echo Starting via '%s'.;%s"
                                    stop-command stop-command start-command start-command)))
        (start-process-shell-command "restart_pg" (current-buffer) final-command))

    (error "Could not find pg_ctl, current PATH: %s" (getenv "PATH"))))

(defun yc/remove-costs ()
  "Remove cost info."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    ;; remove costs: (cost=1268995.52..1268995.52 rows=1958 width=40)..
    (while (search-forward-regexp
            (rx "cost=" (+ digit) "." (+ digit)".."(+ digit) "." (+ digit) (* space))
            nil t)
      (replace-match "")))

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "Foreign Scan") nil t)
      (replace-match "")))

  ;; calculate time (actual time=14071.851..14373.667 rows=2044 blocks=256 loops=1)

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "(actual time="
                (group (+ digit) "." (+ digit))
                ".."
                (group (+ digit) "." (+ digit))
                (+ space) "rows=" (+? nonl) ")")
            nil t)
      (let ((startup (match-string 1))
            (total (match-string 2)))
        (insert (format " -- %.03f ms" (- (string-to-number total) (string-to-number startup)))))
      )
    )

  )

(provide 'sql-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; sql-utils.el ends here
