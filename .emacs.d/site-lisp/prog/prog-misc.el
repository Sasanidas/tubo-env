;;; prog-misc.el -- Brief introduction here.

;;; Commentary:

;;; Code:


(use-package dash-at-point
  :commands (dash-at-point))

(use-package zeal-at-point
  :commands (zeal-at-point))


(defun yc/doc-at-point (&optional edit-search)
  "Call doc at point.."
  (interactive "P")
  (if (string= system-type "darwin")
      (dash-at-point edit-search)
    (zeal-at-point edit-search)))

(defun yc/insert-single-comment ()
  "Insert signle line of comment using srecode."
  (interactive)
  (condition-case err
      (progn
        (srecode-load-tables-for-mode major-mode)
        (yc/remove-empty-lines (point-min))
        (srecode-insert "declaration:comment-single-line"))
    (error (insert " /* */"))))




(provide 'prog-misc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; prog-misc.el ends here
