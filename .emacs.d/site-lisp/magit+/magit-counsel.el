;;; magit-counsel.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(require 'magit-git)
(require 'magit-process)

;;;###autoload
(defun counsel-magit-checkout ()
  "Run magit-checkout with counsel."
  (interactive)
  (ivy-read "Checkout " (magit-list-refnames)
            :action (lambda (rev)
                      (interactive)
                      (magit-run-git "checkout" rev))

            :caller 'counsel-magit))

(defun counsel-magit-checkout-file ()
  "Checkout current file from REV."
  (interactive)

  (let* ((file (magit-current-file)))

    (ivy-read "Checkout " (magit-list-refnames)
              :action (lambda (rev)
                        (interactive)
                        (magit-with-toplevel
                          (magit-run-git "checkout" rev "--" file)))
              :caller 'counsel-magit)))


(provide 'magit-counsel)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; magit-counsel.el ends here
