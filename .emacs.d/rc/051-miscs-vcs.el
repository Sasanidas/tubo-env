;;; 051-miscs-vcs.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

 ;; ************************** magit ****************************

(use-package magit
  :pin melpa
  :commands (magit-blame-addition)
  :bind (:map ctl-x-map
              ("gs" . magit-status)
              ("gf" . magit-find-file-other-window)
              ("gb" . magit-blame-addition))
  :custom
  (magit-revert-buffers t)
  (magit-commit-show-diff nil)
  (magit-push-always-verify nil)
  (magit-revision-insert-related-refs t)
  (magit-revision-show-gravatars nil)
  (magit-revision-headers-format "Author:     %aN <%aE>\nDate: %ad\n")
  (git-commit-summary-max-length 72)
  (magit-log-arguments '("-n256" "--graph" "--decorate" "--color"))
  (magit-patch-arguments (quote ("--output-directory=patches")))
  (magit-merge-arguments
   (quote
    ("--strategy=recursive" "--strategy-option=ignore-space-change")))
  (magit-visit-ref-behavior
   (quote (focus-on-ref create-branch checkout-any check-branch)))
  (git-commit-major-mode 'text-mode)
  (magit-status-sections-hook
   (quote
    (
     magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpulled-from-upstream
     magit-insert-unpulled-from-pushremote
     magit-insert-unpushed-to-upstream
     magit-insert-unpushed-to-pushremote)))

  :hook ((magit-find-file .       (lambda () ;; Guess proper encoding for files.
                                    (setq buffer-read-only nil)
                                    (recode-region (point-min) (point-max) 'undecided 'utf-8)
                                    (setq buffer-read-only t)))


         ;; (magit-status-mode .       (lambda ()
         ;;                              (when (executable-find "arc")
         ;;                                (require 'magit-arc)
         ;;                                (magit-arc-mode))
         ;;                              (when (try-require 'magit-svn)
         ;;                                (define-key magit-mode-map (kbd "N") 'magit-svn-popup))))
         )
  :config
  (progn
    ;; (magit-define-popup-option 'magit-merge-popup
    ;;   ?x "Strategy options" "--strategy-option=" #'read-from-minibuffer)
    (magit-auto-revert-mode 1)))

(use-package magit-files
  :pin melpa
  :defer t
  :config
  (progn
    (define-key magit-file-mode-map "\C-xg" nil)
    ;; (magit-define-popup-action 'magit-file-popup
    ;;   ?f "Find file"  'magit-find-file-other-window)
)
  )


(use-package magit-log :bind ((;; ,(kbd "C-x g l")
                               "gl". magit-log-buffer-file)))
(use-package git-timemachine
  :bind ((;; ,(kbd "C-x g t")
          "gt". git-timemachine)))

(use-package magit-counsel
  :bind ((;; ,(kbd "C-x g c")
          "gc".  counsel-magit-checkout)
         (;; ,(kbd "C-x g C")
          "gC". counsel-magit-checkout-file)))


(defun yc/magit-stash-format-patch (func stash)
  "Advice for `magit-stash-format-patch'.
Call FUNC with ARGS."
  (let* ((dir (format "%s/patches/" (magit-toplevel)))
         (tmp (unless (file-directory-p dir)
                (mkdir dir)))
         (default-directory dir)
         (output (concat dir (magit-rev-format "0001-%f.patch" stash))))

    (funcall func stash)
    (message "Patch save as %s." output)))

(yc/eval-after-load
  "magit-stash"
  (advice-add 'magit-stash-format-patch :around #'yc/magit-stash-format-patch))


(use-package magit-git
  :commands (magit-git-string magit-revision-files))

(use-package magit-process
  :commands (magit-process-file))

(defun yc/git-copy-file-path ()
  "Copy path of current visited file."
  (interactive)
  (let ((url (magit-git-string "config" "remote.origin.url"))
        (root (magit-toplevel)))

    (unless url (error "Not in a git repo"))

    (kill-new
     (concat (replace-regexp-in-string
              (rx "git@github.com:" (group (+? nonl)) ".git")
              "https://github.com/\\1"
              url)
             "/blob/master/"
             (and root (file-relative-name buffer-file-name root))
             ))))

(use-package magit-auto-revert
  :commands (magit-auto-revert-mode))

(use-package mule :commands (recode-region))

(use-package git-commit
  :defer t
  :hook (git-commit-mode . (lambda ()
                        (setq fill-column 72)
                        (turn-on-flyspell)
                        (goto-char (point-min))
                        (PDEBUG "CURR-POINT: " (point))
                        ))
  :config
  (progn
    (substitute-key-definition
     'kill-buffer  'git-commit-abort git-commit-mode-map)
    (substitute-key-definition
     'ido-kill-buffer  'git-commit-abort git-commit-mode-map)))

 ;; svn

(use-package dsvn
  :commands (svn-status))

(provide '051-miscs-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 051-miscs-vcs.el ends here
