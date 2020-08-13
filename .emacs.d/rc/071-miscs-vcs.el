;;; 071-miscs-vcs.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:
(use-package mule  :commands (recode-region))

 ;; ************************** magit ****************************

(use-package git-commit
  :defer t
  :hook (git-commit-mode . (lambda ()
                        (setq fill-column 72)
                        (turn-on-flyspell)
                        (goto-char (point-min))
                        (PDEBUG "CURR-POINT: " (point))))
  :config
  (progn
    (substitute-key-definition
     'kill-buffer  'git-commit-abort git-commit-mode-map)
    (substitute-key-definition
     'ido-kill-buffer  'git-commit-abort git-commit-mode-map))
  :custom
  (git-commit-summary-max-length 72))

(use-package magit-repos
  :custom
  (magit-repository-directories `((,(expand-file-name "~") . 0)))
  :bind (:map ctl-x-map
              ("gL" . magit-list-repositories)))

(use-package magit-files
  :defer t
  :config
  (progn
    (define-key magit-file-mode-map "\C-xg" nil)))


(use-package magit-log  :bind (:map ctl-x-map ("gl" . magit-log-buffer-file)))

(use-package git-timemachine
  :ensure t
  :bind (:map ctl-x-map ("gt" . git-timemachine)))

(use-package magit-counsel
  :bind (:map ctl-x-map
              ("gc" . counsel-magit-checkout)
              ("gU" . counsel-magit-checkout-file)
              ("gw" . magit-find-file-in-other-worktree)))

(use-package magit-auto-revert
  :commands (magit-auto-revert-mode))

(defun yc/counsel-git-grep ()
  "Description."
  (interactive)
  (let ((m (point-marker))
        (init (aif (symbol-at-point) (symbol-name it))))
    (counsel-git-grep init)
    (yc/push-stack m)))

(defvar yc/auto-merge-func nil
  "Function try to merge file automatically.
This function accept file name as argument, and return t if file is merged automatically.")

(use-package magit-ediff
  :commands (magit-unmerged-files))

(defun yc/git-add-current-file ()
  "Add curent file to state."
  (interactive)

  (unless (buffer-file-name)
    (error "Not a file"))

  (if (buffer-modified-p)
      (save-buffer))

  (shell-command-to-string (concat "git add " (buffer-file-name)))

  (let* ((default-directory (magit-toplevel))
         (cands (magit-unmerged-files)))
    (if cands
        (let* ((next-file
                (if current-prefix-arg
                    (ivy-read "Choose NextFile: " cands)
                  (car cands))))

          ;; Do smerge-ediff when auto-merge-func is not specified, or failed.
          (when (or (not yc/auto-merge-func)
                    (not (funcall yc/auto-merge-func next-file)))
            (with-current-buffer (find-file next-file)
              (smerge-ediff))))

      (message "All files are cleared in this folder."))))


(use-package magit
  :ensure t
  :commands (magit-blame-addition magit-revision-files magit-toplevel)
  :bind (:map ctl-x-map
              ("gs" . magit-status)
              ("gf" . magit-find-file-other-window)
              ("gb" . magit-blame-addition)
              ("gg" . 'yc/counsel-git-grep)
              ("ga" . 'yc/git-add-current-file))
  :hook ((magit-process-mode . goto-address-mode))
  :custom
  (magit-diff-refine-hunk t)  ;; show granular diffs in selected hunk
  (magit-revert-buffers t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-push-always-verify nil)
  (magit-revision-insert-related-refs t)
  (magit-revision-show-gravatars nil)
  (magit-revision-headers-format "Author:     %aN <%aE>\nDate: %ad\n")
  (magit-no-confirm nil)
  (magit-delete-by-moving-to-trash nil)
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

  :hook ((magit-find-file
          .
          (lambda () ;; Guess proper encoding for files.
            (setq buffer-read-only nil)
            (recode-region (point-min) (point-max) 'undecided 'utf-8)
            (setq buffer-read-only t)))
         )
  :config
  (progn
    (magit-auto-revert-mode 1)))

(use-package dsvn
  :commands (svn-status))

(provide '071-miscs-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 071-miscs-vcs.el ends here
