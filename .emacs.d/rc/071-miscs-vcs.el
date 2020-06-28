;;; 071-miscs-vcs.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:
(use-package mule :commands (recode-region))

 ;; ************************** magit ****************************
(use-package magit-repos
  :custom
  (magit-repository-directories `((,(expand-file-name "~") . 0)))
  :bind (:map ctl-x-map
              ("gL" . magit-list-repositories)))

(use-package magit-files
  :pin melpa
  :defer t
  :config
  (progn
    (define-key magit-file-mode-map "\C-xg" nil)))


(use-package magit-log :bind (:map ctl-x-map ("gl" . magit-log-buffer-file)))
(use-package git-timemachine :bind (:map ctl-x-map ("gt" . git-timemachine)))
(use-package magit-counsel
  :bind (:map ctl-x-map
              ("gc" . counsel-magit-checkout)
              ("gU" . counsel-magit-checkout-file)))

(use-package magit-auto-revert
  :commands (magit-auto-revert-mode))


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
     'ido-kill-buffer  'git-commit-abort git-commit-mode-map)))

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

 ;; svn

(use-package dsvn
  :commands (svn-status))

(provide '071-miscs-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 071-miscs-vcs.el ends here
