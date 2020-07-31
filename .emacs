;;; .emacs --- init-file of emacs.
;;; Commentary:
;;; Code:

 ;; Load all configuration and packages.
(let ((ts-init (current-time))
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil)
        (default-directory  )
      (debug-on-error t))
  (message "Start loading configurations...\n")

  (push (expand-file-name "~/.emacs.d/rc") load-path)

  ;; Add customized paths to the front of load-path
  (dolist (path (list
                 (expand-file-name "site-lisp/"  user-emacs-directory)
                 (expand-file-name "straight/build"  user-emacs-directory)
                  ))
  (let ((default-directory path))
    (normal-top-level-add-subdirs-to-load-path)))

  ;; Never load the `central-custome-file'.
  (setq custom-file "~/.emacs.d/rc/10-emacs-custome.el")
  (if (file-exists-p custom-file)
      (delete-file custom-file nil))

  (dolist (fn (directory-files "~/.emacs.d/rc/" t "^[0-9]+.*?\.el$"))
    (condition-case load-message
        (load fn)
      ('error  (message "Skipped file %s -- %s" fn load-message))))

  (message "\nFinished startup in %.2f seconds.\n"
           (float-time (time-since ts-init))))

;;; .emacs ends here
