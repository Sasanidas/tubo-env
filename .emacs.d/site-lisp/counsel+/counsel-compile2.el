;;; counsel-compile.el --- Select a Makefile target with counsel

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/counsel-compile
;; Package-Version: 0.1.0
;; Version: 0.1
;; Package-Requires: ((counsel "1.5.3") (projectile "0.11.0"))
;; Keywords: makefile

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A call to `counsel-compile' will give you a `counsel' selection of this directory
;; Makefile's targets.  Selecting a target will call `compile' on it.

;;; Code:

(require 'counsel)

(defgroup counsel-compile nil
  "Select a Makefile target with counsel."
  :group 'convenience)

(defcustom counsel-compile-do-save nil
  "If t, save all open buffers visiting files from Makefile's directory."
  :type 'boolean
  :group 'counsel-compile)

(autoload '-uniq "dash")

(defun makefile/get-target-list (file)
  "Get list of targets from FILE."
  (let ((r-match-targets
         (rx bol (group (+ (not (any "#
"))) ) ":" (? (: space (+ nonl))) eol))
        (r-match-target
         (rx (group (+ (not (any space))))))
        targets)

    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward r-match-targets nil t)
          (let ((str (match-string 1))
                (pos 0))
            (PDEBUG "TARGETS: " str
              "BEG: " (match-beginning 0)
              "END: " (match-end 0))
            (while (string-match r-match-target str pos)
              (push (match-string 1 str) targets)
              (setq pos (match-end 0)))


            ;; (unless (string-match "^\\." str)
            ;;   (push str targets))
            ))))

    targets))

(defun counsel-make (&optional makefile)
  "Use `counsel' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile"
  (interactive)
  (PDEBUG "MK: " makefile
    "PFX:" current-prefix-arg)

  (let* ((files (cond
                 ((not makefile) '("GNUmakefile" "makefile" "Makefile"))
                 ((listp makefile) makefile)
                 ((stringp makefile) (list makefile))
                 (t (error "Oops: %s" makefile))))
         (targets (flatten-list (mapcar 'makefile/get-target-list files))))

    (PDEBUG "FILES: " files)

    (unless targets
      (error "No target in %s" default-directory))

    (PDEBUG "TARGET: " targets)

    ;; save buffers before calling make
    (let* ((regex (format "^%s" default-directory))
           (buffers
            (cl-remove-if-not
             (lambda (b)
               (let ((name (buffer-file-name b)))
                 (and name
                      (string-match regex (expand-file-name name)))))
             (buffer-list))))
      (mapc
       (lambda (b)
         (with-current-buffer b
           (save-buffer)))
       buffers))


    (ivy-read "Targets " (-uniq (sort targets 'string<))
              :action (lambda (x)
                        (compile (format "make -j%d %s"
                                         (if current-prefix-arg
                                             (if (listp current-prefix-arg)
                                                 (car current-prefix-arg)
                                               current-prefix-arg)
                                           (yc/get-cpu-number))
                                         x) ))
              :caller 'counsel-make)))

(defun counsel-ninja ()
  "Use `counsel' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile"
  (interactive)
  (PDEBUG "ENTER.")
  (let ((r-match-targtes (rx bol "build all:" (* space) (? "phony") (group (+? nonl)) eol))
        (targets '("all")))

    (with-temp-buffer
      (insert-file-contents "build.ninja")
      (goto-char (point-min))

      (unless (search-forward-regexp r-match-targtes nil t)
        (error "Error while parsing targets from ninja"))

      (setq targets (append targets (s-split " " (match-string 1)))))

    (unless targets
      (error "No target in %s" default-directory))

    (PDEBUG "TARGET: " targets)

    (ivy-read "Targets " (-uniq (sort targets 'string<))
              :action (lambda (x)
                        (compile (format "ninja %s" x) ))
              :caller 'counsel-ninja)))

;;;###autoload
(defun counsel-compile2 (arg)
  "Use `counsel' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile"
  (interactive "P")
  (PDEBUG "PFX: " arg)

  (cond
   ((or (file-exists-p "Makefile")
        (file-exists-p "makefile")
        (file-exists-p "GNUmakefile"))
    (counsel-make))
   ((file-exists-p "build.ninja")
    (counsel-ninja))

   (t (error "Compile method not support"))
   ))

;;;###autoload
(defun counsel-compile-projectile ()
  "Call `counsel-compile' for `projectile-project-root'."
  (interactive)
  (require 'projectile)
  (let ((makefile (expand-file-name
                   "Makefile"
                   (projectile-project-root))))
    (counsel-make
     (and (file-exists-p makefile) makefile))))

(provide 'counsel-compile2)

;;; counsel-compile.el ends here
