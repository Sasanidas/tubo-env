;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: iPhone_Crash_Log.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;; Copyright (C) 2014 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <yangyingchao@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;

(defun command-output-to-string (cmd &rest args)
  "Execute a command command and return result as string.
args should be a list, but to make caller's life easier, it can accept one atom instead of a
  list."
  (let ((cmd-output (with-output-to-string
                      (with-current-buffer standard-output
                        (apply #'process-file
                               cmd
                               nil (list t t) nil
                               args)))))
    (ansi-color-apply cmd-output)))

(defun parse-uuid (uuids)
  "description"
  (let ((tbl (make-hash-table :test 'equal))
        (r-uuid-parser (rx "UUID:" (*? blank)
                           (group (+ hex) ) "-" ;; 1st
                           (group (+ hex)) "-" ;; 2nd
                           (group (+ hex)) "-" ;; 3rd
                           (group (+ hex)) "-" ;; 4th
                           (group (+ hex))     ;; 5th
                           (*? blank) "(" (group (+? anything)) (*? blank)")"
                           ))
        (pos 0))

    (defun p-uuid-iter ()
      "description"
      (when (string-match r-uuid-parser uuids pos)
        (puthash (downcase (concat (match-string 1 uuids)
                                   (match-string 2 uuids)
                                   (match-string 3 uuids)
                                   (match-string 4  uuids)
                                   (match-string 5  uuids)))
                 (match-string 6  uuids) tbl)
        (setq pos (match-end 0))
        (p-uuid-iter)))
    (p-uuid-iter)
    tbl))

(defun iphone-crash (binary lf)
  "description"
  (interactive)
  (let* ((app binary)
         (img (if (file-directory-p binary)
                  (if (string-match (rx (group (+? alnum)) ".app.dSYM") binary) ;; input is .dysm dir
                      (progn
                        (setq app (match-string 1 binary))
                        (format "%s/Contents/Resources/DWARF/%s" binary app))
                    (if (string-match (rx (group (+ alnum)) ".app") binary) ;; .binary dir
                        (progn
                          (setq app (match-string 1 binary))
                          (format "%s/%s" binary app))
                      binary))
                binary))
         (r-match-uuid (rx "Binary Images:" (+? anything) "<" (group (+ hex)) "> "))
         (r-match-trigger (rx "Thread" (* blank) (+ digit) (* blank) "Crashed:" (* blank)
                              (group (+? anything)) "

"
                              ))
         (r-match-crash-entry (rx line-start
                                  (group (+? digit))
                                  (+ blank) (group (+? (or alnum "."))) (+ blank)
                                  (group (: "0x" (+ alnum)))
                                  (+ blank) (group (: "0x" (+ alnum)))
                                  (+ blank) "+" (+ blank) (+ digit)
                                  eol))
         (utbl (parse-uuid (command-output-to-string "dwarfdump"  "--uuid" img)))
         (logs (with-temp-buffer
                 (insert-buffer (find-file-noselect lf))
                 (buffer-substring-no-properties
                  (point-min) (point-max))))
         (crash-log nil)
         (arch nil)
         (result nil)
         fstr uuid
         )

    ;; Check arch
    (if (string-match r-match-uuid logs 0)
        (setq uuid (match-string 1 logs)
         arch (gethash uuid utbl)))
    (if (not arch)
        (error "Can't find proper arch!!"))

    (setq fstr (format "UUID: %s, Arch: %s\n" uuid arch))
    ;; check crash thread
    (if (string-match r-match-trigger logs 0)
        (setq crash-log (match-string 1 logs)))

    (if (not crash-log)
        (error "Can't find proper crash log"))

    (defun entry-iter (pos)
      (if (and (< pos (length crash-log))
               (string-match r-match-crash-entry crash-log pos))
          (let* ((id (match-string 1 crash-log))
                 (mod (match-string 2 crash-log))
                 (ldaddr (match-string 4 crash-log))
                 (addr (match-string 3 crash-log))
                 (bt (if (string= mod app)
                         (command-output-to-string "xcrun" "atos" "-o" img "-arch" arch
                                                   "-l" ldaddr
                                                   addr )
                       "System Library, out of scope..\n")))
            (setq result (cons (format "%s:%s:(%s-%s) -- %s" id mod addr ldaddr bt) result))
            (entry-iter (1+ (match-end 0))))))
    (entry-iter 0)
    (dolist (e (nreverse result))
      (setq fstr (concat fstr e)))
    fstr))

(provide 'iPhone_Crash_Log)
;;;;; iPhone_Crash_Log.el ends here
