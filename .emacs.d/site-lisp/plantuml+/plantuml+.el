;;; company-plantuml.el --- company-mode completion backend for plantuml -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Free Software Foundation, Inc.

;; Author: Chen Bin <chenbin DOT sh AT gmail>
;; Version: 0.2

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; company-plantuml offers completions for module names, variable names and
;; commands used by plantuml.  And their descriptions.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'plantuml-mode)
(require 'ivy)


(defun company-plantuml--candidates (prefix)
  "Return candiates for `PREFIX'."
  (all-completions prefix plantuml-kwdList))

;;;###autoload
(defun company-plantuml (command &optional arg &rest ignored)
  "`company-mode' completion backend for plantuml.
plantuml is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-plantuml))
    (init (unless (equal major-mode 'plantuml-mode)
            (error "Major mode is not plantuml-mode")))
    (prefix (and (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-plantuml--candidates arg))
    ))


(defvar plantuml-indent-offset 4)

(defvar plantuml-indent-regexp-start
  (rx bol (* space)
      (or
       (: (? (: (or "abstract" "interface") (+ space)))
          (or "class" "enum" "partition") (+ space) (+? nonl) "{")
       (: (or "if" "elseif" "else") (* space) "(")
       (: "while" (* space) "(")
       "note"
       )
      ))

(defvar plantuml-indent-regexp-end
  (rx bol (* space)
      (or
       "}" "endif" "endwhile" "end note" "else"
       )
   )
  )


;;;###autoload
(defun plantuml-indent-line ()
  "Indent current line as plantuml code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at plantuml-indent-regexp-end)
          (progn
            (save-excursion
              (forward-line -1)
              (if (looking-at plantuml-indent-regexp-start)
                  (setq cur-indent (current-indentation))
                (setq cur-indent(- (current-indentation)
                                   plantuml-indent-offset))))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (cond
             ((looking-at plantuml-indent-regexp-start)
              (setq cur-indent (+ (current-indentation)
                                  plantuml-indent-offset)
                    not-indented nil))
             ((looking-at plantuml-indent-regexp-end)
              (setq cur-indent (current-indentation)
                    not-indented nil))
             ((bobp) (setq not-indented nil))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


(provide 'plantuml-+)
;;; plantuml+.el ends here
