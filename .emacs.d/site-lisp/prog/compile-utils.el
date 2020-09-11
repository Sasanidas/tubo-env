;;; compile-utils.el --- Utilities related to compile/make/cmake -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/arthas>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: August 14, 2020
;; Modified: August 14, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/arthas/compile-utils
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'executable)
(require 'compile)
(require 'projectile)
(require '02-functions)

(require 'cl-macs)

 ;; cmake..

(defun yc/yank-cmake-command (&optional arg)
  "Yank cmake command.
If ARG is true, build for release version, otherwise for debug version."
  (interactive "P")
  (insert (concat (mapconcat 'identity
             '("cmake"
              "-DCMAKE_VERBOSE_MAKEFILE=ON"
              "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
              "-DCMAKE_BUILD_TYPE="
              )
             " ")
                  (if arg "RelWithDebInfo" "Debug"))))

(defun yc/cmake-generate-fake-project ()
  "Generate fake CMakeLists.txt for current project."
  (interactive)
  (let* ((project-root-suggestion (projectile-project-root))
         (choices (list
                   (format "Import project root %s" project-root-suggestion)
                   "Import project by selecting root directory interactively."))
         (action-index (cl-position
                        (completing-read (format "%s is not part of any project. Select action: "
                                                 (buffer-name))
                                         choices
                                         nil
                                         t)
                        choices
                        :test 'equal))
         (project-root (case action-index
                         (0 project-root-suggestion)
                         (1 (read-directory-name "Select workspace folder to add: "
                                                 (or project-root-suggestion default-directory)
                                                 nil
                                                 t))
                         (t nil))))
    (PDEBUG "project-root: " project-root)
    (unless project-root
      (error "Project root is not set"))

    (setq project-root (file-name-as-directory project-root))


    (let* ((cmake-file (expand-file-name "CMakeLists.txt" project-root ))
           (build-dir  (expand-file-name "build" project-root))
           (exclude-file (expand-file-name "/.git/info/exclude" project-root))
           (gen-file (expand-file-name "gen-compile-db.sh" build-dir))
           (command (concat (executable-find "bash") " " gen-file))

           (default-directory build-dir)
           (buffer-out (concat "*interpreting - " (file-name-nondirectory gen-file) "*"))
           ignore-list)

      (unless (file-exists-p cmake-file)
        (push "/CMakeLists.txt" ignore-list))

      (unless (file-exists-p (expand-file-name ".lsp-conf" project-root))
        (push "/.lsp-conf" ignore-list))

      (unless (file-directory-p build-dir)
        (make-directory build-dir)
        (push "/build/" 'ignore-list ))

      (unless (file-exists-p gen-file)
        (with-temp-file gen-file
          (insert "#!/bin/bash

touch ../.lsp-conf

function gen_compile_db ()
{
    rm -rf C*

    cat <<EOF > ../CMakeLists.txt
# -*- mode: cmake -*-
cmake_minimum_required(VERSION 2.6)
file(GLOB_RECURSE FAKE_SOURCES
  src/*.c
  src/*.cpp
  src/*.cc
  contrib/*.c
  )

include_directories(\\${CMAKE_SOURCE_DIR}/src/include)

add_executable(fake_target \\${FAKE_SOURCES} )

EOF

cmake .. -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

}

gen_compile_db
"))
        (shell-command (format "chmod +x %s" gen-file)))

      (PDEBUG "LIST" ignore-list)

      (when (file-exists-p exclude-file)
        (with-temp-file exclude-file
          (insert-file-contents exclude-file)
          (goto-char (point-max))
          (insert (mapconcat 'identity ignore-list "\n"))))

      (PDEBUG "COMMAND: " command)
        (set (make-local-variable 'executable-command) command)
        (compilation-start command t (lambda (_x) buffer-out)))))





(defun do-compile ()
  "Save buffers and start compile with ARG."
  (interactive)
  (let ((source-window (get-buffer-window))
        (compile-window nil)
        (compile-cmd (get-compile-command)))
    (unless compile-cmd
      (error "Failed to get command to compile"))

    (PDEBUG "Compile CMD: " compile-cmd)
    (save-some-buffers t)
    (setq compilation-read-command nil)
    (when (not (get-buffer-window "*compilation*"))
      (setq compile-window (split-window-vertically))
      (select-window compile-window)
      (get-buffer-create "*compilation*")
      (switch-to-buffer "*compilation*")
      (select-window source-window))
    (setq compilation-read-command nil)
    (make-local-variable 'compile-command)
    ;; (when (string= system-type "darwin")
    ;;   (setenv "CC" "clang")
    ;;   (setenv "CXX" "clang++"))
    (compile compile-cmd)))

(defun do-run ()
  "Save buffers and start compile with ARG."
  (interactive)
  (let ((source-window (get-buffer-window))
        (compile-window nil)
        (cmd (get-run-command)))
    (unless cmd
      (error "Failed to get command to run"))

    (PDEBUG "RUN CMD: " cmd)

    (save-some-buffers t)
    (setq compilation-read-command nil)
    (when (not (get-buffer-window "*compilation*"))
      (setq compile-window (split-window-vertically))
      (select-window compile-window)
      (get-buffer-create "*compilation*")
      (switch-to-buffer "*compilation*")
      (select-window source-window))
    (setq compilation-read-command nil)
    (make-local-variable 'compile-command)
    ;; (when (string= system-type "darwin")
    ;;   (setenv "CC" "clang")
    ;;   (setenv "CXX" "clang++"))
    (compile cmd)))

(defun compile-or-recompile (arg)
  "Do compile or recompile, based on ARG."
  (interactive "P")
  (let ((buffer (get-buffer "*compilation*")))
    (if buffer
        (with-current-buffer buffer
          (recompile arg))
      (do-compile))))

(provide 'compile-utils)

;;; compile-utils.el ends here
