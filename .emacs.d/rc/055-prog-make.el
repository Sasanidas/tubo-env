;;; 058-prog-make.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

;; utilities..

 ;; *********** CMake Mode ***************

(use-package cmake-font-lock
  :commands (cmake-font-lock-activate)
  :hook ((cmake-mode . cmake-font-lock-activate)))

(use-package company-cmake

  :commands (company-cmake)
  :hook ((cmake-mode .
                     (lambda ()
                       (yc/add-company-backends-with-yasnippet company-cmake)))))

(use-package cmake-mode
  :commands (cmake-mode cmake-help cmake-help-list-commands)
  :mode (rx (or "CMakeList.txt" "CMakeLists.txt" (: ".cmake" buffer-end)))
  :bind (:map cmake-mode-map
              (;; (kbd "C-c h")
               "h"
               . cmake-help)
              ("\C-cl" . cmake-help-list-commands)
              ("\C-cu" .  unscreamify-cmake-buffer))
  :custom
  (cmake-tab-width 4))


(cdsq yc/cmake-command-template
  (mapconcat 'identity
             '("cmake"
              "-DCMAKE_VERBOSE_MAKEFILE=ON"
              "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
              "-DCMAKE_BUILD_TYPE=%s"
              )
             " "))

(defun yc/yank-cmake-command (arg)
  "Description."
  (interactive "P")
  (insert (format yc/cmake-command-template (if arg "RelWithDebInfo" "Debug"))))

(yc/add-compile-unit 'cmake 88
  (when (or (file-exists-p "CMakeLists.txt")
            (equal file "CMakeLists.txt")
            (equal ext ".cmake"))

    (lambda ()
      (let* ((build-type (if current-prefix-arg "RelWithDebInfo" "Debug"))
             (build-dir (format "%s/cmake_build_%s" default-directory
                                build-type))
             (cmd-template "cd %s && cmake -DCMAKE_VERBOSE_MAKEFILE=ON  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=%s .. && make -j%d"))
        (if (file-directory-p build-dir)
            (if (yes-or-no-p "build directory exists, recreate it? ")
                (progn
                  ;; clear existing caches...
                  (dolist (fn '("CMakeCache.txt" "Makefile" "CMakeFiles"))
                    (let ((ffn (format "%s/%s" build-dir fn)))
                      (when (file-exists-p ffn)
                        (if (file-directory-p ffn)
                            (delete-directory ffn t)
                          (delete-file ffn)))))

                  (unless (file-exists-p build-dir)
                    (mkdir build-dir))

                  (format cmd-template
                          build-dir build-type (yc/get-compiling-threads)))
              (format "cd %s && make -j%d" build-dir (yc/get-compiling-threads)))
          (progn
            (mkdir build-dir)
            (format cmd-template
                    build-dir build-type (yc/get-compiling-threads))))))))

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
           (compilation-error-regexp-alist executable-error-regexp-alist)
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


(use-package compile
  :commands (compile)
  :init
  (progn
    (custom-set-variables
     '(compilation-scroll-output t)))
  :bind (:map compilation-mode-map

              ([S-f9] ;;(kbd "<S-f9>")
               . (lambda ()
                   (interactive)
                   (let ((cur (point)))
                     (goto-char (point-min))
                     (unless (search-forward-regexp (rx (* space) "error:" (* space)) nil t)
                       (goto-char cur)))))
              ("<f9>" . 'next-error))

  :config
    (advice-add 'compile :around #'yc/compile-adv)
    (advice-add 'recompile :before #'yc/recompile)

    (add-hook 'compilation-finish-functions
      (lambda (buf str)
        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, F11 to goto next error.")

          ;;no errors, make the compilation window go away in 0.5 seconds
          ;;        (run-at-time 5.0 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!")))))

(defun yc/compile-adv (func command &optional comint)
  "Advice for 'compile'.
Call FUNC which is 'compile with ARGS."
  (funcall func (format "LANG=C %s" command) comint))

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


(yc/set-keys
 (list (cons (kbd "<f6>") 'do-compile)
       (cons (kbd "<f7>") 'do-run)
       (cons (kbd "<C-f6>") (lambda (arg)
                              (interactive "P")
                              (let ((buffer (get-buffer "*compilation*")))
                                (if buffer
                                    (with-current-buffer buffer
                                      (recompile arg))
                                  (do-compile arg)))))))


(defun yc/recompile (&rest args)
  "Advice for `recompile'.
Call FUNC with ARGS."
  (unless
      (string-match-p (rx "make" (+ space) (or "install" "default" "all" "-k" "-j"))
                      compile-command)
    (if (yes-or-no-p (format "recompile with command %s" compile-command))
        compile-command
      (error "Action abort"))))


  ;; *********** Makefile ***************

(use-package make-mode
  :mode (((rx bol (or "Makefile" "makefile"
                      (: "." (+ alnum) (: (+ alnum) ".mk")) )
              eol)
          . makefile-mode))
  :hook ((makefile-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))))

(use-package counsel-compile2

  :commands (counsel-compile2 counsel-compile-projectile counsel-make)
  :bind ((;; ,(kbd "<M-f6>")
          [M-f6]. counsel-compile2)
         ;; (kbd "ESC <f6>")
         ([27 f6] . counsel-compile2)
         (;; ,(kbd "<M-S-f6>")
          [M-S-f6]. counsel-compile-projectile)))



(use-package ninja-mode
  ;; :ensure nil
  )

(yc/add-compile-unit 'ninja 77
  (when (file-exists-p "build.ninja")
    (lambda ()"ninja")))


(provide '058-prog-make)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 058-prog-make.el ends here
