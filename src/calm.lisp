(in-package #:calm)

(defvar *calm-version* (uiop:run-program "git describe --always --tags" :output '(:string :stripped t)))

(defun entry ()
  (let ((core-path sb-ext:*core-pathname*)
        (cwd-path (uiop:getcwd)))
    (let ((core-file "./core.lisp"))
      ;; load core file
      (if (probe-file core-file)
          (load core-file)
          (load (merge-pathnames core-file core-path))))))
