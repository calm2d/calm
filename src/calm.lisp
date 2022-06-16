(in-package #:calm)

(defun entry ()
  (let ((core-path sb-ext:*core-pathname*)
        (cwd-path (uiop:getcwd)))
    (let ((core-file "./core.lisp"))
      ;; load core file
      (if (probe-file core-file)
          (load core-file)
          (load (merge-pathnames core-file core-path))))))
