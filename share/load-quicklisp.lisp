(unless (find-package :quicklisp)
  (let ((ql (find-if #'probe-file
                     (map 'list (lambda (setup) (merge-pathnames setup (user-homedir-pathname)))
                          '("quicklisp/setup.lisp" ".quicklisp/setup.lisp" "Quicklisp/setup.lisp")))))
    (if ql
        (progn
          (format t "~&Loading QL from ~A~&" ql)
          (load ql))
        (progn
          (format *error-output* "~&QL not found!~&")
          (error "No quicklisp")))))
