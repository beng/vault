; alias find-file to ff
(defun eshell/ff (&rest args)
  (apply #'find-file args))
