(in-package :fern)

(defun test ()
  (let ((scheduler1 (create-scheduler))
        (scheduler2 (create-scheduler)))
    (with-process (:printer process)
      (format t "---------------------~%")
      (receive process
        ((x) (format t "~a~%" x))))
    (with-process (:process1 process)
      (receive process
        ((msg 0)
         (send :printer (format nil "~a: ~a" (process-id process) msg))
         (terminate process))
        ((msg num)
         (send :printer (format nil "~a: ~a" (process-id process) msg))
         (when (process-active-p (lookup :process1))
           (send :process1 (format nil "~a, (1 ~a)" msg (1- num)) (1- num))))))
    (send :process1 "hai" 3)
    (sleep 1)
    (stop-scheduler scheduler1)
    (stop-scheduler scheduler2)
    (terminate :printer)))

