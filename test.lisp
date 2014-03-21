(defpackage :fern-test
  (:use :cl :cl-async-future :fern))
(in-package :fern-test)

(defun test ()
  (let ((scheduler1 (create-scheduler))
        (scheduler2 (create-scheduler)))

    (define-process printer (process)
      (format t "---------------------~%")
      (receive
        ((x) (format t "~a~%" x))))

    (define-process counter (process)
      (after 2000
        (format t "YOU'RE DEAD, LAMPSHADE ~a~%" (self)))
      (receive
        (('terminate)
         (format t "terminating ~a~%" (self))
         (terminate process))
        ((msg 0 id)
         (send :printer (format nil "~a: ~a" (self) msg))
         (send id 'terminate)
         (as:with-delay (4)
           (format t "terminate that fucker~%")
           (send (self) 'terminate)))
        ((msg num id)
         (send :printer (format nil "~a: ~a" (self) msg))
         (send id (format nil "~a, (~a ~a)" msg (self) (1- num)) (1- num) (self)))))

    (register :printer (spawn printer))
    (spawn counter ("hai" 3 (spawn counter)))

    (sleep 1)
    (stop-scheduler scheduler1)
    (stop-scheduler scheduler2)
    (terminate :printer)))

(defun test-stress ()
  (let ((scheduler1 (create-scheduler))
        (scheduler2 (create-scheduler)))

    (define-process printer (process)
      (format t "---------------------~%")
      (receive
        ((x) (format t "~a~%" x))))

    (define-process blabber (process)
      (receive
        ((x)
         (send :printer (format nil "res: ~a" x)))
        (("send" 0)
         (send :adder 0))
        (("send" num)
         (send :adder num)
         (send :blabber "send" (1- num)))))

    (define-process muncher (process)
      (receive
        (((list x y))
         (as:with-delay (1)
           (send :adder "munched" (list (+ x 2) (+ y 2)))))))

    (define-process adder (process)
      (receive
        (("munched" (list x y))
         (send :blabber (* x y)))
        ((x)
         (send :muncher (list x 3)))))

    (register :printer (spawn printer))
    (register :muncher (spawn muncher))
    (register :adder (spawn adder))
    (register :blabber (spawn blabber ("send" 1024)))

    (sleep 5)
    (stop-scheduler scheduler1)
    (stop-scheduler scheduler2)

    (terminate :blabber)
    (terminate :adder)
    (terminate :muncher)
    (terminate :printer)))

