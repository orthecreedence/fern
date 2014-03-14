(in-package :fern)

(defun test ()
  (let ((scheduler1 (create-scheduler))
        (scheduler2 (create-scheduler)))

    (define-process printer (process)
      (format t "---------------------~%")
      (receive process
        ((x) (format t "~a~%" x))))

    (define-process counter (process)
      (receive process
        ((msg 0 _)
         (send :printer (format nil "~a: ~a" (self) msg))
         (terminate process))
        ((msg num id)
         (send :printer (format nil "~a: ~a" (self) msg))
         (send id (format nil "~a, (~a ~a)" msg (self) (1- num)) (1- num) (self)))))

    (register :printer (spawn printer))
    (register :counter2 (spawn counter))
    (register :counter1 (spawn counter ("hai" 3 :counter2)))

    (sleep 1)
    (stop-scheduler scheduler1)
    (stop-scheduler scheduler2)
    (terminate :printer)))

(defun test-stress ()
  (let ((scheduler1 (create-scheduler))
        (scheduler2 (create-scheduler))
        (scheduler3 (create-scheduler))
        (scheduler4 (create-scheduler)))

    (define-process printer (process)
      (format t "---------------------~%")
      (receive process
        ((x) (format t "~a~%" x))))

    (define-process blabber (process)
      (receive process
        ((x)
         (send :printer (format nil "res: ~a" x)))
        (("send" 16)
         (send :adder 16))
        (("send" num)
         (send :adder num)
         (send :blabber "send" (1+ num)))))

    (define-process muncher (process)
      (receive process
        (((list x y))
         (send :adder "munched" (list (+ x 2) (+ y 2))))))

    (define-process adder (process)
      (receive process
        (("munched" (list x y))
         (send :blabber (* x y)))
        ((x)
         (send :muncher (list x 3)))))

    (register :printer (spawn printer))
    (register :muncher (spawn muncher))
    (register :adder (spawn adder))
    (register :blabber (spawn blabber ("send" 0)))

    (sleep 1)
    (stop-scheduler scheduler1)
    (stop-scheduler scheduler2)
    (stop-scheduler scheduler3)
    (stop-scheduler scheduler4)

    (terminate :blabber)
    (terminate :adder)
    (terminate :muncher)
    (terminate :printer)))

