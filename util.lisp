(in-package :fern)

(defun make-queue ()
  "Makes a thread-safe FIFO queue (unbounded)."
  (make-instance 'jpl-queues:synchronized-queue
                 :queue (make-instance 'jpl-queues:unbounded-fifo-queue)))

(defun cons-map (fn list)
  "A looper that can handle cons cells."
  (let* ((cur (car list))
         (next (cdr list))
         (res nil))
    (loop while cur do
      (cond ((eq (type-of next) 'cons)
             (setf res (append res (list (funcall fn cur)))
                   cur (car next)
                   next (cdr next)))
            (t
             (setf res (append res (cons (funcall fn cur) (funcall fn next)))
                   cur nil))))
    res))

