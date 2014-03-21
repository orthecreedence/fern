(in-package :fern)

(defclass scheduler ()
  ((name :accessor scheduler-name :initarg :name :initform "slappy"
    :documentation "Holds the scheduler's name.")
   (active :accessor scheduler-active :initarg :active :initform nil
    :documentation "Whether or not this scheduler should be running.")
   (thread :accessor scheduler-thread :initarg :thread :initform nil
    :documentation "Holds a reference to the scheduler's thread.")
   (lock :accessor scheduler-lock :initform (bt:make-lock)
    :documentation "Lock for updating the scheduler's state.")))

(defun scheduler-main-loop (scheduler)
  "Main scheduler loop."
  (unless (bt:with-lock-held ((scheduler-lock scheduler))
            (scheduler-active scheduler))
    (return-from scheduler-main-loop nil))
  (if (jpl-queues:empty? *process-queue*)
      (as:delay (lambda () (scheduler-main-loop scheduler)) :time .00001)
      (let* ((process (jpl-queues:dequeue *process-queue*)))
        (handler-case
          (run-process process)
          (t (e) (log:error "scheduler (~a): error running process: ~a" (scheduler-name scheduler) e)))
        (as:delay (lambda () (scheduler-main-loop scheduler))))))

;;; ----------------------------------------------------------------------------
;;; main API
;;; ----------------------------------------------------------------------------

(defun create-scheduler (&key (name (random 999)))
  "Create and start a threaded process scheduler."
  (let* ((scheduler (make-instance 'scheduler :name name :active t))
         (thread (bt:make-thread (lambda ()
                                   (log:debug "scheduler ~a starting" (scheduler-name scheduler))
                                   (as:with-event-loop (:catch-app-errors t)
                                     (scheduler-main-loop scheduler))
                                   (log:debug "scheduler ~a ending" (scheduler-name scheduler))))))
    (setf (scheduler-thread scheduler) thread)
    scheduler))

(defun stop-scheduler (scheduler &key force)
  "Stop a scheduler. If :force t is passed, will kill the scheduler's thread."
  (bt:with-lock-held ((scheduler-lock scheduler))
    (setf (scheduler-active scheduler) nil))
  (when force
    (bt:destroy-thread (scheduler-thread scheduler))))

