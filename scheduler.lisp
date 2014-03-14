(in-package :fern)

(defvar *process-queue* (make-queue)
  "Holds a queue of active processes.")

(defclass scheduler ()
  ((name :accessor scheduler-name :initarg :name :initform "slappy"
    :documentation "Holds the scheduler's name.")
   (active :accessor scheduler-active :initarg :active :initform nil
    :documentation "Whether or not this scheduler should be running.")
   (thread :accessor scheduler-thread :initarg :thread :initform nil
    :documentation "Holds a reference to the scheduler's thread.")
   (lock :accessor scheduler-lock :initform (bt:make-lock)
    :documentation "Lock for updating the scheduler's state.")))

(defun switch-main (scheduler)
  "Main scheduler loop."
  (unless (bt:with-lock-held ((scheduler-lock scheduler))
            (scheduler-active scheduler))
    (return-from switch-main nil))
  (if (jpl-queues:empty? *process-queue*)
      (as:delay (lambda () (switch-main scheduler)))
      (let* ((process (jpl-queues:dequeue *process-queue*)))
        (handler-case
          (run-process process)
          (t (e) (log:error "scheduler (~a): error running process: ~a" (scheduler-name scheduler) e)))
        (as:delay (lambda () (switch-main scheduler))))))

(defun activate-process (process)
  "Mark a process for execution."
  (when (process-active process)
    (jpl-queues:enqueue process *process-queue*)))

;;; ----------------------------------------------------------------------------
;;; main API
;;; ----------------------------------------------------------------------------

(defun create-scheduler (&key (name (random 999)))
  "Create a threaded process scheduler."
  (let* ((scheduler (make-instance 'scheduler :name name :active t))
         (thread (bt:make-thread (lambda ()
                                   (log:debug "scheduler ~a starting" (scheduler-name scheduler))
                                   (as:with-event-loop (:catch-app-errors t)
                                     (switch-main scheduler))
                                   (log:debug "scheduler ~a ending" (scheduler-name scheduler))))))
    (setf (scheduler-thread scheduler) thread)
    scheduler))

(defun stop-scheduler (scheduler &key force)
  "Forcibly kill a scheduler."
  (bt:with-lock-held ((scheduler-lock scheduler))
    (setf (scheduler-active scheduler) nil))
  (when force
    (bt:destroy-thread (scheduler-thread scheduler))))

(defun process (main-function &key name)
  "Create a new process and queue it for execution."
  (let ((process (make-process main-function :name name :active t)))
    (activate-process process)
    process))

(defmacro with-process ((bind-process &key name) &body body)
  "Convenience wrapper around process function."
  `(process (lambda (,bind-process) ,@body) :name ,name))

(defun message (process message)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (enqueue-message process message)
  (activate-process process))

(defun terminate (process)
  "Quit a process."
  (wipe-process process))

