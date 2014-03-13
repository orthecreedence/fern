(in-package :fern)

(defvar *process-queue* (make-queue)
  "Holds a queue of active processes.")

(defclass switcher ()
  ((name :accessor switcher-name :initarg :name :initform "slappy"
    :documentation "Holds the switcher's name.")
   (active :accessor switcher-active :initarg :active :initform nil
    :documentation "Whether or not this switcher should be running.")
   (thread :accessor switcher-thread :initarg :thread :initform nil
    :documentation "Holds a reference to the switcher's thread.")))

(defun process (main-function)
  "Create a new process and queue it for execution."
  (let ((process (make-process main-function :active t)))
    (activate-process process)
    process))

(defun switch-main (switcher)
  "Main switcher loop."
  (unless (switcher-active switcher)
    (return-from switch-main nil))
  (if (jpl-queues:empty? *process-queue*)
      (as:delay (lambda () (switch-main switcher)))
      (let ((process (jpl-queues:dequeue *process-queue*)))
        (handler-case
          (run-process process)
          (t (e) (log:error "switcher (~a): error running process: ~a" (switcher-name switcher) e)))
        (as:delay (lambda () (switch-main switcher))))))

(defun create-switcher (&key (name (random 999)))
  "Create a threaded process switcher."
  (let* ((switcher (make-instance 'switcher :name name :active t))
         (thread (bt:make-thread (lambda ()
                                   (log:debug "switcher ~a starting" (switcher-name switcher))
                                   (as:with-event-loop (:catch-app-errors t)
                                     (switch-main switcher))
                                   (log:debug "switcher ~a ending" (switcher-name switcher))))))
    (setf (switcher-thread switcher) thread)
    switcher))

(defun stop-switcher (switcher &key force)
  "Forcibly kill a switcher."
  (setf (switcher-active switcher) nil)
  (when force
    (bt:destroy-thread (switcher-thread switcher))))

(defun activate-process (process)
  "Mark a process for execution."
  (when (process-active process)
    (jpl-queues:enqueue process *process-queue*)))

(defun message (process message)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (enqueue-message process message)
  (jpl-queues:enqueue process *process-queue*))

