(in-package :fern)

(define-condition process-not-found () ())
(define-condition process-not-defined () ())

(defvar *process-queue* (make-queue)
  "Holds a queue of active processes.")

(defvar *next-id* 0
  "Holds numerical IDs for processes.")
(defvar *next-id-lock* (bt:make-lock)
  "Lock for process ID generator.")

(defvar *process-definitions* (make-hash-table :test 'eq)
  "Holds name -> process definition mappings.")
(defvar *process-definitions-lock* (bt:make-lock)
  "Holds LOCK FOR name -> process definition mappings.")

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
  (when (and (process-active-p process)
             (not (process-queued-p process)))
    (jpl-queues:enqueue process *process-queue*)))

(defun generate-process-id ()
  "Get a new process ID (unique)."
  (bt:with-lock-held (*next-id-lock*)
    (incf *next-id*)))

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

(defun process (main-function)
  "Create a new process and queue it for execution."
  (let* ((id (generate-process-id))
         (process (make-process main-function :id id :active t)))
    (register-process-id id process)
    (activate-process process)
    process))

(defmacro define-process (process-type (bind-process) &body body)
  "Convenience wrapper around process function."
  `(progn
     (bt:with-lock-held (*process-definitions-lock*)
       (setf (gethash ',process-type *process-definitions*)
             (lambda ()
               (process (lambda (,bind-process) ,@body)))))))

(defun do-spawn (process-type)
  "Spawn a new process of the given type."
  (let ((process-definition (bt:with-lock-held (*process-definitions-lock*)
                              (gethash process-type *process-definitions*))))
    (if process-definition
        (funcall process-definition)
        (error 'process-not-defined))))

(defmacro spawn (process-type &optional (args nil args-supplied-p))
  (let ((process-var (gensym "process")))
    `(let ((,process-var (do-spawn ',process-type)))
       (when ,args-supplied-p
         (send ,process-var ,@args))
       ,process-var)))

(defun terminate (process)
  "Quit a process."
  (with-process-lookup (process)
    (wipe-process process)
    (unregister-process-id (process-id process))
    (dolist (name (reverse-name-lookup process :clear t)) 
      (unregister name))))

(defun send (process &rest args)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (with-process-lookup (process)
    (enqueue-message process args)
    (activate-process process)))

