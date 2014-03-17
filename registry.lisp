(in-package :fern)

(define-condition process-not-found () ()
  (:documentation "Thrown when an operation happens on a missing process."))

(define-condition remote-not-allowed (error) ()
  (:documentation "Thrown when an operation happens on a node that isn't local."))

(defclass registry ()
  ((ids :accessor registry-ids :initform (make-hash-table :test #'eq)
    :documentation "Hash list of id -> process lookups.")
   (names :accessor registry-names :initform (make-hash-table :test #'equal)
    :documentation "Hash list of registered names -> process lookups.")
   (reverse-names :accessor registry-reverse-names :initform (make-hash-table :test #'eq)
    :documentation "Holds process -> names lookups.")
   (locks :accessor registry-locks :initform (list :ids (bt:make-lock)
                                                   :names (bt:make-lock)
                                                   :reverse-names (bt:make-lock))
    :documentation "Holds locks for the registry's fields.")))

(defvar *registry* (make-instance 'registry)
  "Holds the main process registry.")

(defmacro reglock ((registry field) &body body)
  "Makes locking syntax easier."
  `(bt:with-lock-held ((getf (registry-locks ,registry) ,field))
     ,@body))

(defun split-id (id/host)
  "Convert an id in the format 1234@host to (1234 . \"host\"). If host is not
   present, it will be nil. If id/host is an integer, it's assumed to be local
   and just returned."
  (when (numberp id/host)
    (return-from split-id (cons id/host nil)))
  (let* ((pos (position #\@ id/host))
         (id (if pos
                 (subseq id/host 0 pos)
                 id/host))
         (id-num (ignore-errors (parse-integer id)))
         (id (or id-num id))
         (host (when pos (subseq id/host (1+ pos)))))
    (cons id host)))

(defun is-remote (id)
  "Determine if a process id points to a remote machine."
  (let* ((id/host (split-id id))
         (host (cdr id/host)))
    (and host (not (string= host (machine-instance))))))

(defun lookup (id/name)
  "Lookup a process by id."
  (cond ((typep id/name 'process)
         id/name)
        ((typep id/name 'number)
         (reglock (*registry* :ids)
           (gethash id/name (registry-ids *registry*))))
        (t
         (reglock (*registry* :names)
           (gethash (format-process-name id/name) (registry-names *registry*))))))

(defmacro with-process-lookup ((process &key errorp) &body body)
  "Gets a process by id (if it's not already a process object)."
  `(let* ((,process (lookup ,process)))
     ,(if errorp
          `(if process
               (progn ,@body)
               (error 'process-not-found))
          `(when ,process
             ,@body))))

(defun format-process-name (name)
  "Standard function for formatting process ids."
  (string-downcase (string name)))

(defun register-process-id (id process)
  "Ties a process ID to its process."
  (reglock (*registry* :ids)
    (setf (gethash id (registry-ids *registry*)) process)))

(defun unregister-process-id (id)
  "Ties a process ID to its process."
  (reglock (*registry* :ids)
    (remhash id (registry-ids *registry*))))

(defun register (name process)
  "Register a process name"
  (with-slots (names reverse-names) *registry*
    (reglock (*registry* :names)
      (setf (gethash (format-process-name name) names) process))
    (reglock (*registry* :reverse-names)
      (push name (gethash process reverse-names)))))

(defun unregister (name)
  "Unregister a process name"
  (with-slots (names reverse-names) *registry*
    (reglock (*registry* :names)
      (remhash (format-process-name name) names))))

(defun reverse-name-lookup (process &key clear)
  "Get a list of process names given a process."
  (with-slots (reverse-names) *registry*
    (reglock (*registry* :reverse-names)
      (prog1 (copy-tree (gethash process reverse-names))
        (when clear
          (remhash process reverse-names))))))

