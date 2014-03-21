(in-package :fern)

(define-condition bad-message (error) ()
  (:documentation "Thrown when a message contains non-serializable objects."))

(alexandria:define-constant +symbol-key-value+ "%SYM%"
  :test 'string=
  :documentation "The hash key given when serializing symbols.")

(alexandria:define-constant +keyword-key-value+ "%KEY%"
  :test 'string=
  :documentation "The hash key given when serializing keywords.")

(defclass message ()
  ((args :accessor message-args :initarg :args :initform nil
    :documentation "The arguments this message holds.")))

(defun messagep (message)
  "Determine if given object is a message."
  (typep message 'message))

(defun make-message (args)
  "Create a new message from the given arguments."
  (let ((args (copy-message args)))
    (make-instance 'message :args args)))

(defun copy-message (message)
  "Deep copy a message to remove ALL potential shared state. Only works with
   atoms, lists, sequences, and hash tables (basically anything you could
   serialize into JSON)."
  (recurse-tree
    message
    (lambda (x)
      (cond ((numberp x) x)
            ((stringp x) (copy-seq x))
            ((symbolp x) x)
            ((processp x) (id x))
            (t (error 'bad-message))))))

;; encode keywords into an object we'll be looking for on deserialize
(defmethod yason:encode ((sym keyword) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element +keyword-key-value+ (string sym)))))

;; encode symbols into an object we'll be looking for on deserialize
(defmethod yason:encode ((sym symbol) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element +symbol-key-value+ (string sym)))))

(defun serialize-message (message)
  "Serialize a message into a string we can send to a remote machine."
  (with-output-to-string (s)
    (yason:encode (message-args message) s)))

(defun deserialize-message (message-str)
  "Deserialize a message from a string. Looks for the specific object structures
   that hols keywords/symbols and decodes them."
  (let ((message (yason:parse message-str))
        (generate-symbols (lambda (x)
                            (when (hash-table-p x)
                              (let ((sym (gethash +symbol-key-value+ x))
                                    (key (gethash +keyword-key-value+ x)))
                                (cond (sym (intern sym))
                                      (key (intern key :keyword))))))))
    (make-message (recurse-tree message 'identity :replace generate-symbols))))

