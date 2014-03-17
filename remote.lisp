(in-package :fern)

(defun setup-parser (sock)
  "Makes a parser (called as a function) that reads in a message and once
   complete sends it to the appropriate local node.")

(defun listen-remote (&key bind port (*default-comm-port*) event-cb)
  "Listen for messages from other nodes."
  (as:tcp-server bind port
    (lambda (sock data)
      (let ((parser (as:socket-data sock)))
        (funcall parser data)))
    (lambda (ev)
      (if (and (typep ev 'as:event-error)
               (not (typep ev 'as:event-info)))
          (log:error ev)
          (log:debug ev))
      (when event-cb (funcall event-cb ev)))
    :connect-cb 'setup-parser))

(defun send-remote (id/host message)
  "Send a message remotely.")
