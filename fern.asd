(asdf:defsystem fern
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "A concurrency framework for Common Lisp"
  :depends-on (#:alexandria
               #:cl-async-future
               #:cl-async 
               #:bordeaux-threads
               #:jpl-queues
               #:optima
               #:log4cl)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("package"))
   (:file "process" :depends-on ("config" "util"))
   (:file "scheduler" :depends-on ("process" "util"))
   (:file "test" :depends-on ("scheduler"))))

