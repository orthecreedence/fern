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
               #:yason
               #:log4cl)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("package"))
   (:file "message" :depends-on ("package" "util"))
   (:file "registry" :depends-on ("util" "config"))
   (:file "remote" :depends-on ("config" "util" "message" "registry"))
   (:file "process" :depends-on ("config" "util" "message" "registry" "remote"))
   (:file "scheduler" :depends-on ("config" "process" "util"))
   (:file "test" :depends-on ("process" "scheduler"))))

