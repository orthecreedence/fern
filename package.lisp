(defpackage :fern
  (:use :cl :cl-async-future :optima)
  (:export :id
           :with-messages
           :receive
           :process-ready-p
           :process-active-p
           :process-queued-p

           :create-scheduler
           :stop-scheduler
           :process
           :define-process
           :do-spawn
           :spawn
           :terminate
           :send))

