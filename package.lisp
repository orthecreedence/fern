(defpackage :fern
  (:use :cl :cl-async-future :optima)
  (:export :process-id
           :with-messages
           :receive
           :process-ready-p
           :process-active-p

           :create-scheduler
           :stop-scheduler
           :process
           :with-process
           :terminate
           :send))

