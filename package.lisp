(defpackage :fern
  (:use :cl :cl-async-future :optima)
  (:export :messagep
           :message-args

           :register

           :processp
           :id
           :spawn
           :terminate
           :send
           :with-messages
           :define-process
           :receive
           :after
           :self
           :process-ready-p
           :process-active-p
           :process-queued-p

           :create-scheduler
           :stop-scheduler))

