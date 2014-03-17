(defpackage :fern
  (:use :cl :cl-async-future :optima)
  (:export :messagep
           :message-args

           :processp
           :id
           :define-process
           :spawn
           :terminate
           :send
           :with-messages
           :receive
           :process-ready-p
           :process-active-p
           :process-queued-p

           :create-scheduler
           :stop-scheduler))

