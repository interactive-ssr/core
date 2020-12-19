(defpackage issr-core
  (:use #:cl #:plump)
  (:export :clients
           :on-connect-hook
           :on-disconnect-hook
           :*ws-port*
           :*socket*
           :*first-time*
           :*id*
           :generate-id
           :clean
           :diff-dom))

