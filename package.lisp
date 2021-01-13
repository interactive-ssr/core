(defpackage issr-core
  (:use #:cl)
  (:import-from #:plump
                children
                remove-child
                doctype-p
                text-node-p
                comment-p
                element-p
                has-child-nodes
                tag-name
                attributes
                attribute
                insert-before
                family
                serialize
                text)
  (:import-from #:global-vars
                define-global-var
                define-global-parameter)
  (:export
   -clients-
   -on-connect-hook-
   -on-disconnect-hook-
   *ws-port*
   *socket*
   *first-time*
   *id*
   hash-keys
   generate-id
   clean
   diff-dom))

