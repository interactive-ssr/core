(in-package #:issr-core)

(define-global-var -clients- (make-hash-table :test 'equalp)
  "Key: socket, Value: (list *request* page).
Before connecting by websocket, the key is the identifier.")

(define-global-parameter -on-connect-hook- (list)
  "Run each function in `on-connect-hook' after a socket connects.
Each function should take one socket as an argument.")

(define-global-parameter -on-disconnect-hook- (list)
  "Run each function in `on-disconnect-hook' before a socket disconnects.
Each function should take one socket as an argument.")

(defvar *ws-port* 443
  "The port to host the websocket server on.")

(defvar *id* 0
  "Used to identify the socket at the first connection.
Do NOT set globally; only bind dymaically.")

(defvar *socket* nil
  "The current socket being used.
Do NOT set globally; only bind dymaically.")

(defvar *first-time* t
  "T if it is the first time a connection is being made.
Do NOT set globally; only bind dynamically. This should
be the same as `(null `socket')'.")

(defun hash-keys (hash-table)
  (loop :for key :being :the :hash-keys :of hash-table
        :collect key))

(defun generate-id (&optional (length 9) &key (not-in (hash-keys -clients-)))
  "Genereate a random number that has LENGTH digits and is not a member of NOT-IN.
No leading zeros."
  (let ((id (+ (expt 10 (- length 1)) (random (- (expt 10 length) 1)))))
    (if (member id not-in)
        (generate-id length :not-in not-in)
        id)))

(defun clean (node)
  "Remove whitespace and comments from the plump dom."
  (loop for index from (- (length (children node)) 1) downto 0
        for child = (aref (children node) index)
        do (cond ((or (comment-p child)
                      (and (text-node-p child)
                           (str:emptyp (str:trim (text child)))))
                  (remove-child child))
                 ((and (has-child-nodes child)
                       (string/= (tag-name child) "pre"))
                  (clean child))))
  node)

(defun ensure-ids (node)
  "Ensures that plump dom NODE and all of its subtrees have the id attribute.
If an element does not have an id attribute, `ensure-ids' mutates it to have one.
Returns the possibly modified NODE."
  ;; ensure node has id
  (when (element-p node)
    (let ((id (attribute node "id")))
      (unless id
        (setf (attribute node "id")
              (symbol-name (gensym "I"))))))
  ;; apply to children
  (when (has-child-nodes node)
    (loop for child across (children node) do
      (ensure-ids child)))
  node)

(tailrec
 (defun descendant (node indexes)
   "Return the dom node which is the child of ancestor NODE.
INDEXES is a list of locations of the children list of NODE."
   (if (null indexes)
       node
       (descendant
        (aref (children node) (car indexes))
        (cdr indexes)))))

(tailrec
 (defun diff-dom (old-dom new-dom
                  ;; only for recursion
                  &optional (index 0) indexes instructions)
   "Return a list of instructions to update the dom of OLD-DOM to look like NEW-DOM.
OLD-DOM and NEW-DOM should both be plump virtual doms.
See issr.js for possible instructions.
Does not preserve OLD-DOM.
Every element in OLD-DOM should have an id attribute.

INDEXES: Reversed list of indexes to reach the current parent.
INDEX: (aref (children parent) INDEX) to get current node."
   (let ((old-tree (descendant old-dom (reverse indexes)))
         (new-tree (descendant new-dom (reverse indexes))))
     (cond
       ;; base case: no more tree to traverse
       ((and (null indexes) (>= index (length (children new-tree))))
        instructions)
       ;; move to next sibling of parent with no instructions
       ((and (>= index (length (children old-tree)))
             (>= index (length (children new-tree))))
        (diff-dom old-dom new-dom (+ (car indexes) 1)
                  (cdr indexes) instructions))
       ;; insert rest of children from new
       ((>= index (length (children old-tree)))
        (let* ((children (children new-tree))
               (length-children (length children))
               (insert-instructions
                 (loop for i from index to (- length-children 1)
                       collect
                       (list "insert"
                             (attribute old-tree "id")
                             0
                             "append"
                             (serialize (ensure-ids (aref children i)) nil)))))
          (diff-dom old-dom new-dom length-children indexes
                    (append instructions insert-instructions))))
       ;; remove rest of children from old
       ((>= index (length (children new-tree)))
        (let* ((children (children old-tree))
               (length-children (length children))
               (delete-instructions
                 (loop for i from (- length-children 1) downto index
                       for child = (aref children i)
                       collect
                       (if (element-p child)
                           (list "delete" (attribute child "id"))
                           (list "delete" (attribute old-tree "id") index)))))
          (diff-dom old-dom new-dom length-children indexes
                    (append instructions delete-instructions))))
       ;;; start comparing the current node
       (:else
        (let ((old-node (descendant old-tree (list index)))
              (new-node (descendant new-tree (list index))))
          (cond
            ;; move to the next sibling with no instructions
            ((or (doctype-p old-node)
                 (comment-p old-node)
                 (and (element-p old-node)
                      (gethash "noupdate" (attributes old-node) nil)
                      (element-p new-node)
                      (gethash "noupdate" (attributes new-node) nil)))
             (diff-dom old-dom new-dom (+ index 1) indexes instructions))
            ;; update text if current node is a text node
            ((text-node-p old-node)
             (diff-dom old-dom new-dom (+ index 1) indexes
                       (if (and (text-node-p new-node)
                                (string= (text old-node)
                                         (text new-node)))
                           instructions
                           (progn
                             (insert-before old-node nil)   ;add a nil to shift the node array
                             (append instructions
                                     (list (list "insert"
                                                 (attribute (parent old-node) "id")
                                                 1
                                                 "prepend"
                                                 (text new-node))))))))
            ;; add, delete, or replace
            ((or (not (eq (type-of old-node) (type-of new-node)))
                 (string/= (if (element-p old-node) (tag-name old-node) "")
                           (if (element-p new-node) (tag-name new-node) ""))
                 (and (element-p old-node)
                      (element-p new-node)
                      (gethash "id" (attributes old-node))
                      (gethash "id" (attributes new-node))
                      (string/= (gethash "id" (attributes old-node))
                                (gethash "id" (attributes new-node)))))
             (let ((diff-length (- (length (family new-node))
                                   (length (family old-node)))))
               (cond
                 ;; delete
                 ((or (< diff-length 0)
                      (attribute new-node "noupdate"))
                  (remove-child old-node) ;shift siblings
                  (diff-dom old-dom new-dom index indexes
                            (append instructions
                                    (list (list "delete" (attribute old-node "id"))))))
                 ;; add
                 ((or (< 0 diff-length)
                      (attribute old-node "noupdate"))
                  (insert-before old-node nil) ;shift siblings
                  (diff-dom old-dom new-dom (+ index 1) indexes
                            (append instructions
                                    (list (list "insert"
                                                (attribute old-node "id")
                                                0
                                                "before"
                                                (serialize (ensure-ids new-node) nil))))))
                 ;; replace
                 (:else
                  (diff-dom old-dom new-dom (+ index 1) indexes
                            (append instructions
                                    (list (list "mod"
                                                (attribute old-node "id")
                                                (list "outerHTML" (serialize (ensure-ids new-node) nil))))))))))
            ;; update attrs then descend into children
            (:else
             (let* ((update (when (element-p new-node)
                              (gethash "update" (attributes new-node))))
                    (new-attrs
                      (when (and (element-p old-node)
                                 (element-p new-node))
                        ;; id should never change
                        (setf (attribute new-node "id")
                              (attribute old-node "id"))
                        (remove-if #'null
                                   (mapcar (lambda (key)
                                             (let ((old-value (gethash key (attributes old-node) ""))
                                                   (new-value (gethash key (attributes new-node) "")))
                                               (when (or update
                                                         (string/= old-value new-value))
                                                 (list key new-value))))
                                           (union (hash-keys (attributes new-node))
                                                  (hash-keys (attributes old-node))
                                                  :test 'string=))))))
               (diff-dom old-dom new-dom 0 (cons index indexes)
                         (if new-attrs
                             (append instructions (list (append (list "mod" (attribute old-node "id")) new-attrs)))
                             instructions)))))))))))
