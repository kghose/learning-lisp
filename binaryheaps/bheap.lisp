#|
The "public" operations we need are
- Push value
- Pop value
- Peek at minimum (root)
- Decrease value of a node: 
    This is needed for our application (path finding) in case we get to the same state 
    (nodes represent states) via a different path that is better. 
    We don't need increase value, because we would simply discard a path that gets to 
    a previously visited state more expensively.

Using tree representation for learning

Internally:

Push operation requires insertion of new node at "end" of tree (next)
  - requires sink-down
Pop operation requires deletion of last node in tree
  - requires swim-up
Decrease value can be implemented as 
  - find the node
  - decrease it's value 
  - swim up the node

In a later stage we're going to incorporate a hash table with the binary heap
to enable node finding (for decreasing value) 

|#

(defstruct bheap root (node-count 0 :type integer))
(defstruct payload key data)
(defstruct bnode payload parent lchild rchild)
(defun key (n) (payload-key (bnode-payload n)))
;; For this prototype implementation, we are not going to bother with
;; 'data' but it's the actual information we are queuing. It's also
;; going to be the key for our hash table

(defun push-bheap (bh payload)
  (swim-up bh (insert-bheap bh payload)))

(defun pop-bheap (bh)
  (let ((pv (bnode-payload (bheap-root bh))))
    (sink-down bh (bheap-root (delete-last (swap-first-last bh))))
    pv))

(defun decrease (bh key data)) ; not going to be implemented this round

;; Create a new node at the end of the heap
;; return the inserted node
(defun insert-bheap (bh payload)
  (let* ((parent (parent-of-next bh))
	 (new-node (make-bnode :payload payload :parent parent)))
    ; (unless (null parent) (format t "~A~%" (key parent)))
    (cond
	   ((null parent) (setf (bheap-root bh) new-node))	; root node
	   (t (if (null (bnode-lchild parent))
		  (setf (bnode-lchild parent) new-node)
		  (setf (bnode-rchild parent) new-node))))
    (incf (bheap-node-count bh))
    new-node))

(defun parent-of-next (bh)
  (find-node (bheap-root bh) (floor (+ 1 (bheap-node-count bh)) 2)))


;; Use the binary representation trick to thread through the
;; tree to a given node.
;; If the node does not exist, we create it - needed for insert
;; No bounds checking
; root - the root of the tree
; index - the index number of the node (1 = root)
(defun find-node (root index)
  (multiple-value-bind
	(n-idx rem) (floor index 2)
    (if (= n-idx 0)
	root
	(get-child (find-node root n-idx) rem))))

(defun get-child (nd ch)
  (if (= ch 0) (bnode-lchild nd) (bnode-rchild nd)))

;; Swim a node up the tree
; bh - binary heap root. This is only because we return the binary heap
; nd - the node we are considering
; returns bh
(defun swim-up (bh nd)
  (unless (null (bnode-parent nd)) ; already at top
    (when (> (key (bnode-parent nd)) (key nd))
      (swim-up bh (rot-up nd)))))

;; swap payloads of node and parent
;; return parent
(defun rot-up (nd)
  (rotatef (bnode-payload nd) (bnode-payload (bnode-parent nd)))
  (bnode-parent nd))

; returns bh
(defun swap-first-last (bh)
  (rotatef (bnode-payload (bheap-root bh)) (bnode-payload (last-node bh)))
  bh)

(defun last-node (bh)
  (find-node (bheap-root bh) (bheap-node-count bh)))

(defun delete-last (bh)
  (let* ((ln (last-node bh))
	 (pn (bnode-parent ln)))
    (cond
      ((null pn) (setf (bnode-payload (bheap-root bh)) nil)) ; root node
      ((if (null (bnode-rchild pn))
	   (setf (bnode-lchild pn) nil)
	   (setf (bnode-rchild pn) nil)))))
  (decf (bheap-node-count bh))
  bh)

;; Sink a node down the tree
; bh - binary heap root. This is only because we return the binary heap
; nd - the node we are considering
; returns bh
(defun sink-down (bh nd)
  (unless (leaf-node? nd)
    (let ((sm-ch (smaller-child nd)))
      (when (> (key nd) (key sm-ch))
        (sink-down bh (rot-down nd sm-ch))))))

(defun leaf-node? (nd)
  (and (null (bnode-lchild nd)) (null (bnode-rchild nd))))

; Return the node object that is the smaller of the two children
; Return nil if there are no children
; expects tree to obey binary heap property
(defun smaller-child (nd)
  (cond
    ((leaf-node? nd) nil)
    ((null (bnode-rchild nd)) (bnode-lchild nd))  ; bheap, left child must exist
    ((> (key (bnode-lchild nd)) (key (bnode-rchild nd))) (bnode-rchild nd))
    (t (bnode-lchild nd))))

;; swap parent and smaller child. Return smaller child
(defun rot-down (nd sm-ch)
  (rotatef (bnode-payload nd) (bnode-payload sm-ch))
  sm-ch)


;; http://stackoverflow.com/a/7382977
;; Not quite as clean as I want. The / and \ print oddly
(defmethod print-object ((bh bheap) out)
  (print-tree out (bheap-root bh)))

; Based on http://stackoverflow.com/a/19484210
(defun print-tree (out nd &optional rchild (prefix ""))
  "Recurse depth first into tree and print a line when we get to a leaf"
  (when (bnode-rchild nd)
    (print-tree out (bnode-rchild nd) t (concatenate `string prefix (next-prefix rchild))))
  (format out "~A~A---- ~A~%" prefix (if rchild " /" " \\") (key nd))
  (when (bnode-lchild nd)
    (print-tree out (bnode-lchild nd) nil (concatenate `string prefix (next-prefix (not rchild))))))

(defun next-prefix (flag)
  (if flag "        " " |      "))
