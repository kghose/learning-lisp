;; Code to create a test tree

; k is the number of levels to do
(defun create-test-tree (k)
  (let ((root (make-bnode :payload (make-payload :key 1 :data 1))))
    (create-children root (- k 1))
    (make-bheap
     :root root
     :node-count (- (expt 2 k) 1))))

(defun create-children (parent levels-left)
  (if (> levels-left 0)
    (let*
      ((left-child
        (make-bnode :payload (make-payload :key (* 2 (key parent))) :parent parent))
       (right-child
        (make-bnode :payload (make-payload :key (+ 1 (key left-child))) :parent parent)))
      (setf (bnode-lchild parent) left-child)
      (setf (bnode-rchild parent) right-child)
      (create-children left-child (- levels-left 1))
      (create-children right-child (- levels-left 1)))))
