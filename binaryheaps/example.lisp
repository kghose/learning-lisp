(load "bheap.lisp")
(load "create-test-tree.lisp")

; (profile make-children)

(setf *print-circle* t)

(when t
  (format t "~%~%Create test tree and show off printing:~%~%")
  (let ((bh (create-test-tree 5)))
    (print-tree t (bheap-root bh))
    ;(format t "%~A~%" bh)
    ; (print bh)
    ))

(when nil
  (format t "~%~%Create test tree and find nodes:~%~%")
  (let ((bh (create-test-tree 5)))
    (print-tree t (bheap-root bh))
    (format t "~%~%Find node 5:~%~%")
    (print-tree t (find-node (bheap-root bh) 5))
    (format t "~%~%Find node 1:~%~%")
    (print-tree t (find-node (bheap-root bh) 1))
    (format t "~%~%Replace node 10 with -22:~%~%")
    (setf (node-value (find-node (bheap-root bh) 10)) -22)
    (print-tree t (bheap-root bh))))

(defun push-tests (bh val)
  (unless (= val -1)
    (progn
     (format t "~%Inserted ~d:~%" val)
     (push-bheap bh (make-payload :key val))
     (print-tree t (bheap-root bh))
     (push-tests bh (- val 1)))))

(when t
  (format t "~%~%Push into bheap~%~%")
  (let ((bh (make-bheap)))
    (push-tests bh 10)))

(defun pop-tests (bh val)
  (unless (= val -1)
    (progn
     (format t "~%Popped ~d:~%" (pop-bheap bh))
     (print-tree t (bheap-root bh))
     (pop-tests bh (- val 1)))))

(when t
  (format t "~%~%Pop from bheap~%~%")
  (let ((bh (create-test-tree 5)))
    (pop-tests bh 10)))

;
; (let ((bh (create-test-tree 1)))
;   (a-run bh 2))
;
; (let ((bh (make-bheap :root (make-node) :node-count 0)))
;   (bheap-insert bh 10)
;   ; (bheap-insert bh 11)
;   ; (print-tree (bheap-root bh))
;   ; (bheap-insert bh 12)
;   ; (bheap-insert bh 13)
;   )

; (setf bh (create-test-tree 1))
