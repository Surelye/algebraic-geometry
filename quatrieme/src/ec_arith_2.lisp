(defpackage #:ec-arith-2
  (:use :cl))

(in-package #:ec-arith-2)


(defmacro while (condition &body body)
  `(do () ((not ,condition))
     ,@body))


(defun get-binary (num) ; на самом деле, get-binary-reversed
  (when (zerop num) (return-from get-binary (make-array 1 :initial-element 0)))
  (let* ((len (do ((2^j 1 (ash 2^j 1)) (j 0 (1+ j)))
                  ((> 2^j num) j)))
         (bin (make-array len)))
    (do ((idx 0 (1+ idx))) ((= len idx) bin)
      (setf (svref bin idx) (logand num 1))
      (setq num (ash num -1)))))


(defun poly* (f s p) ; Полиномы подаются в обратном порядке, результат тоже в обратном
  (let* ((f-cond (lambda (seq)
                   (map 'vector #'(lambda (coef) (mod coef p)) seq)))
         (s-cond (lambda (seq)
                   (position-if #'(lambda (coef) (not (zerop coef)))
                                seq :from-end t)))
         (f-deg) (s-deg) (*-deg) (*-poly) (pos) (sum-idx))
    (setq f (funcall f-cond f) pos (funcall s-cond f))
    (when (null pos) (return-from poly* (make-array 1 :initial-element 0)))
    (setq f (subseq f 0 (1+ pos)) f-deg (1- (length f))
          s    (funcall f-cond s)   pos (funcall s-cond s))
    (when (null pos) (return-from poly* (make-array 1 :initial-element 0)))
    (setq s (subseq s 0 (1+ pos))
          s-deg (1- (length s)) *-deg (+ f-deg s-deg)
          *-poly (make-array (1+ *-deg) :initial-element 0))
    (do ((i 0 (1+ i))) ((> i f-deg))
      (when (not (zerop (svref f i)))
        (do ((j 0 (1+ j))) ((> j s-deg))
          (when (not (zerop (svref s j)))
            (setq sum-idx (+ i j))
            (setf (svref *-poly sum-idx) (mod (+ (svref *-poly sum-idx)
                                                 (* (svref f i)
                                                    (svref s j))) p))))))
    (subseq *-poly 0 (1+ (funcall s-cond *-poly)))))
