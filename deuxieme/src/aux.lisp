(defpackage #:aux
  (:use :cl))

(in-package #:aux)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun mod-expt (base power divisor)
  (setq base (mod base divisor))
  (do ((product 1))
      ((zerop power) product)
    (do () ((oddp power))
      (setq base (mod (* base base) divisor)
            power (ash power -1)))
    (setq product (mod (* product base) divisor)
          power (1- power))))


(defun miller-rabin (n &optional (k 10))
  (when (or (= 2 n) (= 3 n)) (return-from miller-rabin t))
  (when (or (< n 2) (= 0 (logand n 1))) (return-from miller-rabin))
  (let* ((n-pred (1- n)) (bound (- n-pred 2)) (t-val n-pred) (s 0) (round 0) (x))
    (while (= 0 (logand t-val 1)) (setq s (1+ s) t-val (ash t-val -1)))
    (do () (nil)
      (tagbody next-iteration
         (when (= k round) (return-from miller-rabin t))
         (setq x (mod-expt (+ 2 (random bound)) t-val n))
         (when (or (= 1 x) (= n-pred x))
           (incf round) (go next-iteration))
         (do ((iter 0 (1+ iter))) ((= iter (1- s)) (return-from miller-rabin))
           (setq x (mod (* x x) n))
           (when (= 1 x) (return-from miller-rabin))
           (when (= n-pred x)
             (incf round) (go next-iteration)))))))


(defun div (dividend divider)
  (multiple-value-bind (quotient)
      (floor dividend divider) quotient))


(defun extended-euclidean-algorithm (f-num s-num)
  (let ((s 0) (s-old 1) (t-val 1) (t-old 0)
        (r s-num) (r-old f-num) (quotient))
    (while (not (zerop r))
      (setq quotient (div r-old r))
      (psetq r-old r
             r (- r-old (* quotient r)))
      (psetq s-old s
             s (- s-old (* quotient s)))
      (psetq t-old t-val
             t-val (- t-old (* quotient t-val))))
    (list r-old s-old t-old)))


(defun is-power-residue (b p-char power)
  (when (or (= 2 power) (= 3 power))
    (= 1 (mod-expt b (div (1- p-char) power) p-char))))


(defun compute-legendre (a p)
  (when (zerop (mod a p))
    (return-from compute-legendre 0))
  (if (is-power-residue a p 2) 1 -1))


(defun find-k-i (a-i q p)
  (do ((k 0 (1+ k))) ((= 1 (mod-expt a-i (* (expt 2 k) q) p)) k)))


(defun get-inv (a p)
  (cadr (extended-euclidean-algorithm a p)))


(defun seq-sqrt-Zp (a p)
  (when (= -1 (compute-legendre a p)) (return-from seq-sqrt-Zp))
  (let ((b) (k-is) (k-i -1) (r-i) (m 0) (q (1- p))
        (up-bound (- p 2)) (a-i a) (a-i-next a)
        (pow-r-i) (2-inv) (2-exp))
    (while (zerop (logand q 1))
      (setq m (1+ m)
            q (ash q -1)))
    (while (/= -1 (compute-legendre (setq b (+ 2 (random up-bound))) p)))
    (while (not (zerop k-i))
      (setq k-i (find-k-i a-i-next q p)
            k-is (cons k-i k-is))
      (psetq a-i-next (mod (* a-i-next (mod-expt b (expt 2 (- m k-i)) p)) p)
             a-i a-i-next))
    (setq k-is (cdr k-is)
          r-i (mod-expt a-i (ash (1+ q) -1) p))
    (do ((i (length k-is) (1- i))) ((= i 0) r-i)
      (setq pow-r-i (- m (car k-is) 1))
      (if (< pow-r-i 0)
          (setq 2-inv (mod (get-inv 2 p) p)
                2-exp (mod-expt 2-inv (- pow-r-i) p)))
          (setq 2-exp (mod-expt 2 pow-r-i p))
      (setq 2-inv (mod (get-inv (mod-expt b 2-exp p) p) p)
            r-i (mod (* r-i 2-inv) p)
            k-is (cdr k-is)))))
