(defpackage #:ec-arith
  (:use :cl))

(in-package #:ec-arith)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun div (dividend divider)
  (multiple-value-bind (quotient)
      (floor dividend divider) quotient))


(defun extended-gcd (a b)
  (let ((s 0) (old-s 1)
        (r b) (old-r a)
        (quotient nil) (bezout-t nil))
    (while (not (zerop r))
      (setq quotient (div old-r r))
      (psetq old-r r r (- old-r (* quotient r))
             old-s s s (- old-s (* quotient s))))
    (if (zerop b)
        (setq bezout-t b)
        (setq bezout-t (div (- old-r (* old-s a)) b)))
    (list old-r old-s bezout-t)))


(defun add-points (P Q modulo)
  (when (eql P 'INF)
    (return-from add-points Q))
  (when (eql Q 'INF)
    (return-from add-points P))
  (let ((Px (car P)) (Py (cadr P))
        (Qx (car Q)) (Qy (cadr Q))
        (Rx nil) (Ry nil)
        (frac nil))
    (cond
      ((/= Px Qx) (setq frac (* (- Qy Py)
                                (cadr (extended-gcd (mod (- Qx Px) modulo) modulo)))
                        Rx (- (* frac frac) (+ Px Qx))
                        Ry (+ (- Py) (* frac (- Px Rx)))))
      (t (cond
           ((= Py Qy) (setq frac (* (* 3 Px Px)
                                    (cadr (extended-gcd (mod (* 2 Py) modulo)
                                                        modulo)))
                            Rx (- (* frac frac) (* 2 Px))
                            Ry (- (* frac (- Px Rx)) Py)))
           (t (return-from add-points 'INF)))))
    (list (mod Rx modulo) (mod Ry modulo))))


(defun scalar-product (n P modulo)
  (let ((result 'INF) (addend P))
    (while (not (zerop n))
      (when (= 1 (mod n 2))
        (setq result (add-points addend result modulo)))
      (setq addend (add-points addend addend modulo)
            n (ash n -1)))
    result))


(defun generate-subgroup (generator modulo)
  (let* ((accum generator) (subgroup (list accum)))
    (while (not (eql 'INF accum))
      (setq accum (add-points accum generator modulo)
            subgroup (cons accum subgroup)))
    subgroup))
