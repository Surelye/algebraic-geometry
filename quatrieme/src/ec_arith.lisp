(defpackage #:ec-arith
  (:use #:cl)
  (:export #:add-points
           #:scalar-product))


(in-package #:ec-arith)


(defun add-points (P Q modulo)
  (when (equal P 'INF) (return-from add-points Q))
  (when (equal Q 'INF) (return-from add-points P))
  (let ((Px (car P)) (Py (cadr P))
        (Qx (car Q)) (Qy (cadr Q))
        (Rx) (Ry) (frac))
    (cond
      ((/= Px Qx) (setq frac (* (- Qy Py)
                                (cadr (aux:ext-gcd (mod (- Qx Px) modulo)
                                                    modulo)))
                        Rx (- (* frac frac) (+ Px Qx))
                        Ry (+ (- Py) (* frac (- Px Rx)))))
      (t (cond
           ((= Py Qy) (setq frac (* 3 Px Px
                                    (cadr (aux:ext-gcd (mod (* 2 Py) modulo)
                                                        modulo)))
                            Rx (- (* frac frac) (* 2 Px))
                            Ry (- (* frac (- Px Rx)) Py)))
           (t (return-from add-points 'INF)))))
    (list (mod Rx modulo) (mod Ry modulo))))


(defun scalar-product (n P modulo)
  (let ((result 'INF) (addend P))
    (aux:while (not (zerop n))
      (when (= 1 (logand n 1))
        (setq result (add-points addend result modulo)))
      (setq addend (add-points addend addend modulo)
            n (ash n -1)))
    result))
