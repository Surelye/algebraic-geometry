(defpackage #:ec-arith-2
  (:use :cl))

(in-package #:ec-arith-2)


(defvar *p* 2)


(defmacro while (condition &body body)
  `(do () ((not ,condition))
     ,@body))


(defun get-binary (num)
  (let* ((str (write-to-string num :base *p*))
         (len (length str)) (res))
    (setq res (make-array len :initial-contents (reverse str))
          res (map 'vector #'digit-char-p res))))


(defun poly*-machinerie (f s)
  (let* ((f-deg (1- (length f))) (s-deg (1- (length s)))
         (*-deg (+ f-deg s-deg)) (sum-idx) (pos)
         (*-poly (make-array (1+ *-deg) :initial-element 0)))
    (do ((i 0 (1+ i))) ((> i f-deg))
      (unless (zerop (svref f i))
        (do ((j 0 (1+ j))) ((> j s-deg))
          (unless (zerop (svref s j))
            (setq sum-idx (+ i j))
            (setf (svref *-poly sum-idx) (logxor (svref *-poly sum-idx) 1))))))
    (setq pos (position-if-not #'zerop *-poly :from-end t))
    (if (null pos) #(0)
        (subseq *-poly 0 (1+ pos)))))


(defun poly* (&rest polys)
  (reduce #'poly*-machinerie polys))


(defun ext-gcd (a b)
  (let ((s 0) (old-s 1) (r b) (old-r a)
        (quotient) (bezout-t))
    (while (not (zerop r))
      (setq quotient (floor old-r r))
      (psetq old-r r r (- old-r (* quotient r))
             old-s s s (- old-s (* quotient s))))
    (if (zerop b) (setq bezout-t 0)
        (setq bezout-t (floor (- old-r (* old-s a)) b)))
    (list old-r old-s bezout-t)))


(defun pdf (f s)
  (let* ((fc (copy-seq f))
         (deg-f (1- (length f))) (deg-s (1- (length s)))
         (deg-dif (- deg-f deg-s)) (quot))
    (when (< deg-dif 0) (return-from pdf (list #(0) f)))
    (setq quot (make-array (1+ deg-dif) :initial-element 0))
    (do ((k (- deg-f deg-s) (1- k)))
        ((= -1 k) (list quot (subseq fc 0 deg-s)))
      (setf (aref quot k) (aref f (+ deg-s k)))
      (do ((j (+ deg-s k -1) (1- j))) ((< j k))
        (setf (aref fc j) (logxor (aref f j) (* (aref quot k)
                                               (aref s (- j k)))))))))


(defun zero-poly? (f)
  (every #'zerop f))


(defun poly+-machinerie (f s)
  (let ((f-copy (copy-seq f)) (s-copy (copy-seq s))
        (len-f (length f)) (len-s (length s)) (res) (pos)
        (routine (lambda (fp sp sp-len)
                   (do ((i 0 (1+ i))) ((= sp-len i) fp)
                     (setf (svref fp i) (logxor (svref fp i) (svref sp i)))))))
    (setq res (if (> len-f len-s)
                  (funcall routine f-copy s-copy len-s)
                  (funcall routine s-copy f-copy len-f))
          pos (position-if-not #'zerop res :from-end t))
    (if (null pos) #(0)
        (subseq res 0 (1+ pos)))))


(defun poly+ (&rest polys)
  (reduce #'poly+-machinerie polys))


(defun xea-p (f s)
  ;(when (or (zero-poly? s) (< (length f) (length s)))
  ;  (return-from xea-p))
  (let ((p-0 f) (p-1 s) ;(g-0 #(1)) (g-1 #(0))
        (f-0 #(0)) (f-1 #(1)) (q))
    (while (not (zero-poly? p-1))
      (setq q (car (pdf p-0 p-1)))
      (psetq p-0 p-1 p-1 (poly+ p-0 (poly* p-1 q))
             ;g-0 g-1 g-1 (poly+ g-0 (poly* g-1 q p) p)
             f-0 f-1 f-1 (poly+ f-0 (poly* f-1 q)))) f-0))
    ;(list p-0 g-0 f-0)))


(defun equal-poly? (f s)
  (every #'= f s))


(defvar *mod-poly* (make-array 234 :initial-element 0))
(setf (aref *mod-poly*   0) 1
      (aref *mod-poly*  74) 1
      (aref *mod-poly* 233) 1)


(defun poly-mod-pow (base-poly power mod-poly)
  (setq base-poly (cadr (pdf base-poly mod-poly)))
  (do ((product #(1))) ((zerop power) product)
    (do () ((oddp power))
      (setq base-poly (cadr (pdf (poly* base-poly base-poly) mod-poly))
            power (ash power -1)))
    (setq product (cadr (pdf (poly* product base-poly) mod-poly))
          power (1- power))))


(defun add-points (P Q)
  (when (equal P "INF") (return-from add-points Q))
  (when (equal Q "INF") (return-from add-points P))
  (let* ((Px (car P)) (Py (cadr P))
         (Qx (car Q)) (Qy (cadr Q))
         (Rx) (Ry) (frac) (inv))
    (cond
      ((not (equal-poly? Px Qx)) (setq inv (xea-p *mod-poly* (poly+ Px Qx))
                                       frac (poly* (poly+ Py Qy) inv)
                                       Rx (poly+ (poly* frac frac) frac Px Qx)
                                       Ry (poly+ (poly* (poly+ frac #(1)) Rx)
                                                 (poly* (poly+ (poly* Py Qx) (poly* Qy Px)) inv))))
      (t (cond
           ((equal-poly? Py Qy) (setq inv (xea-p *mod-poly* Px)
                                      frac (poly* (poly+ (poly* Px Px) Py) inv)
                                      Rx (poly+ (poly* frac frac) frac)
                                      Ry (poly+ (poly* (poly+ frac #(1)) Rx) (poly* Px Px))))
           (t (return-from add-points "INF")))))
    (list (cadr (pdf Rx *mod-poly*)) (cadr (pdf Ry *mod-poly*)))))


(defun scalar-product (n P)
  (let ((result "INF") (addend P))
    (while (not (zerop n))
      (when (= 1 (logxor n 1))
        (setq result (add-points addend result)))
      (setq addend (add-points addend addend)
            n (ash n -1))) result))
