(defpackage #:gen-ec
  (:use :cl))

(in-package #:gen-ec)


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


(defun from-binary-to-decimal (binary-form)
  (apply #'+ (mapcar #'(lambda (bit power) (* bit (expt 2 power)))
                     binary-form
                     (loop for i from (1- (length binary-form)) downto 0
                           collect i))))


(defun miller-rabin (num &optional (rounds 10))
  (when (or (= 2 num) (= 3 num)) (return-from miller-rabin t))
  (when (zerop (logand num 1)) (return-from miller-rabin))
  (let* ((n-pred (1- num)) (s 0) (t-val n-pred) (round-num 0) (a) (x))
    (while (zerop (mod t-val 2)) (setq s (1+ s) t-val (ash t-val -1)))
    (tagbody next-iteration
       (while (< round-num rounds)
         (setq a (+ 2 (random (- num 3)))
               x (mod-expt a t-val num))
         (when (or (= x 1) (= x n-pred))
           (setq round-num (1+ round-num)) (go next-iteration))
         (dotimes (iter (1- s))
           (setq x (mod (* x x) num))
           (when (= 1 x) (return-from miller-rabin))
           (when (= n-pred x)
             (setq round-num (1+ round-num)) (go next-iteration)))
         (return-from miller-rabin))
       (return-from miller-rabin t))))


(defun find-prime-mod-6 (lower-bound upper-bound starter)
  (let ((rem (mod starter 6))
        (starter-copy))
    (when (= 3 rem) (setq starter (- starter 2)))
    (when (= 5 rem) (setq starter (- starter 4)))
    (when (< starter lower-bound) (setq starter (+ 6 starter)))
    (setq starter-copy starter)
    (do ((iter starter (+ 6 iter))) ((> iter upper-bound))
      (when (miller-rabin iter) (return-from find-prime-mod-6 iter)))
    (do ((iter (+ 1 lower-bound (- 6 (mod lower-bound 6))) (+ 6 iter)))
        ((> iter starter-copy))
      (when (miller-rabin iter) (return-from find-prime-mod-6 iter)))))


(defun generate-prime (target-length)
  (let* ((bits-generated (- target-length 3))
         (lower-bound (from-binary-to-decimal
                       (cons 1 (append (loop for i from 0 to bits-generated
                                             collect 0) '(1)))))
         (upper-bound (from-binary-to-decimal
                       (cons 1 (append (loop for i from 0 to bits-generated
                                             collect 1) '(1)))))
         (prime? (from-binary-to-decimal
                  (cons 1 (append (loop for i from 0 to bits-generated
                                        collect (random 2)) '(1))))))
    (when (and (= 1 (mod prime? 6)) (miller-rabin prime?))
      (return-from generate-prime prime?))
    (find-prime-mod-6 lower-bound upper-bound prime?)))


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


(defun ceil (num)
  (multiple-value-bind (ceiled)
      (ceiling num) ceiled))


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


(defun compute-u (D p)
  (let* ((D-normed (mod D p))
         (root? (seq-sqrt-zp D-normed p)))
    (while (/= D-normed (mod (* root? root?) p))
      (setq root? (seq-sqrt-zp D-normed p)))
    root?))


(defun get-ring-factorization (p-char &optional (D 3))
  (let ((legendre (compute-legendre (- D) p-char))
        (u-i nil) (iter nil) (u-is nil)
        (m-i nil) (m-is nil) (a-i nil) (b-i 1)
        (a-is nil) (b-is nil) (a-i-f-num)
        (a-i-s-num) (b-i-num) (denom))
    (when (= -1 legendre)
      (return-from get-ring-factorization))
    (setq u-is (cons (compute-u (- D) p-char) u-is)
          m-is (cons p-char m-is))
    (do ((i 0 (1+ i)))
        ((= 1 (car m-is)) (setq a-i u-i
                                iter (1- i)))
      (setq u-i (car u-is)
            m-i (car m-is))
      (setq m-is (cons (/ (+ (* u-i u-i) D) m-i) m-is)
            m-i (car m-is)
            u-is (cons (min (mod u-i m-i) (mod (- m-i u-i) m-i)) u-is)))
    (setq u-is (cddr u-is))
    (do ((j iter (1- j)))
        ((zerop j) (list a-i b-i))
      (setq u-i (car u-is)
            a-i-f-num (* u-i a-i)
            a-i-s-num (* D b-i)
            b-i-num (* u-i b-i)
            denom (+ (* a-i a-i) (* D b-i b-i)))
      (psetq a-is (list (/ (+ a-i-f-num a-i-s-num) denom)
                        (/ (- a-i-f-num a-i-s-num) denom))
             b-is (list (/ (+ (- a-i) b-i-num) denom)
                        (/ (- (- a-i) b-i-num) denom)))
      (if (integerp (car a-is))
          (setq a-i (car a-is))
          (setq a-i (cadr a-is)))
      (if (integerp (car b-is))
          (setq b-i (car b-is))
          (setq b-i (cadr b-is)))
      (setq m-is (cdr m-is)
            u-is (cdr u-is)))))


(defun get-char-and-factors (target-length)
  (let ((p-char) (factors))
    (while (null (setq p-char (generate-prime target-length)
                       factors (get-ring-factorization p-char))))
    (cons p-char factors)))


(defun get-possible-#Es (p-c-d)
  (let* ((succ-p (1+ (car p-c-d)))
         (c (cadr p-c-d)) (d (caddr p-c-d))
         (possible-s (list (+ c (* 3 d)) (- c (* 3 d)) (* 2 c))))
    (append (mapcar #'(lambda (s) (+ succ-p s)) possible-s)
            (mapcar #'(lambda (s) (- succ-p s)) possible-s))))


(defun routine (divider)
  (lambda (dividend) (zerop (mod dividend divider))))


(defun check-equalities (possible-#Es)
  (let ((divisors '(1 6 3 2)) (E-m-divisor) (m))
    (dolist (divisor divisors)
      (dolist (E# possible-#Es)
        (when (funcall (routine divisor) E#)
          (setq m (div E# divisor))
          (when (miller-rabin m)
            (setq E-m-divisor (cons (list E# m divisor) E-m-divisor))))))
    E-m-divisor))


(defun generate-P0-and-b (p-char)
  (let ((x0 0) (y0 0) (b nil))
    (while (zerop x0)
      (setq x0 (random p-char)))
    (while (zerop y0)
      (setq y0 (random p-char)))
    (setq b (mod (- (* y0 y0) (* x0 x0 x0)) p-char))
    (list (list x0 y0) b)))


(defun check-residues (b p-char divisor)
  (cond ((= 1 divisor) (and (not (is-power-residue b p-char 2))
                            (not (is-power-residue b p-char 3))))
        ((= 6 divisor) (and (is-power-residue b p-char 2)
                            (is-power-residue b p-char 3)))
        ((= 3 divisor) (and (is-power-residue b p-char 2)
                            (not (is-power-residue b p-char 3))))
        ((= 2 divisor) (and (not (is-power-residue b p-char 2))
                            (is-power-residue b p-char 3)))
        (t nil)))


(defun generate-curve (req-length m-sec)
  (let* ((p-d-e) (p-char) (Es) (E-m-divisor)
         (P0-and-b) (generator))
    (tagbody generate-p
       (while (null (setq p-d-e (get-char-and-factors req-length)
                          p-char (car p-d-e)
                          Es (get-possible-#Es p-d-e)
                          E-m-divisor (check-equalities Es))))
       (setq E-m-divisor (nth (random (length E-m-divisor)) E-m-divisor))
       (when (= (cadr E-m-divisor) p-char)
         (go generate-p))
       (dotimes (iter m-sec)
         (when (= 1 (mod-expt p-char (1+ iter) (cadr E-m-divisor)))
           (go generate-p))))
       (tagbody generate-point-and-b
          (setq P0-and-b (generate-P0-and-b p-char))
          (when (not (check-residues (cadr P0-and-b) p-char (caddr E-m-divisor)))
            (go generate-point-and-b))
          (if (equal (ec-arith::scalar-product (car E-m-divisor)
                                               (car P0-and-b)
                                               p-char)
                     EC-ARITH::'INF)
              (setq generator (ec-arith::scalar-product (caddr E-m-divisor)
                                                        (car P0-and-b)
                                                        p-char))
              (go generate-point-and-b)))
    (list p-char (cadr P0-and-b) (cadr E-m-divisor) generator)))
