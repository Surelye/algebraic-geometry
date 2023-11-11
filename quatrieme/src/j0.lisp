(defpackage #:gen-ec
  (:use #:cl)
  (:export #:generate-curve))


(in-package #:gen-ec)


(defun from-binary-to-decimal (binary-form)
  (apply #'+ (mapcar #'(lambda (bit power) (* bit (expt 2 power)))
                     binary-form
                     (loop for i from (1- (length binary-form)) downto 0
                           collect i))))


(defun find-prime-mod-6 (lower-bound upper-bound starter)
  (let ((rem (mod starter 6))
        (starter-copy))
    (when (= 3 rem) (setq starter (- starter 2)))
    (when (= 5 rem) (setq starter (- starter 4)))
    (when (< starter lower-bound) (setq starter (+ 6 starter)))
    (setq starter-copy starter)
    (do ((iter starter (+ 6 iter))) ((> iter upper-bound))
      (when (aux:miller-rabin iter) (return-from find-prime-mod-6 iter)))
    (do ((iter (+ 1 lower-bound (- 6 (mod lower-bound 6))) (+ 6 iter)))
        ((> iter starter-copy))
      (when (aux:miller-rabin iter) (return-from find-prime-mod-6 iter)))))


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
    (when (and (= 1 (mod prime? 6)) (aux:miller-rabin prime?))
      (return-from generate-prime prime?))
    (find-prime-mod-6 lower-bound upper-bound prime?)))


(defun compute-u (D p)
  (let* ((D-normed (mod D p))
         (root? (aux:sqrt-Zp D-normed p)))
    (aux:while (/= D-normed (mod (* root? root?) p))
      (setq root? (aux:sqrt-zp D-normed p)))
    root?))


(defun get-ring-factorization (p-char &optional (D 3))
  (let ((is-square-residue (aux:is-power-residue (- D) p-char 2))
        u-i iter u-is m-i m-is a-i (b-i 1)
        a-is b-is a-i-f-num a-i-s-num
        b-i-num denom)
    (when (not is-square-residue)
      (return-from get-ring-factorization))
    (setq u-is (cons (compute-u (- D) p-char) u-is)
          m-is (cons p-char m-is))
    (do ((i 0 (1+ i))) ((= 1 (car m-is)) (setq a-i u-i
                                               iter (1- i)))
      (setq u-i (car u-is) m-i (car m-is))
      (setq m-is (cons (/ (+ (* u-i u-i) D) m-i) m-is) m-i (car m-is)
            u-is (cons (min (mod u-i m-i) (mod (- m-i u-i) m-i)) u-is)))
    (setq u-is (cddr u-is))
    (do ((j iter (1- j))) ((zerop j) (list a-i b-i))
      (setq u-i (car u-is) a-i-f-num (* u-i a-i)
            a-i-s-num (* D b-i) b-i-num (* u-i b-i)
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
      (setq m-is (cdr m-is) u-is (cdr u-is)))))


(defun get-char-and-factors (target-length)
  (let ((p-char) (factors))
    (aux:while (null (setq p-char (generate-prime target-length)
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
          (setq m (floor E# divisor))
          (when (aux:miller-rabin m)
            (setq E-m-divisor (cons (list E# m divisor) E-m-divisor))))))
    E-m-divisor))


(defun generate-P0-and-b (p-char)
  (let ((x0 0) (y0 0) (b nil))
    (aux:while (zerop x0)
      (setq x0 (random p-char)))
    (aux:while (zerop y0)
      (setq y0 (random p-char)))
    (setq b (mod (- (* y0 y0) (* x0 x0 x0)) p-char))
    (list (list x0 y0) b)))


(defun check-residues (b p-char divisor)
  (cond ((= 1 divisor) (and (not (aux:is-power-residue b p-char 2))
                            (not (aux:is-power-residue b p-char 3))))
        ((= 6 divisor) (and (aux:is-power-residue b p-char 2)
                            (aux:is-power-residue b p-char 3)))
        ((= 3 divisor) (and (aux:is-power-residue b p-char 2)
                            (not (aux:is-power-residue b p-char 3))))
        ((= 2 divisor) (and (not (aux:is-power-residue b p-char 2))
                            (aux:is-power-residue b p-char 3)))
        (t nil)))


(defun generate-curve (req-length m-sec)
  (let* ((p-d-e) (p-char) (Es) (E-m-divisor)
         (P0-and-b) (generator))
    (tagbody generate-p
       (aux:while (null (setq p-d-e (get-char-and-factors req-length)
                          p-char (car p-d-e)
                          Es (get-possible-#Es p-d-e)
                          E-m-divisor (check-equalities Es))))
       (setq E-m-divisor (nth (random (length E-m-divisor)) E-m-divisor))
       (when (= (cadr E-m-divisor) p-char)
         (go generate-p))
       (dotimes (iter m-sec)
         (when (= 1 (aux:mod-expt p-char (1+ iter) (cadr E-m-divisor)))
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
    (list p-char (cadr P0-and-b) (car E-m-divisor)
          (cadr E-m-divisor) generator)))
