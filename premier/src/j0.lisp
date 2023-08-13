(defpackage #:gen-el-curve
  (:use :cl))

(in-package #:gen-el-curve)


(defvar list-of-primes (list 131 137 139 149 151 157 163 167
                             173 179 181 191 193 197 199 211
                             223 227 229 233 239 241 251))


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun get-bit-length (number)
  (let ((bit-length 0))
    (if (zerop number) (setq bit-length 1)
        (while (not (zerop number))
          (setq number (ash number -1)
                bit-length (1+ bit-length))))
    bit-length))


(defun generate-s-even (s-even-length)
  (let* ((s-even (list 1))
         (extra-bits-num 2)
         (total-length (+ s-even-length extra-bits-num)))
    (dotimes (iter s-even-length)
      (setq s-even (cons (random 2) s-even)))
    (setq s-even (cons 0 s-even))
    (apply #'+ (mapcar #'(lambda (bit power) (if (= bit 1) (expt 2 power) 0))
                       s-even
                       (loop for pow from 0 to (1- total-length) collect pow)))))


(defun mod-expt (base power divisor)
  (setq base (mod base divisor))
  (do ((product 1))
      ((zerop power) product)
    (do () ((oddp power))
      (setq base (mod (* base base) divisor)
            power (ash power -1)))
    (setq product (mod (* product base) divisor)
          power (1- power))))


(defun generate-prime (target-length)
  (let* ((primes-length (length list-of-primes))
         (q-prime (nth (random primes-length) list-of-primes))
         (q-prime-length (get-bit-length q-prime))
         (s-even-length nil) (s-even nil) (q-s-product nil) (p-prime nil))
    (while (< q-prime-length target-length)
      (setq s-even-length (- target-length q-prime-length)
            s-even (generate-s-even s-even-length)
            q-s-product (* q-prime s-even)
            p-prime (1+ q-s-product))
      (when (and (= 1 (mod-expt 2 q-s-product p-prime)) (/= 1 (mod-expt 2 s-even p-prime)))
        (setq q-prime p-prime
              q-prime-length (get-bit-length q-prime))))
    q-prime))


(defun get-char-and-factors (target-length)
  (let ((e-mult 1.2) (p-char nil) (factors nil))
    (while t
      (setq p-char (generate-prime target-length))
      (when (and (= 1 (mod p-char 6)) (= 3 (mod p-char 4)))
        (format t "Пробуем разложить число ~d...~%" p-char)
        (setq factors (get-ring-factorization p-char e-mult))
        (cond ((null factors) (format t "Быстро разложить число не получилось. Сгенерируем новое.~%")
               (setq e-mult (+ 0.2 e-mult)))
              (t (format t "Была сгенерирована характеристика поля p = ~d с длиной ~d битов.~%"
                         p-char (get-bit-length p-char))
                 (format t "Элементы разложения: d = ~d и e = ~d.~%"
                         (car factors) (cadr factors))
                 (return-from get-char-and-factors (cons p-char factors))))))))


(defun get-ring-factorization (p-char e-mult)
  (let* ((lower-bound (isqrt (div p-char 2)))
         (lb-rem (mod lower-bound 3)) (upper-bound (1+ (isqrt p-char)))
         (ub-multed nil))
    (do ((d (+ 2 lower-bound (- lb-rem)) (+ 3 d)))
        ((> d upper-bound) nil)
      (setq ub-multed (ceil (* e-mult upper-bound)))
      (do ((e (+ 3 ub-multed (- (mod ub-multed 3))) (- e 3)))
          ((< e lower-bound) nil)
        (when (= p-char (+ (* d d) (* e e) (- (* d e))))
          (return-from get-ring-factorization (list d e)))))
    nil))


(defun div (dividend divider)
  (multiple-value-bind (quotient remainder)
      (floor dividend divider) quotient))


(defun ceil (num)
  (multiple-value-bind (ceiled diff)
      (ceiling num) ceiled))


(defun get-possible-#Es (p-d-e)
  (let* ((succ-p (1+ (car p-d-e)))
         (d (cadr p-d-e)) (e (caddr p-d-e))
         (possible-s (list (+ d e) (- (* 2 d) e) (- d (* 2 e)))))
    (append (mapcar #'(lambda (s) (+ succ-p s)) possible-s)
            (mapcar #'(lambda (s) (- succ-p s)) possible-s))))


(defun routine (divider)
  (lambda (dividend) (zerop (mod dividend divider))))


(defun miller-rabin (num &optional (rounds 10))
  (when (or (= 2 num) (= 3 num)) (return-from miller-rabin t))
  (when (zerop (mod num 2)) (return-from miller-rabin nil))
  (let* ((n-pred (1- num)) (s 0) (t-val n-pred) (round-num 0) (a nil) (x nil))
    (while (zerop (mod t-val 2)) (setq s (1+ s) t-val (ash t-val -1)))
    (tagbody
     next-iteration
       (while (< round-num rounds)
         (setq a (+ 2 (random (- num 3)))
               x (mod-expt a t-val num))
         (when (or (= x 1) (= x n-pred))
           (setq round-num (1+ round-num)) (go next-iteration))
         (dotimes (iter (1- s))
           (setq x (mod (* x x) num))
           (when (= 1 x) (return-from miller-rabin nil))
           (when (= n-pred x)
             (setq round-num (1+ round-num)) (go next-iteration)))
         (return-from miller-rabin nil))
       (return-from miller-rabin t))))


(defun check-equalities (possible-#Es)
  (let ((divisors '(1 6 3 2)) (E-m-divisor nil)
        (m nil))
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


(defun is-power-residue (b p-char power)
  (when (or (= 2 power) (= 3 power))
    (= 1 (mod-expt b (div (1- p-char) power) p-char))))


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


(defun read-required-length ()
  (format t "Введите требуемую длину n характеристики поля в битах (n > 7): ")
  (let ((length (read)))
    (while (or (not (integerp length)) (< length 8))
      (format t "Некорректный ввод. Попробуйте ввести значение длины n снова: ")
      (setq length (read)))
    length))


(defun generate-curve ()
  (let* ((req-length (read-required-length))
         (p-d-e (get-char-and-factors req-length))
         (p-char (car p-d-e))
         (Es (get-possible-#Es p-d-e))
         (E-m-divisor (check-equalities Es))
         (P0-and-b nil) (generator nil))
    (while (null E-m-divisor)
      (format t "Равенство и условия следствия не выполнены. Повторно сгенерируем характеристику поля.~%")
      (setq p-d-e (get-char-and-factors req-length)
            p-char (car p-d-e)
            Es (get-possible-#Es p-d-e)
            E-m-divisor (check-equalities Es)))
    (setq E-m-divisor (car E-m-divisor))
    (tagbody
     generate-point-and-b
       (setq P0-and-b (generate-P0-and-b p-char))
       (when (not (check-residues (cadr P0-and-b) p-char (caddr E-m-divisor)))
         (go generate-point-and-b))
      (if (eql (ec-arith::scalar-product (car E-m-divisor)
                                          (car P0-and-b)
                                          p-char)
                EC-ARITH::'INF)
           (setq generator (ec-arith::scalar-product (caddr E-m-divisor)
                                                     (car P0-and-b)
                                                     p-char))
           (go generate-point-and-b)))
    (format t "Получены значения характеристики поля p = ~d, коэффициента уравнения ЭК -- b = ~d, порядка циклической группы -- m = ~d,
 генератора циклической подгруппы G = (~{~a~^, ~})."
            p-char (cadr P0-and-b) (cadr E-m-divisor) generator)
    (list p-char (cadr P0-and-b) (cadr E-m-divisor) generator)))
