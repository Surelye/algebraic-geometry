(defpackage #:gen-ec
  (:use :cl))

(in-package #:gen-ec)


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


(defun find-prime-mod-6 (lower-bound upper-bound starter)
  (let ((rem (mod starter 6))
        (starter-copy))
    (cond ((= 3 rem) (setq starter (- starter 2)))
          ((= 5 rem) (setq starter (- starter 4)))
          (t))
    (when (< starter lower-bound) (setq starter (+ 6 starter)))
    (setq starter-copy starter)
    (do ((iter starter (+ 6 iter)))
        ((> iter upper-bound))
      (when (miller-rabin iter) (return-from find-prime-mod-6 iter)))
    (do ((iter (+ 1 lower-bound (- 6 (mod lower-bound 6))) (+ 6 iter)))
        ((> iter starter-copy))
      (when (miller-rabin iter) (return-from find-prime-mod-6 iter)))
    nil))


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
    (when (miller-rabin prime?) (return-from generate-prime prime?))
    (find-prime-mod-6 lower-bound upper-bound prime?)))


(defun div (dividend divider)
  (multiple-value-bind (quotient)
      (floor dividend divider) quotient))


(defun ceil (num)
  (multiple-value-bind (ceiled)
      (ceiling num) ceiled))


(defun is-power-residue (b p-char power)
  (when (or (= 2 power) (= 3 power))
    (= 1 (mod-expt b (div (1- p-char) power) p-char))))


(defun compute-legendre (a p)
  (when (zerop (mod a p))
    (return-from compute-legendre 0))
  (if (null (is-power-residue a p 2)) -1 1))


(defun square-root-q-5-mod-8 (a p)
  (let ((b (mod-expt a (div (+ 3 p) 8) p))
        (c (mod-expt a (div (1- p) 4) p))
        (i nil) (xs nil))
    (when (and (/= 1 c) (/= (1- p) c))
      (return-from square-root-q-5-mod-8 -1))
    (when (= 1 c)
      (return-from square-root-q-5-mod-8 (min b (mod (- b) p))))
    (setq i (mod-expt 2 (div (1- p) 4) p)
          xs (list (mod (* b i) p) (mod (* (mod (- b) p) i) p)))
    (apply #'min xs)))


(defun square-root-q-3-mod-4 (a p)
  (let ((x (mod-expt a (div (1+ p) 4) p)))
    (if (= (mod a p) (mod (* x x) p))
        x
        -1)))


(defun compute-u (D p)
  (let ((u nil))
    (when (= 3 (mod p 4))
      (setq u (square-root-q-3-mod-4 D p)))
    (when (= 5 (mod p 8))
      (setq u (square-root-q-5-mod-8 D p)))
    u))


(defun get-ring-factorization (p-char &optional (D 3))
  (let ((legendre (compute-legendre (- D) p-char))
        (u-i nil) (iter nil) (u-is nil)
        (m-i nil) (m-is nil) (a-i nil) (b-i 1)
        (a-is nil) (b-is nil))
    (when (= -1 legendre)
      (return-from get-ring-factorization nil))
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
      (setq u-i (car u-is))
      (psetq a-is (list (/ (+ (*    u-i  a-i) (* D b-i)) (+ (* a-i a-i) (* D b-i b-i)))
                        (/ (+ (* (- u-i) a-i) (* D b-i)) (+ (* a-i a-i) (* D b-i b-i))))
             b-is (list (/ (+ (- a-i) (* u-i b-i)) (+ (* a-i a-i) (* D b-i b-i)))
                        (/ (- (- a-i) (* u-i b-i)) (+ (* a-i a-i) (* D b-i b-i)))))
      (if (integerp (car a-is))
          (setq a-i (car a-is))
          (setq a-i (cadr a-is)))
      (if (integerp (car b-is))
          (setq b-i (car b-is))
          (setq b-i (cadr b-is)))
      (setq m-is (cdr m-is)
            u-is (cdr u-is)))))


(defun get-char-and-factors (target-length)
  (let ((p-char nil) (factors nil))
    (while t
      (setq p-char (generate-prime target-length))
      (when (and (= 1 (mod p-char 6))
                 (or (= 5 (mod p-char 8)) (= 3 (mod p-char 4))))
        (format t "Пробуем разложить число 0x~X...~%" p-char)
        (setq factors (get-ring-factorization p-char))
        (cond ((null factors) (format t "Разложить число не получилось. Сгенерируем новое.~%"))
              (t (format t "Была сгенерирована характеристика поля p = 0x~X с длиной ~d битов.~%"
                         p-char (get-bit-length p-char))
                 (format t "Элементы разложения: d = 0x~X и e = 0x~X.~%"
                         (car factors) (cadr factors))
                 (return-from get-char-and-factors (cons p-char factors))))))))


(defun get-possible-#Es (p-c-d)
  (let* ((succ-p (1+ (car p-c-d)))
         (c (cadr p-c-d)) (d (caddr p-c-d))
         (possible-s (list (+ c (* 3 d)) (- c (* 3 d)) (* 2 c))))
    (append (mapcar #'(lambda (s) (+ succ-p s)) possible-s)
            (mapcar #'(lambda (s) (- succ-p s)) possible-s))))


(defun routine (divider)
  (lambda (dividend) (zerop (mod dividend divider))))


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


(defun read-m-sec-param ()
  (format t "Введите параметр безопасности m (m -- натуральное число): ")
  (let ((m-sec (read)))
    (while (or (not (integerp m-sec)) (> 1 m-sec))
      (format t "Некорректный ввод. Попробуйте ввести значение параметра безопасности m снова: ")
      (setq m-sec (read)))
    m-sec))


(defun write-to-file (list-to-write &optional (filename "subgroup"))
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (point list-to-write)
      (format out "~A~%" point))))


(defun generate-curve ()
  (let* ((req-length (read-required-length))
         (m-sec (read-m-sec-param))
         (p-d-e (get-char-and-factors req-length))
         (p-char (car p-d-e))
         (Es (get-possible-#Es p-d-e))
         (E-m-divisor (check-equalities Es))
         (P0-and-b) (generator) (subgroup))
    (tagbody
     generate-p
       (while (null E-m-divisor)
         (format t "Равенство и условия следствия не выполнены. Повторно сгенерируем характеристику поля.~%")
         (setq p-d-e (get-char-and-factors req-length)
               p-char (car p-d-e)
               Es (get-possible-#Es p-d-e)
               E-m-divisor (check-equalities Es)))
       (setq E-m-divisor (car E-m-divisor))
       (when (= (cadr E-m-divisor) p-char)
         (go generate-p))
       (dotimes (iter m-sec)
         (when (= 1 (mod-expt p-char (1+ iter) (cadr E-m-divisor)))
           (go generate-p))))
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
    (format t "Получены значения:
~t~tхарактеристики поля p = 0x~X;
~t~tкоэффициента уравнения ЭК -- b = 0x~X;
~t~tпорядка циклической группы -- m = 0x~X;
~t~tгенератора циклической подгруппы G = (~{0x~X~^, ~})."
            p-char (cadr P0-and-b) (cadr E-m-divisor) generator)
    (when (y-or-n-p "Хотите сгенерировать циклическую подгруппу?")
      (setq subgroup (ec-arith::generate-subgroup generator p-char))
      (write-to-file subgroup)
      (format t "Получившаяся подгруппа была записана в файл subgroup.~%")
      (setq subgroup (cdr subgroup))
      (ls-user::draw-plot (mapcar #'car subgroup) (mapcar #'cadr subgroup)))
    (list p-char (cadr P0-and-b) (cadr E-m-divisor) generator)))
