(defpackage #:aux
  (:use #:cl)
  (:export #:while
           #:ext-gcd
           #:read-param
           #:print-generated
           #:write-to-file
           #:mod-expt
           #:miller-rabin
           #:jacobi
           #:is-power-residue
           #:sqrt-Zp))


(in-package #:aux)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


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


(defun make-strings (option)
  (let ((hello-string) (error-string "Некорректный ввод. "))
    (cond ((eql 'REQ-LENGTH option) (setq hello-string "~%Введите требуемую длину n характеристики поля в битах (n > 7): "
                                          error-string (concatenate 'string error-string "Попробуйте ввести значение длины n снова: ")))
          ((eql 'M-SEC option) (setq hello-string "Введите параметр безопасности m (m -- натуральное число): "
                                     error-string (concatenate 'string error-string "Попробуйте ввести значение параметра безопасности m снова: ")))
          ((eql 'NUM-USERS option) (setq hello-string "~%Введите количество пользователей (целое число в диапазоне [2; 50]): "
                                         error-string (concatenate 'string error-string "Попробуйте ввести количество пользователей снова: ")))
          (t (return-from make-strings)))
    (list hello-string error-string)))


(defun read-param (option)
  (let ((param) (hello-string) (error-string)
        (condition (cond ((eql 'REQ-LENGTH option) (lambda (param) (and (integerp param) (> param 7))))
                         ((eql 'M-SEC option) (lambda (param) (and (integerp param) (> param 0))))
                         ((eql 'NUM-USERS option) (lambda (param) (and (integerp param) (> param 1) (< param 51))))
                         (t (return-from read-param)))))
    (destructuring-bind (hello error) (make-strings option)
      (setq hello-string hello error-string error))
    (format t hello-string)
    (while (not (funcall condition (setq param (read))))
      (format t error-string)) param))


(defun print-generated (args)
  (destructuring-bind (p b n r gen) args
    (format t "~%Для кривой были сгенерированы параметры:
~%~4tХарактеристика поля p           =  0x~x;
~4tКоэффициент уравнения ЭК b      =  0x~x;
~4tПорядок кривой #E               =  0x~x;
~4tПорядок циклической подгруппы m =  0x~x;
~4tГенератор G подгруппы           = (~{0x~x~^, ~}).~%"
            p b n r gen)))


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (datum data)
      (if (atom datum)
          (format out "~a~%" datum)
          (format out "~a~%~a~%" (car datum) (cadr datum))))))


(defun mod-expt (base power modulo)
  (setq base (mod base modulo))
  (do ((product 1)) ((zerop power) product)
    (do () ((oddp power))
      (setq base (mod (* base base) modulo)
            power (ash power -1)))
    (setq product (mod (* product base) modulo)
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


(defun jacobi-machinerie (a b)
  (let ((r 1) (t-val) (c))
    (when (< a 0) (setq a (- a))
      (when (= 3 (mod b 4)) (setq r (- r))))
    (tagbody eliminate-evenness
       (setq t-val 0)
       (while (zerop (logand a 1))
         (setq t-val (1+ t-val) a (ash a -1)))
       (when (= 1 (logand t-val 1))
         (when (or (= 3 (mod b 8)) (= 5 (mod b 8)))
           (setq r (- r))))
       (when (and (= 3 (mod a 4)) (= 3 (mod b 4)))
         (setq r (- r)))
       (setq c a a (mod b c) b c)
       (if (not (zerop a))
           (go eliminate-evenness)
           (return-from jacobi-machinerie r)))))


(defun jacobi (a b)
  (when (and (integerp a) (integerp b) (= 1 (logand b 1)) (> b 1))
    (if (= 1 (gcd a b))
        (jacobi-machinerie a b) 0)))


(defun is-power-residue (a p power)
  (when (or (= 2 power) (= 3 power))
    (= 1 (mod-expt a (/ (1- p) power) p))))


(defun find-k-i (a-i q p)
  (do ((k 0 (1+ k))) ((= 1 (mod-expt a-i (* (expt 2 k) q) p)) k)))


(defun sqrt-Zp (a p)
  (let ((b) (k-i -1) (k-is) (r-i) (m 0) (q (1- p))
        (a-prev a) (a-cur a) (pow))
    (while (zerop (logand q 1)) (setq m (1+ m) q (ash q -1)))
    (while (/= -1 (jacobi (setq b (random p)) p)))
    (while (not (zerop k-i))
      (setq  k-i    (find-k-i a-cur q p) k-is (cons k-i k-is))
      (psetq a-cur  (mod (* a-cur (mod-expt b (ash 1 (- m k-i)) p)) p)
             a-prev a-cur))
    (setq k-is (cdr k-is) r-i (mod-expt a-prev (ash (1+ q) -1) p))
    (do ((i (length k-is) (1- i))) ((= 0 i) r-i)
      (setq pow (ash 1 (- m (car k-is) 1))
            r-i (mod (* r-i (cadr (ext-gcd (mod-expt b pow p) p))) p)
            k-is (cdr k-is)))))
