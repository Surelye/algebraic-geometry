(defpackage :aux
  (:use :common-lisp)
  (:export :while :ext-gcd
           :read-param :print-generated
           :write-to-file :mod-expt
           :miller-rabin :compute-legendre
           :cipolla))


(in-package :aux)


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


(defun is-square-residue (a p)
  (= 1 (mod-expt a (ash (1- p) -1) p)))


(defun compute-legendre-machinerie (a p)
  (cond ((zerop (mod a p)) 0)
        ((is-square-residue a p) 1)
        (t -1)))


(defun compute-legendre (a p)
  (when (and (integerp a) (miller-rabin p))
    (compute-legendre-machinerie a p)))


(defun complex-*-mod (f s sqr p)
  (let ((ffc (car f)) (fsc (cadr f))
        (sfc (car s)) (ssc (cadr s)))
    (mapcar #'(lambda (c) (mod c p))
            (list (+ (* ffc sfc) (* fsc ssc sqr))
                  (+ (* ffc ssc) (* fsc sfc))))))


(defun lst-mod-m (lst m)
  (mapcar #'(lambda (c) (mod c m)) lst))


(defun complex-^-mod (base power sqr m)
  (setq base (lst-mod-m base m))
  (do ((product '(1 0)))
      ((zerop power) (car product))
    (do () ((oddp power))
      (setq base (lst-mod-m (complex-*-mod base base sqr m) m)
            power (ash power -1)))
    (setq product (lst-mod-m (complex-*-mod product base sqr m) m)
          power (1- power))))


(defun cipolla (n p)
  (when (or (not (integerp n)) (not (integerp p)) (not (miller-rabin p))
            (< p 3) (/= 1 (compute-legendre n p))) (return-from cipolla))
  (let ((a) (sqr))
    (while (/= -1 (compute-legendre (- (* (setq a (random p)) a) n) p)))
    (setq sqr (- (* a a) n))
    (complex-^-mod (list a 1) (ash (1+ p) -1) sqr p)))
