(defpackage #:ecdh
  (:use :cl))

(in-package #:ecdh)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun make-strings (option)
  (let ((hello-string) (error-string "Некорректный ввод. "))
    (cond ((eql 'REQ-LENGTH option) (setq hello-string "Введите требуемую длину n характеристики поля в битах (n > 7): "
                                          error-string (concatenate 'string error-string "Попробуйте ввести значение длины n снова: ")))
          ((eql 'M-SEC option) (setq hello-string "Введите параметр безопасности m (m -- натуральное число): "
                                     error-string (concatenate 'string error-string "Попробуйте ввести значение параметра безопасности m снова: ")))
          ((eql 'NUM-USERS option) (setq hello-string "Введите количество пользователей (целое число в диапазоне [2; 50]): "
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
      (format t error-string))
    param))


(defun p-to-hex (point)
  (mapcar #'(lambda (coord) (write-to-string coord :base 16)) point))


(defun generate-common-key (user-data p-char num-users)
  (let ((current-accum) (prev-accum) (current-secret)
        (cursed-index) (vile-index))
    (dotimes (iter num-users)
      (setq current-accum (caddr (nth iter user-data))
            prev-accum current-accum)
      (do ((jiter iter (1+ jiter))) ((= jiter (1- num-users)))
        (if (= iter jiter)
            (format t "Пользователь ~c передаёт свой публичный ключ (~{0x~a~^, ~}) пользователю ~c.~%"
                    (car (nth jiter user-data)) (p-to-hex prev-accum) (car (nth (1+ jiter) user-data)))
            (progn (setq prev-accum current-accum
                         current-secret (cadr (nth jiter user-data))
                         current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
                   (format t "Пользователь ~c вычисляет 0x~a * (~{0x~a~^, ~}) (mod p) = (~{0x~a~^, ~}) и передаёт пользователю ~c.~%"
                           (car (nth jiter user-data)) (write-to-string current-secret :base 16)
                           (p-to-hex prev-accum) (p-to-hex current-accum)
                           (car (nth (1+ jiter) user-data))))))
      (do ((jiter 0 (1+ jiter))) ((= jiter iter))
        (setq prev-accum current-accum
              cursed-index (mod (+ (1- num-users) jiter) num-users)
              current-secret (cadr (nth cursed-index user-data))
              current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
        (if (and (= cursed-index (1- num-users)) (= iter (1- num-users)))
            (progn (setq current-accum (caddr (nth cursed-index user-data))
                         prev-accum current-accum)
                   (format t "Пользователь ~c передаёт свой публичный ключ (~{0x~a~^, ~}) пользователю ~c.~%"
                           (car (nth cursed-index user-data)) (p-to-hex prev-accum)
                           (car (nth (mod (1+ cursed-index) num-users) user-data))))
            (format t "Пользователь ~c вычисляет 0x~a * (~{0x~a~^, ~}) (mod p) = (~{0x~a~^, ~}) и передаёт результат пользователю ~c.~%"
                    (car (nth cursed-index user-data)) (write-to-string current-secret :base 16) (p-to-hex prev-accum)
                    (p-to-hex current-accum) (car (nth (mod (+ num-users jiter) num-users) user-data)))))
      (setq prev-accum current-accum
            vile-index (mod (+ iter (1- num-users)) num-users)
            current-secret (cadr (nth vile-index user-data))
            current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
      (format t "Пользователь ~c вычисляет 0x~a * (~{0x~a~^, ~}) (mod p) = (~{0x~a~^, ~}). Теперь у него есть значение секретного ключа.~%~%"
              (car (nth vile-index user-data)) (write-to-string current-secret :base 16)
              (p-to-hex prev-accum) (p-to-hex current-accum)))
    (car (p-to-hex current-accum))))


(defun ecdh ()
  (let* ((req-length (read-param 'REQ-LENGTH))
         (m-sec (read-param 'M-SEC))
         (params (gen-ec::generate-curve req-length m-sec))
         (p-char (car params)) (b (cadr params)) (E# (caddr params))
         (generator (cadddr params)) (num-users) (user-data)
         (current-random) (common-key))
    (format t "Для кривой были сгенерированы параметры:
~t~tхарактеристики поля p = 0x~X;
~t~tкоэффициента уравнения ЭК -- b = 0x~X;
~t~tпорядка циклической группы -- m = 0x~X;
~t~tгенератора циклической подгруппы G = (~{0x~X~^, ~}).~%"
            p-char b E# generator)
    (setq num-users (read-param 'NUM-USERS))
    (dotimes (iter num-users)
      (setq current-random (+ 2 (random (- E# 2)))
            user-data (cons (list (code-char (+ 65 iter)) current-random
                                  (ec-arith::scalar-product current-random generator p-char))
                            user-data)))
    (setq user-data (reverse user-data)
          common-key (generate-common-key user-data p-char num-users))
    common-key))
