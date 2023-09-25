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


(defun print-status (status args)
  (cond ((eql 'PASS status)
         (destructuring-bind (p-user pub-key n-user) args
           (format t "Пользователь ~c передаёт свой публичный ключ (~{0x~a~^, ~}) пользователю ~c.~%"
                   p-user (p-to-hex pub-key) n-user)))
        ((eql 'PASS-COMP status)
         (destructuring-bind (p-user scalar prev-p cur-p n-user) args
           (format t "Пользователь ~c вычисляет 0x~a * (~{0x~a~^, ~}) (mod p) = (~{0x~a~^, ~}) и передаёт пользователю ~c.~%"
                   p-user (write-to-string scalar :base 16) (p-to-hex prev-p)
                   (p-to-hex cur-p) n-user)))
        ((eql 'DONE status)
         (destructuring-bind (p-user scalar prev-p cur-p) args
           (format t "Пользователь ~c вычисляет 0x~a * (~{0x~a~^, ~}) (mod p) = (~{0x~a~^, ~}). Теперь у него есть значение секретного ключа.~%~%"
                   p-user (write-to-string scalar :base 16) (p-to-hex prev-p)
                   (p-to-hex cur-p))))
        (t (return-from print-status))))


(defun name (idx data)
  (car (nth idx data)))


(defun generate-common-key (user-data p-char num-users)
  (let ((current-accum) (prev-accum) (current-secret) (cursed-index) (vile-index))
    (dotimes (iter num-users)
      (setq current-accum (caddr (nth iter user-data))
            prev-accum current-accum)
      (do ((jiter iter (1+ jiter))) ((= jiter (1- num-users)))
        (if (= iter jiter)
            (print-status 'PASS (list (name jiter user-data) prev-accum
                                      (name (1+ jiter) user-data)))
            (progn (setq prev-accum current-accum
                         current-secret (cadr (nth jiter user-data))
                         current-accum (ec-arith::scalar-product current-secret
                                                                 prev-accum p-char))
                   (print-status 'PASS-COMP (list (name jiter user-data)
                                                  current-secret prev-accum
                                                  current-accum
                                                  (name (1+ jiter) user-data))))))
      (do ((jiter 0 (1+ jiter))) ((= jiter iter))
        (setq prev-accum current-accum
              cursed-index (mod (+ (1- num-users) jiter) num-users)
              current-secret (cadr (nth cursed-index user-data))
              current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
        (if (and (= cursed-index (1- num-users)) (= iter (1- num-users)))
            (progn (setq current-accum (caddr (nth cursed-index user-data))
                         prev-accum current-accum)
                   (print-status 'PASS (list (name cursed-index user-data) prev-accum
                                             (name (mod (1+ cursed-index) num-users) user-data))))
            (print-status 'PASS-COMP (list (name cursed-index user-data)
                                           current-secret prev-accum current-accum
                                           (name (mod (+ num-users jiter) num-users) user-data)))))
      (setq prev-accum current-accum
            vile-index (mod (+ iter (1- num-users)) num-users)
            current-secret (cadr (nth vile-index user-data))
            current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
      (print-status 'DONE (list (name vile-index user-data) current-secret
                                prev-accum current-accum)))
    (car (p-to-hex current-accum))))


(defun print-generated (args)
  (destructuring-bind (p-char b E# generator) args
    (format t "Для кривой были сгенерированы параметры:
~t~tхарактеристики поля p = 0x~x;
~t~tкоэффициента уравнения ЭК -- b = 0x~x;
~t~tпорядка циклической группы -- m = 0x~x;
~t~tгенератора циклической подгруппы G = (~{0x~x~^, ~}).~%"
            p-char b E# generator)))


(defun ecdh ()
  (let* ((req-length (read-param 'REQ-LENGTH)) (m-sec (read-param 'M-SEC))
         (params (gen-ec::generate-curve req-length m-sec)) (num-users)
         (user-data) (current-random) (common-key))
    (destructuring-bind (p-char b E# generator) params
      (print-generated (list p-char b E# generator))
      (setq num-users (read-param 'NUM-USERS)
            user-data (loop for user from 0 to (1- num-users)
                            collect (list (code-char (+ 65 user))
                                          (setq current-random (+ 2 (random (- E# 2))))
                                          (ec-arith::scalar-product current-random generator p-char)))
            common-key (generate-common-key user-data p-char num-users))
      common-key)))
