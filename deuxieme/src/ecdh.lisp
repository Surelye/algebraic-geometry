(defpackage #:ecdh
  (:use :cl))

(in-package #:ecdh)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun read-required-length ()
  (format t "Введите требуемую длину n характеристики поля в битах (n > 7): ")
  (let ((length (read)))
    (while (or (not (integerp length)) (< length 8))
      (format t "Некорректный ввод. Попробуйте ввести значение длины n снова: ")
      (setq length (read)))
    length))


(defun read-num-users ()
  (let ((num-users (read)))
    (while (or (not (integerp num-users)) (< num-users 2) (> num-users 50))
      (format t "Некорректный ввод. Попробуйте ввести количество пользователей снова: ")
      (setq num-users (read)))
    num-users))


(defun p-to-hex (point)
  (mapcar #'(lambda (coord) (write-to-string coord :base 16)) point))


(defun generate-common-key (user-data p-char num-users)
  (let ((current-accum nil) (prev-accum nil)
        (current-secret nil) (cursed-index nil)
        (vile-index nil))
    (dotimes (iter num-users)
      (setq current-accum (caddr (nth iter user-data))
            prev-accum current-accum)
      (do ((jiter iter (1+ jiter)))
          ((= jiter (1- num-users)))
        (if (= iter jiter)
            (format t "Пользователь ~c передаёт свой публичный ключ (~{~a~^, ~}) пользователю ~c.~%"
                    (car (nth jiter user-data)) (p-to-hex prev-accum) (car (nth (1+ jiter) user-data)))
            (progn (setq prev-accum current-accum
                         current-secret (cadr (nth jiter user-data))
                         current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
                   (format t "Пользователь ~c вычисляет ~a * (~{~a~^, ~}) (mod p) = (~{~a~^, ~}) и передаёт пользователю ~c.~%"
                           (car (nth jiter user-data)) (write-to-string current-secret :base 16)
                           (p-to-hex prev-accum) (p-to-hex current-accum)
                           (car (nth (1+ jiter) user-data))))))
      (do ((jiter 0 (1+ jiter)))
          ((= jiter iter))
        (setq prev-accum current-accum
              cursed-index (mod (+ (1- num-users) jiter) num-users)
              current-secret (cadr (nth cursed-index user-data))
              current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
        (if (and (= cursed-index (1- num-users)) (= iter (1- num-users)))
            (progn (setq current-accum (caddr (nth cursed-index user-data))
                         prev-accum current-accum)
                   (format t "Пользователь ~c передаёт свой публичный ключ (~{~a~^, ~}) пользователю ~c.~%"
                           (car (nth cursed-index user-data)) (p-to-hex prev-accum)
                           (car (nth (mod (1+ cursed-index) num-users) user-data))))
            (format t "Пользователь ~c вычисляет ~a * (~{~a~^, ~}) (mod p) = (~{~a~^, ~}) и передаёт результат пользователю ~c.~%"
                    (car (nth cursed-index user-data)) (write-to-string current-secret :base 16) (p-to-hex prev-accum)
                    (p-to-hex current-accum) (car (nth (mod (+ num-users jiter) num-users) user-data)))))
      (setq prev-accum current-accum
            vile-index (mod (+ iter (1- num-users)) num-users)
            current-secret (cadr (nth vile-index user-data))
            current-accum (ec-arith::scalar-product current-secret prev-accum p-char))
      (format t "Пользователь ~c вычисляет ~a * (~{~a~^, ~}) (mod p) = (~{~a~^, ~}). Теперь у него есть значение секретного ключа.~%~%"
              (car (nth vile-index user-data)) (write-to-string current-secret :base 16)
              (p-to-hex prev-accum) (p-to-hex current-accum)))
    (car (p-to-hex current-accum))))


(defun ecdh ()
  (let* ((req-length (read-required-length))
         (params (gen-ec::generate-curve req-length))
         (p-char (car params)) (b (cadr params)) (E# (caddr params))
         (generator (cadddr params)) (num-users nil) (user-data nil)
         (current-random nil) (common-key nil))
    (format t "Для кривой были сгенерированы параметры: характеристики поля p = ~d, коэффициента уравнения ЭК -- b = ~d, порядка
 циклической группы -- m = ~d, генератора циклической подгруппы G = (~{~a~^, ~}).~%"
            p-char b E# generator)
    (format t "Введите количество пользователей (целое число в диапазоне [2; 50]): ")
    (setq num-users (read-num-users))
    (dotimes (iter num-users)
      (setq current-random (+ 2 (random (- E# 2)))
            user-data (cons (list (code-char (+ 65 iter)) current-random
                                  (ec-arith::scalar-product current-random generator p-char))
                            user-data)))
    (setq user-data (reverse user-data)
          common-key (generate-common-key user-data p-char num-users))
    common-key))
