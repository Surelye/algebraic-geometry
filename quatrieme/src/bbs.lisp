(defpackage #:bbs
  (:use :cl))

(in-package #:bbs)


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


(defun convert-to-decimal (reversed-binary-num)
  (apply #'+ (mapcar #'(lambda (bit power) (* bit (expt 2 power)))
                     reversed-binary-num
                     (loop for power from 0 to (1- (length reversed-binary-num))
                           collect power))))


(defun write-to-file (list-to-write &optional (filename "sequence"))
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (segment list-to-write)
      (format out "~D " segment))
    (format out "~%")))


(defun generate-sequence ()
  (let* ((req-length (read-required-length))
         (params (gen-ec::generate-curve req-length))
         (p-char (car params)) (b (cadr params)) (q (caddr params))
         (generator (cadddr params)) (seq-length nil) (seq nil)
         (bit-length nil) (coprime nil) (n (* 3 q))
         (current-num nil) (current-point nil)
         (filename nil))
    (format t "Была сгенерирована кривая с параметрами: характеристики поля p = ~d, коэффициента уравнения ЭК -- b = ~d, порядка
 циклической группы -- m = ~d, генератора циклической подгруппы G = (~{~a~^, ~}).~%"
            p-char b q generator)
    (tagbody
     read-inputs
       (format t "Введите длину генерируемой последовательности (целое число, большее 0): ")
       (setq seq-length (read))
       (format t "Введите битовую длину элемента последовательности: (целое число, большее 0): ")
       (setq bit-length (read))
       (when (not (and (integerp seq-length) (integerp bit-length)
                       (> seq-length 0) (> bit-length 0)))
         (format t "Некорректное значение параметров. Попробуйте ввести их снова.~%")
         (go read-inputs)))
    (dotimes (iter seq-length)
      (setq coprime (random n))
      (while (or (= 1 coprime) (/= 1 (gcd coprime n)))
        (setq coprime (random n)))
      (setq coprime (mod (* coprime coprime) n))
      (dotimes (jiter bit-length)
        (setq coprime (mod (* coprime coprime) n)
              current-point (ec-arith::scalar-product coprime generator p-char)
              current-num (cons (mod (car current-point) 2) current-num)))
      (setq seq (cons (convert-to-decimal current-num) seq)
            current-num nil))
    (format t "Введите название файла, в который будет записана сгенерированная последовательность (оставьте поле пустым для имени файла 'sequence'): ")
    (setq filename (read-line))
    (if (zerop (length filename))
        (write-to-file seq)
        (write-to-file seq filename))))
