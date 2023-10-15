(defpackage :messi-omurah
  (:use :common-lisp))

(in-package :messi-omurah)


(defun mo-gen-ec ()
  (let* ((req-len (aux:read-param aux::'REQ-LENGTH))
         (m-sec (aux:read-param aux::'M-SEC))
         (params (gen-ec::generate-curve req-len m-sec)))
    (destructuring-bind (p b n r gen) params
      (aux:print-generated (list p b n r gen))
      (aux:write-to-file params "curve-params")
      (format t "~%Параметры кривой были записаны в файл curve-params."))))


(defun get-params ()
  (let ((filename) (content))
    (format t "~%Введите имя файла, в котором содержатся параметры кривой (по умолчанию curve-params): ")
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename)) (setq filename "curve-params"))
       (when (not (uiop:file-exists-p filename))
         (format t "~%Файла с указанным именем не существует! Попробуйте ввести имя файла снова: ")
         (go try-again)))
    (setq content (uiop:read-file-lines filename))
    (when (/= 6 (length content)) (return-from get-params))
    (setq content (mapcar #'parse-integer content))))


(defun Q-belongs-curve (Q b p &optional (a 0))
  (let ((x (car Q)) (y (cadr Q)))
    (= (mod (* y y) p) (mod (+ (* x x x) (* a x) b) p))))


(defun ok-params? (params)
  (let ((error-string "~%Целостность параметров ЭК была нарушена. Шифрование невозможно.")
        (test-1) (test-2) (test-3) (test-4))
    (when (null params)
      (format t error-string) (return-from ok-params?))
    (destructuring-bind (p b n r x y) params
      (setq test-1 (aux:miller-rabin p 25)
            test-2 (zerop (mod n r))
            test-3 (Q-belongs-curve (list x y) b p)
            test-4 (eql (ec-arith::scalar-product r (list x y) p)
                        EC-ARITH::'INF))
      (format t "~%Проверка целостности параметров:~%
    Простота числа p:             ~a;
    Порядок подгруппы r делит #E: ~a;
    Принадлежность G кривой:      ~a;
    r * G = INF:                  ~a.~%" test-1 test-2 test-3 test-4)
      (when (not (and test-1 test-2 test-3 test-4))
        (format t error-string) (return-from ok-params?))) t))


(defun get-num-bits-to-cipher (max-bits)
  (let ((bits))
    (format t "~%Введите количество старших битов n, используемых для шифрования (1 < n < ~a): "
            (1+ max-bits))
    (tagbody try-again
       (setq bits (read))
       (when (not (and (integerp bits) (<= 2 bits max-bits)))
         (format t "~%Некорректный ввод! Попробуйте ввести количество старших битов, используемых для шифрования снова: ")
         (go try-again))) bits))


(defun get-message (p)
  (let ((message) (bit-len) (bits) (bound))
    (setq bit-len (1- (length (write-to-string p :base 2)))
          bits (get-num-bits-to-cipher bit-len)
          bound (1- (ash 1 bits)))
    (aux:write-to-file (list bits) "bits-to-cipher")
    (format t "~%Введите сообщение (число) m, которое хотите зашифровать (1 < m < ~a): " (1+ bound))
    (tagbody try-again
       (setq message (read))
       (when (not (and (integerp message) (<= 2 message bound)))
         (format t "~%Введённое значение не может быть зашифровано. Попробуйте ввести m снова: ")
         (go try-again)))
    (aux:write-to-file (list message) "message") message))


(defun read-parse (filename)
  (parse-integer (uiop:read-file-line filename)))


(defun ec-equation (p b x &optional (a 0))
  (mod (+ (* x x x) (* a x) b) p))


(defun find-R-point (params bits message)
  (let ((bin-message (write-to-string message :base 2)) (x) (y) (R-point)
        (p (car params)) (b (cadr params)) (r (nth 3 params)))
    (tagbody try-again
       (setq x (concatenate 'string bin-message
                            (reduce #'(lambda (f s) (concatenate 'string f s))
                                    (loop for bit from 1 to (- bits (length bin-message))
                                          collect (write-to-string (random 2)))))
             x (parse-integer x :radix 2)
             y (aux:cipolla (ec-equation p b x) p))
       (when (null y) (go try-again))
       (setq R-point (list x y))
       (when (not (eql (ec-arith::scalar-product r R-point p) EC-ARITH::'INF))
         (go try-again)))
    (aux:write-to-file R-point "r-point") R-point))


(defun mo-encrypt-machinerie (params)
  (let* ((bits (read-parse "bits-to-cipher")) (message (read-parse "message"))
         (R-point (find-R-point params bits message))
         (p (car params)) (n (caddr params)) (a) (b)
         (a-inv) (b-inv) (aR) (baR) (abaR) (bbR))
    (aux:while (/= 1 (gcd (setq a (random n)) n)))
    (setq aR (ec-arith::scalar-product a R-point p))
    (aux:while (/= 1 (gcd (setq b (random n)) n)))
    (setq baR (ec-arith::scalar-product b aR p)
          a-inv (mod (cadr (aux:ext-gcd a n)) n)
          b-inv (mod (cadr (aux:ext-gcd b n)) n)
          abaR (ec-arith::scalar-product a-inv baR p)
          bbR (ec-arith::scalar-product b-inv abaR p))
    (aux:write-to-file aR "encrypted-A")
    (aux:write-to-file baR "encrypted-B")
    (aux:write-to-file abaR "decrypted-A")
    (aux:write-to-file bbR "decrypted-B")))




(defun mo-encrypt ()
  (let ((params (get-params)) (p) (message))
    (when (not (ok-params? params)) (return-from mo-encrypt))
    (setq p (car params)
          message (get-message p))
    (mo-encrypt-machinerie params)
