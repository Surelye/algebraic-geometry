(defpackage :messi-omurah
  (:use :common-lisp))

(in-package :messi-omurah)


(defun stop () (read-line))


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
  (let ((error-string "~2%Целостность параметров ЭК была нарушена. (Де)шифрование невозможно.")
        (test-1) (test-2) (test-3) (test-4))
    (when (null params)
      (format t error-string) (return-from ok-params?))
    (destructuring-bind (p b n r x y) params
      (setq test-1 (aux:miller-rabin p 25))
      (format t "~%Проверка целостности параметров: ~%
    Простота числа p:             ~a;" test-1)
      (when (not test-1)
        (format t error-string) (return-from ok-params?))
      (setq test-2 (zerop (mod n r)))
      (format t "~%~4tПорядок подгруппы r делит #E: ~a" test-2)
      (when (not test-2)
        (format t error-string) (return-from ok-params?))
      (setq test-3 (Q-belongs-curve (list x y) b p))
      (format t "~%~4tПринадлежность G кривой:      ~a;" test-3)
      (when (not test-3)
        (format t error-string) (return-from ok-params?))
      (setq test-4 (eql (ec-arith::scalar-product r (list x y) p)
                        EC-ARITH::'INF))
      (format t "~%~4tr * G = INF:                  ~a.~%" test-4)
      (when (not test-4)
        (format t error-string) (return-from ok-params?))) t))


(defun get-num-bits-to-cipher (max-bits)
  (let ((bits))
    (format t "~%Введите количество старших битов n, используемых для шифрования (1 < n < ~a): "
            (1+ max-bits))
    (tagbody try-again
       (setq bits (read))
       (when (not (and (integerp bits) (<= 2 bits max-bits)))
         (format t "~%Некорректный ввод! Попробуйте ввести количество старших битов, используемых для шифрования снова: ")
         (go try-again)))
    (aux:write-to-file (list bits) "bits-to-cipher") bits))


(defun get-message (p)
  (let ((message) (bit-len) (bits) (bound))
    (setq bit-len (1- (length (write-to-string p :base 2)))
          bits (get-num-bits-to-cipher bit-len)
          bound (1- (ash 1 bits)))
    (format t "~%Введите сообщение (число) m, которое хотите зашифровать (1 < m < ~a): " (1+ bound))
    (tagbody try-again
       (setq message (read))
       (when (not (and (integerp message) (<= 2 message bound)))
         (format t "~%Введённое значение не может быть зашифровано. Попробуйте ввести m снова: ")
         (go try-again)))
    (aux:write-to-file (list message) "message")
    (aux:write-to-file (list (length (write-to-string message :base 2)))
                       "bits-to-extract")))


(defun read-parse (filename)
  (parse-integer (uiop:read-file-line filename)))


(defun ec-equation (p b x &optional (a 0))
  (mod (+ (* x x x) (* a x) b) p))


(defun find-R-point (params bits message)
  (let* ((bin-message (write-to-string message :base 2))
         (len-bin-message (length bin-message)) (x) (y) (R-point)
         (p (car params)) (b (cadr params)) (r (nth 3 params)))
    (tagbody try-again
       (setq x (concatenate 'string bin-message
                            (if (/= bits len-bin-message)
                                (reduce #'(lambda (f s) (concatenate 'string f s))
                                        (loop for bit from 1 to (- bits len-bin-message)
                                              collect (write-to-string (random 2))))
                                ""))
             x (parse-integer x :radix 2)
             y (aux:sqrt-Zp (ec-equation p b x) p))
       (when (null y) (go try-again))
       (setq R-point (list x y))
       (when (not (eql (ec-arith::scalar-product r R-point p) EC-ARITH::'INF))
         (go try-again)))
    (aux:write-to-file R-point "r-point") R-point))


(defun ok-bits? (bits p)
  (when (not (<= 2 bits (1- (length (write-to-string p :base 2)))))
    (format t "~2%Таким количеством битов сообщение не может быть (де/за)шифровано! Завершение программы.")
    (return-from ok-bits?)) t)


(defun ok-message? (bits message)
  (when (not (<= 2 message (1- (ash 1 bits))))
    (format t "~%Сообщение (число) не лежит в диапазоне шифрования! Завершение программы.")
    (return-from ok-message?)) t)


(defun ok-point? (params point)
  (let ((p (car params)) (b (cadr params))
        (r (nth 3 params)))
    (when (not (Q-belongs-curve point b p))
      (format t "~%Заданная точка не принадлежит кривой! Завершение программы.")
      (return-from ok-point?))
    (when (not (eql (ec-arith::scalar-product r point p) EC-ARITH::'INF))
      (format t "~%Точка не порядка r! Завершение программы.")
      (return-from ok-point?)) t))


(defun step-1-encrypt (params bits message)
  (let ((R-point))
    (format t "~2%[1] -- Отправитель A встраивает сообщение в координату x точки R: ") (stop)
    (setq R-point (find-R-point params bits message))
    (format t "~%~4tR = (~{0x~x~^, ~});~%~4tm =  0b~a;~%~4tx =  0b~a;"
            R-point (write-to-string message :base 2) (write-to-string (car R-point) :base 2))
    (aux:write-to-file R-point "r-point")))


(defun step-2-encrypt (params)
  (let ((p (car params)) (n (caddr params))
        (a) (R-point) (aR))
    (format t "~2%[2] -- Отправитель A генерирует случайное число a, обратимое в Z/#E(K)Z и вычисляет aR: ")
    (aux:while (/= 1 (gcd (setq a (random n)) n)))
    (format t "~2%~4ta  =  0x~x; " a)
    (aux:write-to-file (list a) "a-mult") (stop)
    (setq R-point (mapcar #'parse-integer (uiop:read-file-lines "r-point")))
    (when (not (ok-point? params R-point)) (return-from step-2-encrypt))
    (format t "     R = (~{0x~x~^, ~});" R-point)
    (setq aR (ec-arith::scalar-product a R-point p))
    (format t "~%~4taR = (~{0x~x~^, ~})." aR)
    (aux:write-to-file aR "ar-point") t))


(defun step-3-encrypt (params)
  (let ((p (car params)) (n (caddr params))
        (b) (aR) (baR))
    (format t "~2%[3] -- Отправитель B генерирует случайное число b, обратимое в Z/#E(K)Z и вычисляет b(aR): ")
    (aux:while (/= 1 (gcd (setq b (random n)) n)))
    (format t "~2%~4tb   =  0x~x; " b)
    (aux:write-to-file (list b) "b-mult") (stop)
    (setq aR (mapcar #'parse-integer (uiop:read-file-lines "ar-point")))
    (when (not (ok-point? params aR)) (return-from step-3-encrypt))
    (format t "     aR = (~{0x~x~^, ~});" aR)
    (setq baR (ec-arith::scalar-product b aR p))
    (format t "~%~4tbaR = (~{0x~x~^, ~})." baR)
    (aux:write-to-file baR "bar-point") t))


(defun mo-encrypt-machinerie (params)
  (let* ((p (car params)) (bits) (message))
    (get-message p)
    (format t "~%~25t[ШИФРОВАНИЕ]")
    (setq bits (read-parse "bits-to-cipher"))
    (when (not (ok-bits? bits p)) (return-from mo-encrypt-machinerie))
    (setq message (read-parse "message"))
    (when (not (ok-message? bits message)) (return-from mo-encrypt-machinerie))
    (step-1-encrypt params bits message)
    (when (null (step-2-encrypt params)) (return-from mo-encrypt-machinerie))
    (when (null (step-3-encrypt params)) (return-from mo-encrypt-machinerie))))


(defun mo-encrypt ()
  (let ((params (get-params)))
    (when (not (ok-params? params)) (return-from mo-encrypt))
    (mo-encrypt-machinerie params)))


(defun ok-residue? (a n)
  (when (/= 1 (gcd a n))
    (format t "~2%Заданное число не имеет обратного по модулю #E(K)! Завершение программы.")
    (return-from ok-residue?)) t)


(defun step-1-decrypt (params)
  (let ((p (car params)) (n (caddr params))
        (a) (a-inv) (baR) (abaR))
    (format t "~2%[1] -- А умножает полученную точку на a^-1: ") (stop)
    (setq a (read-parse "a-mult"))
    (format t "~%~4ta                =  0x~x;" a)
    (when (not (ok-residue? a n)) (return-from step-1-decrypt))
    (setq a-inv (mod (cadr (aux:ext-gcd a n)) n))
    (format t "~%~4ta^-1 (mod #E(K)) =  0x~x;" a-inv)
    (setq baR (mapcar #'parse-integer (uiop:read-file-lines "bar-point")))
    (format t "~%~4tb(aR)            = (~{0x~x~^, ~});" baR)
    (when (not (ok-point? params baR)) (return-from step-1-decrypt))
    (setq abaR (ec-arith::scalar-product a-inv baR p))
    (aux:write-to-file (list abaR) "abar-point")
    (format t "~%~4ta^-1(baR)        = (~{0x~x~^, ~})." abaR) t))


(defun step-2-decrypt (params)
  (let ((p (car params)) (n (caddr params))
        (b) (b-inv) (abaR) (R))
    (format t "~2%[2] -- B умножает полученную точку на b^-1: ") (stop)
    (setq b (read-parse "b-mult"))
    (format t "~%~4tb                =  0x~x;" b)
    (when (not (ok-residue? b n)) (return-from step-2-decrypt))
    (setq b-inv (mod (cadr (aux:ext-gcd b n)) n))
    (format t "~%~4tb^-1 (mod #E(K)) =  0x~x;" b-inv)
    (setq abaR (mapcar #'parse-integer (uiop:read-file-lines "abar-point")))
    (format t "~%~4tbR               = (~{0x~x~^, ~});" abaR)
    (when (not (ok-point? params abaR)) (return-from step-2-decrypt))
    (setq R (ec-arith::scalar-product b-inv abaR p))
    (aux:write-to-file (list R) "r-decrypted")
    (format t "~%~4tR                = (~{0x~x~^, ~})." R) t))


(defun step-3-decrypt (params bits)
  (format t "~2%[3] -- B выделяет из точки R сообщение (число) m': ") (stop)
  (let ((decrypted) (message?) (message))
    (setq decrypted (mapcar #'parse-integer (uiop:read-file-lines "r-decrypted")))
    (format t "~%~4tR  = (~{0x~x~^, ~});" decrypted)
    (when (not (ok-point? params decrypted)) (return-from step-3-decrypt))
    (setq message? (write-to-string (car decrypted) :base 2)
          message? (parse-integer (subseq message? 0 bits) :radix 2)
          message (read-parse "message"))
    (format t "~%~4tx  =  0b~a;~%~4tm  =  0b~a;~%~4tm' =  0b~a;~%~4tm  =  0x~x;~%~4tm' =  0x~x."
            (write-to-string (car decrypted) :base 2) (write-to-string message :base 2)
            (write-to-string message? :base 2) (write-to-string message :base 16)
            (write-to-string message? :base 16))
    (format t (if (= message message?)
                  "~2%Расшифрование прошло успешно."
                  "~2%Расшифрование не удалось."))))


(defun mo-decrypt-machinerie (params)
  (let* ((p (car params)) (bits) (encrypted))
    (format t "~%~25t[ДЕШИФРОВАНИЕ]~2%Количество старших битов k, используемое для дешифрования (считывается из файла bits-to-extract): ") (stop)
    (setq bits (read-parse "bits-to-extract"))
    (format t "    n = ~a;" bits)
    (when (not (ok-bits? bits p)) (return-from mo-decrypt-machinerie))
    (format t "~2%Сообщение (число) m, зашифровано на точке: ") (stop)
    (setq encrypted (mapcar #'parse-integer (uiop:read-file-lines "bar-point")))
    (format t "    E(m) = (~{0x~x~^, ~});" encrypted)
    (when (not (ok-point? params encrypted)) (return-from mo-decrypt-machinerie))
    (when (not (step-1-decrypt params)) (return-from mo-decrypt-machinerie))
    (when (not (step-2-decrypt params)) (return-from mo-decrypt-machinerie))
    (when (not (step-3-decrypt params bits)) (return-from mo-decrypt-machinerie))))


(defun mo-decrypt ()
  (let ((params (get-params)))
    (when (not (ok-params? params)) (return-from mo-decrypt))
    (mo-decrypt-machinerie params)))
