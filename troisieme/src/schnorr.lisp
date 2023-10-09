(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun stop () (read-line))


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (datum data)
      (if (atom datum)
          (format out "~a~%" datum)
          (format out "~a ~a~%" (car datum) (cadr datum))))))


(defun schnorr-generate-keys ()
  (let ((l) (lQ))
    (destructuring-bind (p b r Q)
        (gen-ec::generate-curve)
      (setq l (+ 2 (random (- r 2)))
            lQ (ec-arith::scalar-product l Q p))
      (format t "~%Был сгенерирован открытый ключ проверки подписи:
    характеристика поля p = 0x~x;
    коэффициент b уравнения ЭК = 0x~x;
    порядок r циклической подгруппы = 0x~x;
    образующий элемент Q порядка r = (~{0x~a~^, ~});
    точка P = lQ = (~{0x~a~^, ~}).~%"
              p b r Q lQ)
      (format t "~%Был сгенерирован секретный ключ формирования подписи:
    показатель l = 0x~x.~%" l)
      (format t "~%Значение открытого ключа проверки подписи было записано в файл public-key.~%")
      (write-to-file (list p b r Q lQ) "public-key")
      (format t "Значение секретного ключа формирования подписи было записано в файл private-key.~%")
      (write-to-file (list l) "private-key"))))


(defun extract (filename &optional (opt))
  (let ((extr) (len-extr) (keys))
    (handler-case (setq extr (uiop:read-file-lines filename))
      (error (err)
        (format t "В ходе программы было выполнено ошибочное условие:~%~a~%" err)
        (return-from extract)))
    (setq extr (mapcar #'(lambda (str)
                           (uiop:split-string str :separator " ")) extr))
    (handler-case
        (setq extr (mapcar #'parse-integer (apply #'append extr)))
      (error (err)
        (format t "В ходе работы программы было выполнено ошибочное условие:~%~a~%" err)
        (return-from extract)))
    (setq len-extr (length extr)
          keys (list (nth 0 extr) (nth 2 extr)
                       (list (nth 3 extr) (nth 4 extr))))
    (cond ((= 1 len-extr) extr)
          ((= 7 len-extr) (if (eql opt 'P-NEEDED)
                              (append keys (list (list (nth 5 extr) (nth 6 extr))))
                              keys))
          (t nil))))


(defun extract-wrapper (default-filename &optional (opt))
  (let ((filename) (extracted))
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename)) (setq filename default-filename))
       (when (null (setq extracted (extract filename opt)))
         (format t "Введите название файла снова: ") (go try-again))) extracted))


(defun extract-keys (&key (l-req) (p-req))
  (let ((extracted-public) (extracted-private))
    (format t "Введите название файла, в котором содержится открытый ключ проверки подписи (по умолчанию public-key): ")
    (setq extracted-public (extract-wrapper "public-key" p-req))
    (when (null l-req)
      (format t "Введите название файла, в котором содержится секретный ключ формирования подписи (по умолчанию private-key): ")
      (setq extracted-private (extract-wrapper "private-key")))
    (append extracted-public extracted-private)))


(defun extract-message ()
  (let ((filename-message) (message))
    (format t "Введите имя файла, в котором содержится подписываемое сообщение (по умолчанию message): ")
    (tagbody try-again-message
       (setq filename-message (read-line))
       (when (zerop (length filename-message)) (setq filename-message "message"))
       (handler-case (setq message (uiop:read-file-lines filename-message))
         (error (err)
           (format t "В ходе работы программы было выполнено ошибочное условие:~%~a~%" err)
           (format t "Введите имя файла с сообщением снова: ")
           (go try-again-message))))
    message))


(defun read-parse (file)
  (parse-integer (uiop:read-file-line file)))


(defun schnorr-sign-message ()
  (let ((k) (kQ) (message) (e) (s) (b))
    (destructuring-bind (p r Q l) (extract-keys)
      (setq b (parse-integer (uiop:read-file-line "public-key" :at 1)))
      (setq message (extract-message))
       (unless (belongs-to-curve Q b p)
        (format t "Точка Q не принадлежит ЭК!~%")
        (return-from schnorr-sign-message))
     (format t "~%Для подписи сообщения m отправитель:~% ")
      (tagbody try-again-k
         (while (not (< 0 (setq k (random r)) r)))
         (write-to-file (list k) "sign-k")
         (format t "~%    [1] -- Вырабатывает случайное целое число k, 0 < k < r = 0x~x;~% "
                 k)
         (format t "~%    [2] -- Вычисляет точку R = kQ: ") (stop)
         (setq k (read-parse "sign-k")
               kQ (ec-arith::scalar-product k Q p))
          (unless (belongs-to-curve kQ b p)
           (format t "Точка R не принадлежит ЭК!~%")
           (return-from schnorr-sign-message))
        (write-to-file (list kQ) "sign-R")
         (format t "           k = 0x~x;
           R = (~{0x~x~^, ~});~% " k kQ)
         (format t "~%    [3] -- Вычисляет значение хеш-функции e = h(m, R): ") (stop)
         (setq kQ (mapcar #'parse-integer
                          (uiop:split-string (uiop:read-file-line "sign-R")
                                             :separator " "))
               e (sxhash (cons kQ message)))
         (unless (belongs-to-curve kQ b p)
           (format t "Точка R не принадлежит ЭК!~%")
           (return-from schnorr-sign-message))
         (write-to-file (list e) "sign-e")
         (format t "           R = (~{0x~x~^, ~});
           e = 0x~x;~% " kQ e)
         (when (zerop (mod e r)) (go try-again-k)))
      (format t "~%    [4] -- Вычисляет число s = l * e + k (mod r): ") (stop)
      (setq k (read-parse"sign-k")
            e (read-parse "sign-e")
            s (mod (+ (* l e) k) r))
      (write-to-file (list s) "sign-s")
      (format t "           k = 0x~x;~%           e = 0x~x;
           s = 0x~x.~%" k e s)
      (format t "~%Подписанное сообщение было записано в файл signature.~%")
      (write-to-file (list e s) "signature"))))


(defun extract-signature ()
  (let ((filename) (signature) (sig-len) (e-idx) (s-idx)
        (at-e-idx) (at-s-idx)
        (print-error-string
          (lambda (err) (format t "В ходе работы программы было выполнено ошибочное условие:~%~a~%" err)))
        (try-again-signed
           '(format t "Введите имя файла с подписанным сообщением снова: ")))
    (format t "Введите имя файла, в котором содержится подписанное сообщение (по умолчанию signature): ")
    (tagbody try-again-filename
       (setq filename (read-line))
       (when (zerop (length filename)) (setq filename "signature"))
       (handler-case (setq signature (uiop:read-file-lines filename))
         (error (err)
           (funcall print-error-string err)
           (eval try-again-signed)
           (go try-again-filename)))
       (setq sig-len (length signature))
       (when (< sig-len 2)
         (format t "Введённое имя файла не соответствует подписанному сообщению!~%")
         (eval try-again-signed)
         (go try-again-filename))
       (setq s-idx (1- sig-len) at-s-idx (nth s-idx signature)
             e-idx (1-   s-idx) at-e-idx (nth e-idx signature))
       (handler-case (setf (nth e-idx signature) (setq at-e-idx (parse-integer at-e-idx))
                           (nth s-idx signature) (setq at-s-idx (parse-integer at-s-idx)))
         (error (err)
           (funcall print-error-string err)
           (eval try-again-signed))))
    (list (subseq signature 0 e-idx) at-e-idx at-s-idx)))


(defun get-minus-P (P m)
  (when (equal "INF" P) (return-from get-minus-P "INF"))
  (when (zerop (cadr P)) (return-from get-minus-P P))
  (list (car P) (- m (cadr P))))


(defun belongs-to-curve (Q b p)
  (let ((x (car Q)) (y (cadr Q)))
    (= (mod (* y y) p) (mod (+ (* x x x) b) p))))


(defun schnorr-verify-signature ()
  (let ((R?) (e?) (keys (extract-keys :l-req 'NO-L :p-req 'P-NEEDED))
        (filename) (message) (b))
    (setq keys (cons (car keys) (subseq keys 2)))
    (destructuring-bind (p Q lQ m e s) (append keys (extract-signature))
      (setq b (parse-integer (uiop:read-file-line "public-key" :at 1)))
       (unless (belongs-to-curve Q b p)
        (format t "Точка Q не принадлежит ЭК!~%")
        (return-from schnorr-verify-signature))
     (format t "Введите имя файла, в котором содержится сообщение (по умолчанию message): ")
      (setq filename (read-line))
      (when (zerop (length filename))
        (setq filename "message")
        (setq message (uiop:read-file-lines filename)))
      (format t "~%Для проверки подписи получатель:~% ")
      (setq e (read-parse "sign-e")
            s (read-parse "sign-s"))
      (setq R? (ec-arith::add-points (ec-arith::scalar-product s Q p)
                                     (ec-arith::scalar-product e (get-minus-P lQ p) p)
                                     p))
       (unless (belongs-to-curve R? b p)
        (format t "Точка R не принадлежит ЭК!~%")
        (return-from schnorr-verify-signature))
      (write-to-file (list R?) "verif-R")
      (format t "~%    [1] -- Вычисляет точку R' = sQ - eP: ") (stop)
      (setq s (read-parse "sign-s")
            e (read-parse "sign-e")
            R? (ec-arith::add-points (ec-arith::scalar-product s Q p)
                                     (ec-arith::scalar-product e (get-minus-P lQ p) p) p))
      (write-to-file (list R?) "verif-R")
      (format t "           s = 0x~x;~%           e = 0x~x;
           R' = (~{0x~x~^, ~});~%" s e R?)
      (format t "~%    [2] -- Вычисляет значение хеш-функции e' = h(m, R'): ") (stop)
      (setq R? (mapcar #'parse-integer
                       (uiop:split-string (uiop:read-file-line "verif-R")
                                          :separator " "))
            e? (sxhash (cons R? message)))
      (unless (belongs-to-curve R? b p)
        (format t "Точка R' не принадлежит ЭК!~%")
        (return-from schnorr-verify-signature))
      (write-to-file (list e?) "verif-e")
      (format t "           R' = (~{0x~x~^, ~});~%           e' = 0x~x;~% " R? e?)
      (format t "~%    [3] -- Проверяет, что e = e' (mod r): ") (stop)
      (setq e (read-parse "sign-e")
            e? (read-parse"verif-e"))
      (format t "           e?  = h(m, R) = 0x~x;~%           e' = h(m, R') = 0x~x.~%"
              e e?)
      (if (= e? e)
          (format t "~%Равенство выполняется. Подпись корректна.~%")
          (format t "~%Равенство не выполняется. Файл с сообщением или параметры подписи были изменены.~%")))))
