(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


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


(defun schnorr-sign-message ()
  (let ((k) (kQ) (message) (e) (s))
    (destructuring-bind (p r Q l) (extract-keys)
      (setq message (extract-message))
      (tagbody try-again-k
         (while (not (< 0 (setq k (random r)) r)))
         (setq kQ (ec-arith::scalar-product k Q p)
               e (sxhash (cons kQ message)))
         (when (zerop (mod e r)) (go try-again-k)))
      (setq s (mod (+ (* l e) k) r))
      (format t "~%Для подписи сообщения m отправитель:
    [1] -- Вырабатывает случайное целое число k, 0 < k < r = 0x~x;
    [2] -- Вычисляет точку R = kQ = (~{0x~x~^, ~});
    [3] -- Вычисляет хеш-функцию e = h(m, R) = 0x~x;
    [4] -- Вычисляет число s = l * e + k (mod r) = 0x~x.~%" k kQ e s)
      (format t "~%Подписанное сообщение было записано в файл signature.~%")
      (write-to-file (append message (list e s)) "signature"))))


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
       (when (< sig-len 3)
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


(defun schnorr-verify-signature ()
  (let ((R?) (e?) (keys (extract-keys :l-req 'NO-L :p-req 'P-NEEDED)))
    (setq keys (cons (car keys) (subseq keys 2)))
    (destructuring-bind (p Q lQ m e s) (append keys (extract-signature))
      (setq R? (ec-arith::add-points (ec-arith::scalar-product s Q p)
                                     (ec-arith::scalar-product e (get-minus-P lQ p) p)
                                     p)
            e? (sxhash (cons R? m)))
      (format t "~%Для проверки подписи получатель:
    [1] -- Вычисляет точку R' = (~{0x~x~^, ~});
    [2] -- Вычисляет хеш-функцию e' = h(m, R') = 0x~x;
    [3] -- Проверяет, что e = e' (mod r):
        e  = h(m, R)  = 0x~x;
        e' = h(m, R') = 0x~x.~%" R? e? e e?)
      (if (= e? e)
          (format t "~%Равенство выполняется. Подпись корректна.~%")
          (format t "~%Равенство не выполняется. Файл с сообщением или параметры подписи были изменены.~%")))))
