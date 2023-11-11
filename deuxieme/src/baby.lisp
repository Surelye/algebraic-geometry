(defun get-param (opt)
  (let* ((hello-string) (error-string) (opt-str (write-to-string opt))
         (condition (lambda (n) (and (integerp n) (>= n 0)))) (cond*)
         (param))
    (cond ((or (eql 'A opt) (eql 'B opt))
           (setq hello-string (concatenate 'string "Введите значение коэффициента "
                                           opt-str ": ")
                 error-string (concatenate 'string "Некорректное значение коэффициента "
                                           opt-str ". Попробуйте ввести его снова: ")
                 cond* condition))
          ((eql 'P opt)
           (setq hello-string (concatenate 'string "Введите значение модуля "
                                           opt-str " (простое число): ")
                 error-string (concatenate 'string "Некорректное значение модуля "
                                           opt-str ". Попробуйте ввести его снова: ")
                 cond* (lambda (p) (and (funcall condition p)
                                        (aux::miller-rabin p)))))
          (t (return-from get-param)))
    (format t hello-string)
    (aux::while (not (funcall cond* (setq param (read))))
      (format t error-string)) param))


(defun zero-discrim? (a b p)
  (zerop (mod (+ (* 4 a a a) (* 27 b b)) p)))


(defun ec-eq (x a b p)
  (mod (+ (* x x x) (* a x) b) p))


(defun get-random-point (a b p)
  (let* ((x) (y-sqr) (y))
    (tagbody generate-point
       (setq x (random p)
             y-sqr (ec-eq x a b p))
       (when (/= 1 (aux::compute-legendre y-sqr p))
         (go generate-point))
       (setq y (aux::seq-sqrt-Zp y-sqr p))
       (when (/= y-sqr (mod (* y y) p))
         (go generate-point)))
    (list x y)))


(defun get-minus-P (P modulo)
  (when (equal "INF" P) (return-from get-minus-p "INF"))
  (list (car P) (mod (- modulo (cadr P)) modulo)))


(defun get-table (a r-P p s)
  (let ((cur-P r-P) (table `(,(get-minus-p r-P p) ,r-P "INF")))
    (do ((scalar 2 (1+ scalar))) ((> scalar s) table)
      (setq cur-P (ec-arith::add-points a cur-P r-P p)
            table (adjoin (get-minus-P cur-P p)
                          (adjoin cur-P table :test #'equal)
                          :test #'equal)))))


(defun get-ts (a R Q p s table)
  (let ((pos-RiQ R) (neg-RiQ R) (-Q (get-minus-p Q p))
        (multiplier (1+ (* 2 s))) (is) (pairs)
        (js (loop for j from (- s) to s collect j)))
    (when (member R table :test #'equal) (setq is (cons 0 is)))
    (do ((i 1 (1+ i))) ((> i s))
      (setq pos-RiQ (ec-arith::add-points a pos-RiQ  Q p)
            neg-RiQ (ec-arith::add-points a neg-RiQ -Q p))
      (when (member pos-RiQ table :test #'equal)
        (setq is (cons i is)))
      (when (member neg-RiQ table :test #'equal)
        (setq is (cons (- i) is))))
    (dolist (i is (mapcar #'(lambda (pair)
                              (- (* multiplier (car pair)) (cdr pair))) pairs))
      (setq pairs (append (mapcar #'(lambda (j) (cons i j)) js)
                          pairs)))))


(defun only-ords? (a r-P p ords?)
  (setq ords? (remove-if #'minusp ords?))
  (remove-if-not #'(lambda (ord?)
                     (equal "INF" (ec-arith::scalar-product a ord?
                                                            r-P p))) ords?))


(defun baby-step-giant-step (&optional (points-to-check 10))
  (let ((a (get-param 'A)) (b (get-param 'B))
        (p (get-param 'P)) (r-P) (s) (table)
        (Q) (R) (ts) (succ-p) (ord) (cur-point 0))
    (aux::while (zero-discrim? a b p)
      (format t "Дискриминант равен нулю. Попробуйте другие входные данные.~%")
      (setq a (get-param 'A) b (get-param 'B) p (get-param 'P)))
    (setq a (mod a p) b (mod b p)
          succ-p (1+ p) s (ceiling (sqrt (sqrt p))))
    (tagbody regenerate
       (setq r-P (get-random-point a b p)
             table (get-table a r-P p s)
             Q (ec-arith::scalar-product a (1+ (* 2 s)) r-P p)
             R (ec-arith::scalar-product a succ-p r-P p)
             ts (get-ts a R Q p s table))
       (when (null ts) (go regenerate))
       (format t "~%Был получен параметр:~%    s = [sqrt(sqrt(P))] = ~a.~%" s)
       (format t "~%Были определены точки:~%    L = (~{~a~^, ~});~%" r-P)
       (if (equal "INF" Q)
           (format t "    Q = [2s + 1]L = ~a;~%" Q)
           (format t "    Q = [2s + 1]L = (~{~a~^, ~});~%" Q))
       (if (equal "INF" R)
           (format t "    R = [P + 1]L = ~a;~%" R)
           (format t "    R = [P + 1]L = (~{~a~^, ~}).~%" R))
       (setq ord (mapcar #'(lambda (t-val) (+ succ-p t-val)) ts))
       (aux::while (> points-to-check cur-point)
         (setq ord (only-ords? a r-P p ord))
         (when (null ord)
           (setq cur-point 0)
           (format t "~%Кандидатов на значение порядков не найдено! Завершение программы.")
           (return-from baby-step-giant-step))
         (setq r-P (get-random-point a b p)
               cur-point (1+ cur-point)))
       (format t "~%Был найден порядок эллиптической кривой:~%    #E = ~a.~%" (car ord)))
    (car ord)))
