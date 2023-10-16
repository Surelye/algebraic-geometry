(defpackage #:ec-arith-2
  (:use :cl))

(in-package #:ec-arith-2)


(defvar *p* 2)

(defvar *mod-poly*)


(defun get-binary (num)
  (let* ((str (write-to-string num :base *p*))
         (len (length str)) (res))
    (setq res (make-array len :initial-contents (reverse str))
          res (map 'vector #'digit-char-p res))
    (list res len)))


(defun get-coord (str-hex)
  (get-binary (parse-integer str-hex :radix 16)))


(defvar *curves*
  (list :k233 (list :f '(progn (setq *mod-poly* (make-array 234 :initial-element 0))
                               (setf (svref *mod-poly*   0) 1
                                     (svref *mod-poly*  74) 1
                                     (svref *mod-poly* 233) 1)
                               (setq *mod-poly* (list *mod-poly* 234)) (list *mod-poly* 234))
                    :h 4
                    :n 3450873173395281893717377931138512760570940988862252126328087024741343
                    :a 0
                    :b 1
                    :gx (get-coord "17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126")
                    :gy (get-coord "1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3"))

        :k283 (list :f '(progn (setq *mod-poly* (make-array 284 :initial-element 0))
                               (setf (svref *mod-poly*   0) 1
                                     (svref *mod-poly*   5) 1
                                     (svref *mod-poly*   7) 1
                                     (svref *mod-poly*  12) 1
                                     (svref *mod-poly* 283) 1)
                               (setq *mod-poly* (list *mod-poly* 284)) (list *mod-poly* 284))
                    :h 4
                    :n 3885337784451458141838923813647037813284811733793061324295874997529815829704422603873
                    :a 0
                    :b 1
                    :gx (get-coord "503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836")
                    :gy (get-coord "1ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259"))

        :k409 (list :f '(progn (setq *mod-poly* (make-array 410 :initial-element 0))
                               (setf (svref *mod-poly*   0) 1
                                     (svref *mod-poly*   8) 1
                                     (svref *mod-poly* 409) 1)
                               (setq *mod-poly* (list *mod-poly* 410)) (list *mod-poly* 410))
                    :h 4
                    :n 330527984395124299475957654016385519914202341482140609642324395022880711289249191050673258457777458014096366590617731358671
                    :a 0
                    :b 1
                    :gx (get-coord "060f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746")
                    :gy (get-coord "1e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b"))

        :k571 (list :f '(progn (setq *mod-poly* (make-array 572 :initial-element 0))
                               (setf (svref *mod-poly*   0) 1
                                     (svref *mod-poly*   2) 1
                                     (svref *mod-poly*   5) 1
                                     (svref *mod-poly*  10) 1
                                     (svref *mod-poly* 571) 1)
                               (setq *mod-poly* (list *mod-poly* 572)) (list *mod-poly* 572))
                    :h 4
                    :n 1932268761508629172347675945465993672149463664853217499328617625725759571144780212268133978522706711834706712800825351461273674974066617311929682421617092503555733685276673
                    :a 0
                    :b 1
                    :gx (get-coord "26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972")
                    :gy (get-coord "349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3"))))


(defmacro while (condition &body body)
  `(do () ((not ,condition))
     ,@body))


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


(defun pdf (f s)
  (let* ((deg-f (1- (cadr f))) (deg-s (1- (cadr s)))
         (deg-dif (- deg-f deg-s)) (fc) (quot))
    (when (< deg-dif 0) (return-from pdf (list '(#(0) 1) f)))
    (setq fc (copy-seq (car f))
          quot (make-array (1+ deg-dif) :initial-element 0))
    (do ((k (- deg-f deg-s) (1- k)))
        ((= -1 k) (list (list quot (1+ deg-dif))
                        (list (subseq fc 0 deg-s) deg-s)))
      (setf (svref quot k) (svref fc (+ deg-s k)))
      (do ((j (+ deg-s k -1) (1- j))) ((< j k))
        (setf (svref fc j) (logxor (svref fc j) (* (svref quot k)
                                                   (svref (car s) (- j k)))))))))


(defun poly*-machinerie (f s)
  (let* ((f-deg (1- (cadr f))) (s-deg (1- (cadr s)))
         (*-deg (+ f-deg s-deg)) (sum-idx) (pos)
         (*-poly (make-array (1+ *-deg) :initial-element 0)))
    (do ((i 0 (1+ i))) ((> i f-deg))
      (unless (zerop (svref (car f) i))
        (do ((j 0 (1+ j))) ((> j s-deg))
          (unless (zerop (svref (car s) j))
            (setq sum-idx (+ i j))
            (setf (svref *-poly sum-idx) (logxor (svref *-poly sum-idx) 1))))))
    (setq pos (position-if-not #'zerop *-poly :from-end t))
    (if (null pos) '(#(0) 1)
        (list (subseq *-poly 0 (1+ pos)) (1+ pos)))))


(defun poly* (&rest polys)
  (reduce #'poly*-machinerie polys))


(defun zero-poly? (f)
  (every #'zerop (car f)))


(defun poly+-machinerie (f s)
  (let ((len-f (cadr f))
        (len-s (cadr s)) (res) (pos)
        (routine (lambda (fp sp sp-len)
                   (do ((i 0 (1+ i))) ((= sp-len i) fp)
                     (setf (svref fp i) (logxor (svref fp i) (svref sp i)))))))
    (setq res (if (> len-f len-s)
                  (funcall routine (copy-seq (car f)) (car s) len-s)
                  (funcall routine (copy-seq (car s)) (car f) len-f))
          pos (position-if-not #'zerop res :from-end t))
    (if (null pos) (list #(0) 1)
        (list (subseq res 0 (1+ pos)) (1+ pos)))))


(defun poly+ (&rest polys)
  (reduce #'poly+-machinerie polys))


(defun xea-p (f s)
  ;(when (or (zero-poly? s) (< (length f) (length s)))
  ;  (return-from xea-p))
  (let ((p-0 f) (p-1 s) ;(g-0 #(1)) (g-1 #(0))
        (f-0 '(#(0) 1)) (f-1 '(#(1) 1)) (q))
    (while (not (zero-poly? p-1))
      (setq q (car (pdf p-0 p-1)))
      (psetq p-0 p-1 p-1 (poly+ p-0 (poly* p-1 q))
             ;g-0 g-1 g-1 (poly+ g-0 (poly* g-1 q p) p)
             f-0 f-1 f-1 (poly+ f-0 (poly* f-1 q)))) f-0))
    ;(list p-0 g-0 f-0)))


(defun equal-poly? (f s)
  (every #'= (car f) (car s)))


(defun poly-mod-pow (base-poly power mod-poly)
  (setq base-poly (cadr (pdf base-poly mod-poly)))
  (do ((product '(#(1) 1))) ((zerop power) product)
    (do () ((oddp power))
      (setq base-poly (cadr (pdf (poly* base-poly base-poly) mod-poly))
            power (ash power -1)))
    (setq product (cadr (pdf (poly* product base-poly) mod-poly))
          power (1- power))))


(defun red (poly)
  (cadr (pdf poly *mod-poly*)))


(defun add-points (P Q)
  (when (equal P "INF") (return-from add-points Q))
  (when (equal Q "INF") (return-from add-points P))
  (let* ((Px (car P)) (Py (cadr P))
         (Qx (car Q)) (Qy (cadr Q))
         (Rx) (Ry) (frac) (inv) (sqr))
    (cond
      ((not (equal-poly? Px Qx)) (setq inv (xea-p *mod-poly* (poly+ Px Qx))
                                       frac (red (poly* (poly+ Py Qy) inv))
                                       Rx (poly+ (red (poly* frac frac)) frac Px Qx)
                                       Ry (poly+ (red (poly* (poly+ frac '(#(1) 1)) Rx))
                                                 (red (poly* (poly+ (red (poly* Py Qx))
                                                                    (red (poly* Qy Px))) inv)))))
      (t (cond
           ((equal-poly? Py Qy) (setq inv (xea-p *mod-poly* Px)
                                      sqr (red (poly* Px Px))
                                      frac (red (poly* (poly+ sqr Py) inv))
                                      Rx (poly+ (red (poly* frac frac)) frac)
                                      Ry (poly+ (red (poly* (poly+ frac '(#(1) 1)) Rx)) sqr)))
           (t (return-from add-points "INF")))))
    (list (red Rx) (red Ry))))


(defun scalar-product (n P)
  (let ((result "INF") (addend P))
    (while (not (zerop n))
      (when (= 1 (logand n 1))
        (setq result (add-points addend result)))
      (setq addend (add-points addend addend)
            n (ash n -1))) result))


(defun curve-setter (key)
  (eval (getf (getf *curves* key) :f))
  (remove-if #'keywordp (subseq (getf *curves* key) 2)))


(defun curve-setter-wrapper (c)
  (cond ((= 1 c) (curve-setter :k233))
        ((= 2 c) (curve-setter :k283))
        ;((= 3 c) (curve-setter :k409))
        (t       (curve-setter :k571))))


(defun print-poly (c)
  (let ((cfs (cond ((= 1 c) '(233 74     1))
                   ((= 2 c) '(283 12 7 5 1))
                   ;((= 3 c) '(409    8   1))
                   (t       '(571 10 5 2 1)))))
    (format t "    f(z) = ~{z^~a~^ ~^+ ~};~%" cfs)))


(defun get-num (poly)
  (reduce #'+ (map 'vector #'* (car poly)
                   (loop for pow from 0 to (1- (cadr poly))
                         collect (ash 1 pow)))))


(defun get-curve ()
  (let ((c -1)) ;[3] -- K-409
    (format t "~%Введите номер рабочей кривой:~%
    [1] -- K-233;~%~4t[2] -- K-283;
~4t[3] -- K-571;
    [0] -- Выход.~2%Ваш выбор: ")
    (while (not (member (setq c (read)) '(1 2 3 0)));4 0)))
      (format t "Некорректный ввод! Попробуйте ввести номер кривой снова: "))
    (when (zerop c) (return-from get-curve))
    (destructuring-bind (h n a b gx gy) (curve-setter-wrapper c)
      (format t "~%Была выбрана кривая со следующими параметрами:~2%")
      (print-poly c)
      (format t "~4th    = ~a;~%~4tn    = 0x~x;
    a    = ~a;~%~4tb    = ~a;
    G    = (~{0x~x~^,~%~11t ~}).~%" h n a b (mapcar #'get-num (list gx gy)))
      (list h n a b (list gx gy)))))
