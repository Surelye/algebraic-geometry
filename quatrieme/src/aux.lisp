(defpackage :aux
  (:use :common-lisp))

(in-package :aux)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun mod-expt (base power divisor)
  (setq base (mod base divisor))
  (do ((product 1))
      ((zerop power) product)
    (do () ((oddp power))
      (setq base (mod (* base base) divisor)
            power (ash power -1)))
    (setq product (mod (* product base) divisor)
          power (1- power))))
