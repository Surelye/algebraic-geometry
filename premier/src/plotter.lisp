(asdf:load-system :plot/vega)

(in-package :ls-user)


(defparameter plots-loc
  '(:spec-loc #P"./info/plots/"
    :data-loc #P"./info/data/"
    :data-url #P"./info/data_url/"))


(defun draw-plot (xs ys)
  (let ((data (make-df '(:x-coord :y-coord)
                       (list (coerce xs 'vector) (coerce ys 'vector)))))
    (vega:plot-to-device plots-loc
                         (vega:defplot histogram-scatterplot
                             `(:title "График точек эллиптической кривой над конечным полем"
                               :width 1000
                               :height 1000
                               :mark :circle
                               :data ,data
                               :encoding (:x (:field :x-coord :type :quantitative)
                                          :y (:field :y-coord :type :quantitative)))))
    (plot:plot histogram-scatterplot)
    (format t "~a~%" (length xs))
    (uiop:run-program "./script.sh")))
