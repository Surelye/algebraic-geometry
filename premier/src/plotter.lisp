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
           :width 800
           :height 800
           :mark :circle
           :data ,data
           :encoding (:x (:bin (:maxbins 200) :field :x-coord)
                      :y (:bin (:maxbins 200) :field :y-coord)
                      :size (:aggregate :count)))))
    (plot:plot histogram-scatterplot)))
