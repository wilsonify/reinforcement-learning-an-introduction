;(defparameter Gs (loop repeat 10000000 collect (sample)))
(defparameter n 0)
(defparameter sum 0)

(defun sample ()
  (let ((sample
         (loop for ratio = 2 then (* ratio 2)
           when (with-probability 0.5) return 0
           when (with-probability 0.1) return ratio)))
    (incf sum sample)
    (incf n)))

(defun MC (num-eps)
  (loop with v = 0.0
    for ep-num from 1 to num-eps
    for G in Gs
    collect (incf v (/ (- G v) ep-num))))

(defun samples (num)
  (loop repeat num do (sample))
  (/ (float sum) n))

(defun run (nlimit)
  (loop initially (setq n 0) (setq sum 0)
    while (< n nlimit)
    for next-num-samples = 1 then (max 1 (round (* n 0.01)))
    for est = (samples next-num-samples)
    collect (list (log n 10) est)))

(loop repeat 9 do (graph+ (run 100000000)))
(apply 'x-tick-marks (loop for logx from 0 to (log n 10) by 1 collect (list logx (format nil "~a" (expt 10 logx)))))
(y-tick-marks 0 1 2 4 8 16 32)
(y-graph-limits 0 3.5)
(grid-graph nil)