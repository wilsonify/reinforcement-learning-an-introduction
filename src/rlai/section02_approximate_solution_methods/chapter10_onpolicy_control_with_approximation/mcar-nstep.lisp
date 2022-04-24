;;; Mountain car in Lisp
;;; n-step methods with tile coding
;;; state is a cons (x . xdot)

(defun mcar-init ()
  (cons (+ -0.6 (random 0.2))
        0))

(defun mcar-sample (s a)
  (check-type a (integer 0 2) "an action 0, 1, or 2")
  (destructuring-bind (x . xdot) s
    (incf xdot (- (* 0.001 (- a 1))
                  (* 0.0025 (cos (* 3 x)))))
    (bound2 xdot -0.07 0.6999999)
    (incf x xdot)
    (values -1 (cond ((>= x 0.5) nil)
                     ((< x -1.2) (cons -1.2 0))
                     (t (cons x xdot))))))

(defun terminalp (s)
  (null s))

(defparameter num-tilings 8)
(defparameter max-tiles 4096)
(defparameter epsilon 0)
(defparameter iht (make-iht max-tiles))
(defparameter theta (make-array max-tiles :element-type 'float))
(defparameter max-episode-length 10000)
(defparameter Rstore (make-array (* 2 max-episode-length) :element-type 'float))
(defparameter Sstore (make-array (* 2 max-episode-length) :element-type 'cons))
(defparameter Astore (make-array (* 2 max-episode-length) :element-type '(integer 0 2)))
(defparameter alpha (/ 0.5 num-tilings))
(defparameter gamma 1)
(defparameter n 1)

(defun init ()
  (loop for i below max-tiles do (setf (aref theta i) (* .00001 (random-normal)))))

(defun mcar-tiles (s a)
  (destructuring-bind (x . xdot) s
    (tiles iht num-tilings (list (/ (* x 8)
                                    (- 0.5 -1.2))
                                 (/ (* xdot 8)
                                    (- 0.07 -0.07)))
           (list a))))

(defun egreedy (s)
  (with-prob epsilon
    (random 3)
    (argmax (qs s))))

(defun qs (s)
  (loop for a below 3 collect (q s a)))

(defun q (s a)
  (loop for i in (mcar-tiles s a) sum (aref theta i)))

(defun q-from-tiles (tiles)
  (loop for i in tiles sum (aref theta i)))

(defun episode ()
  (setf (aref Sstore 0) (mcar-init))
  (setf (aref Astore 0) (egreedy (aref Sstore 0)))
  (loop with capT = 1000000 with Rsum = 0
    for tt from 0
    for tt+1 = (+ tt 1)
    for tau = (+ tt (- n) 1)
    for tau+n = (+ tau n)
    when (< tt capT)
    do (multiple-value-bind (R Sprime) (mcar-sample (aref Sstore tt) (aref Astore tt))
         (setf (aref Rstore tt+1) R)
         (incf Rsum R)
         (if (terminalp Sprime)
           (setq capT tt+1)
           (progn (setf (aref Sstore tt+1) Sprime)
             (setf (aref Astore tt+1) (egreedy Sprime)))))
    when (>= tau 0)
    do (loop 
         with G =  (+ (loop for i from (+ tau 1) to (min tau+n capT) sum (aref Rstore i))
                      (if (< tau+n capT) (q (aref Sstore tau+n) (aref Astore tau+n)) 0))
         with tiles = (mcar-tiles (aref Sstore tau) (aref Astore tau))
         with alpha-error = (* alpha (- G (q-from-tiles tiles)))
         for i in tiles do (incf (aref theta i) alpha-error))
    when (= tau (- capT 1))
    return Rsum
    when (and (>= tt max-episode-length)
              (= capT 1000000))
    do (format t "~%Episode ran too long ~a ~a" n alpha) 
    and return Rsum))

(defun episode-old ()
  (setf (aref Sstore 0) (mcar-init))
  (setf (aref Astore 0) (egreedy (aref Sstore 0)))
  (loop for time below max-episode-length do
    (multiple-value-bind (R Sprime) (mcar-sample (aref Sstore time) (aref Astore time))
      (setf (aref Rstore (1+ time)) R)
      (if (terminal Sprime)
        (return (loop with end-time = (1+ time)
                  for tau from (- end-time n) below end-time
                  for G = (loop for i from (1+ tau) to end-time sum (aref Rstore i))
                  for tiles = (mcar-tiles (aref Sstore tau) (aref Astore tau))
                  for alpha-error = (* alpha (- G (q-from-tiles tiles)))
                  do (loop for i in tiles do (incf (aref theta i) alpha-error))
                  finally (return (loop for time from 1 to end-time 
                                    sum (aref Rstore time))))))
      (setf (aref Sstore (1+ time)) Sprime)
      (setf (aref Astore (1+ time)) (egreedy Sprime)))
    (let ((tau (+ time (- n) 1)))
      (when (>= tau 0)
        (let* ((G (loop for i from (1+ tau) to (+ tau n) sum (aref Rstore i)))
               (tiles (mcar-tiles (aref Sstore tau) (aref Astore tau)))
               (alpha-error (* alpha (- G (q-from-tiles tiles)))))
          (loop for i in tiles do (incf (aref theta i) alpha-error)))))))
  
(defun run (num-episodes)
  (init)
  (loop repeat num-episodes collect (- (episode))))

(defun runs (num-runs num-episodes num-tilings-arg n-arg alpha-arg)
  (setq num-tilings num-tilings-arg)
  (setq n n-arg)
  (setq alpha (/ alpha-arg (float num-tilings)))
  (print (butlast (record-auto num-runs num-episodes 
                      (loop repeat num-runs collect (run num-episodes))))))

(defun nstep ()
  (loop for alpha-power from -2 by 0.2
    for alpha = (expt 2 alpha-power)
    while (< alpha 2) do
    (runs 100 50 8 1 alpha)
    (nstep-graph))
  (loop for alpha-power from -2 by 0.2
    for alpha = (expt 2 alpha-power)
    while (< alpha 1.7) do
    (runs 100 50 8 2 alpha)
    (nstep-graph))
  (loop for alpha-power from -3 by 0.2
    for alpha = (expt 2 alpha-power)
    while (< alpha 1.5) do
    (runs 100 50 8 4 alpha)
    (nstep-graph))
  (loop for alpha-power from -3 by 0.2
    for alpha = (expt 2 alpha-power)
    while (< alpha .9) do
    (runs 100 50 8 8 alpha)
    (nstep-graph))
  (loop for alpha-power from -3 by 0.2
    for alpha = (expt 2 alpha-power)
    while (< alpha .6) do
    (runs 100 50 8 32 alpha)
    (nstep-graph)))


(defun nstep-graph ()
  (clear-graph)
  (loop for n-power from 0 to 4 do
    (graph+ (loop for r in (sort (records :n (expt 2 n-power) :num-runs 100)
                                 #'< :key (lambda (x) (record-value x :alpha)))
              collect (list (* (record-value r :alpha) (record-value r :num-tilings))
                            (mean (multi-mean (record-value r :data))))))))

#|(clear-graph)
(graph+ (multi-mean (runs 30 200 8 8 0.1)))
(y-tick-marks 100 200 1000)
(y-graph-limits 100 1000)
(grid-graph nil)
(x-tick-marks 0 0.5 1 1.5 2)
(x-graph-limits 1 501)
(y-graph-limits 220 260)
(y-tick-marks 200 220 230 240 250 260 280 300)

(runs 100 500 8 1 0.7)
(graph (loop for r in (records :num-episodes 500 :num-runs 100) collect (multi-mean (record-value r :data))))
(graph (loop for r in (records :num-episodes 500 :num-runs 100 :n 1) collect (loop for d in (multi-mean (record-value r :data)) collect (log d 10))))
(y-graph-limits 1.9 3)
(apply #'y-tick-marks (loop for num in '(100 200 400 1000) collect (list (log num 10) (format nil "~a" num))))
(graph+ (loop for r in (nconc (records :num-episodes 500 :num-runs 100 :n 8 :alpha (/ 0.3 8))
) collect (loop for d in (multi-mean (record-value r :data)) collect (log d 10))) :purple)
|#
























