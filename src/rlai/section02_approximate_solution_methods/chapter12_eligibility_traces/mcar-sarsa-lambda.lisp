;;; Mountain car in Lisp
;;; Sarsa(lambda) with tile coding
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
    (list -1 (cond ((>= x 0.5) nil)
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
(defparameter e  (make-array max-tiles :element-type 'float))
(defparameter alpha (/ 0.5 num-tilings))
(defparameter gamma 1)
(defparameter lambda 0.9)

(defun init ()
  (loop for i below max-tiles do (setf (aref theta i) (* .00001 (random-normal)))))

(defun mcar-tiles (s a)
  (unless (terminalp s)
    (destructuring-bind (x . xdot) s
      (tiles iht num-tilings (list (/ (* x 8)
                                      (- 0.5 -1.2))
                                   (/ (* xdot 8)
                                      (- 0.07 -0.07)))
             (list a)))))

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
  (loop with delta
    initially (fill e 0.0) 
    repeat max-episode-length
    for S = (mcar-init) then Sprime
    for A = (egreedy S) then Aprime
    for tiles = (mcar-tiles S A) then tiles
    for (R Sprime) = (mcar-sample S A)
    for Aprime = (unless (terminalp Sprime) (egreedy Sprime))
    do 
    (loop for tile in tiles do (setf (aref e tile) 1))
    (setq delta (- R (q-from-tiles tiles)))
    (setq tiles (mcar-tiles Sprime Aprime))
    (setq delta (+ delta (* gamma (q-from-tiles tiles))))
    (loop for i below max-tiles with alpha-delta = (* alpha delta) do
         (incf (aref theta i) (* alpha-delta (aref e i)))
         (setf (aref e i) (* gamma lambda (aref e i))))
    sum R into Rsum
    until (terminalp Sprime)
    finally (when (not (terminalp Sprime)) 
              (format t "~%Episode ran too long ~a ~a" lambda alpha))
    (return Rsum)))

(defun run (num-episodes)
  (init)
  (loop repeat num-episodes collect (- (episode))))

(defun runs (num-runs num-episodes num-tilings-arg lambda-arg alpha-arg)
  (setq num-tilings num-tilings-arg)
  (setq lambda lambda-arg)
  (setq alpha (/ alpha-arg (float num-tilings)))
  (print (butlast (record-auto num-runs num-episodes 
                      (loop repeat num-runs collect (run num-episodes))))))

(defun exper ()
  (loop for lambda-arg in '(.96 .92 .98 .99 .84 .68 0 1) do
  (loop for alpha-power from -2 below 1 by 0.2
    for alpha = (expt 2 alpha-power) do
    (runs 100 50 8 lambda-arg alpha)
    (lambda-graph))))

(defun lambda-graph ()
  (clear-graph)
  (loop for l in '(.96 .92 .98 .99 .84 .68 0 1) do
    (graph+ (loop for r in (sort (records :lambda l :num-runs 100)
                                 #'< :key (lambda (x) (record-value x :alpha)))
              collect (list (* (record-value r :alpha) (record-value r :num-tilings))
                            (mean (multi-mean (record-value r :data))))))))

#|(clear-graph)
(graph+ (multi-mean (runs 30 200 8 8 0.1)))
(y-tick-marks 100 200 1000)
(y-graph-limits 100 1000)
(grid-graph nil)
(x-tick-marks 0 0.5 1 1.5)
(x-graph-limits 1 501)
(y-graph-limits 170 300)
(y-tick-marks 180 200 220 240 260 280 300)

(runs 100 500 8 1 0.7)
(graph (loop for r in (records) collect (multi-mean (record-value r :data))))
(graph (loop for r in (records :num-episodes 500 :num-runs 100 :n 1) collect (loop for d in (multi-mean (record-value r :data)) collect (log d 10))))
(y-graph-limits 1.9 3)
(apply #'y-tick-marks (loop for num in '(100 200 400 1000) collect (list (log num 10) (format nil "~a" num))))
(graph+ (loop for r in (nconc (records :num-episodes 500 :num-runs 100 :n 8 :alpha (/ 0.3 8))
) collect (loop for d in (multi-mean (record-value r :data)) collect (log d 10))) :purple)
(prepare-for-recording! "~/Dropbox/2nd Edition/code/chap12/mcar lisp/mcar-lambda.data"  :NUM-RUNS :NUM-EPISODES :NUM-TILINGS :lambda :ALPHA :DATA)
|#
























