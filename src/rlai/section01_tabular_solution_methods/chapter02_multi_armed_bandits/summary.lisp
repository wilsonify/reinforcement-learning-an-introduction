; To run, call (setup), (init), and then, e.g., (runs 2000 1000 .1)

(defvar n)
(defvar alpha 0.2)
(defvar Q*)
(defvar H)
(defvar Rbar)
(defvar policy)
(defvar max-num-tasks 10000)

(defvar Q)
(defvar Qtemp)
(defvar n_a)


(defun setup ()
  (setq n 10)
  (setq H (make-array n))
  (setq policy (make-array n))
  (setq Q (make-array n))
  (setq Qtemp (make-array n))
  (setq n_a (make-array n))
  (setq Q* (make-array (list n max-num-tasks)))
  (standardize-random-state)
  (advance-random-state 0)
  (loop for task below max-num-tasks do
    (loop for a below n do 
      (setf (aref Q* a task) (random-normal)))))

(defun init (Q0)
  (setf Rbar 0.0)
  (loop for a below n do
        (setf (aref Q a) Q0)
        (setf (aref n_a a) 0)
        (setf (aref H a) 0.0)))

(defun runs (&optional alg (num-runs 1000) (num-steps 100) alpha-arg)
  (setq alpha alpha-arg)
  (loop for run-num below num-runs
        for a* = 0
        do (loop for a from 1 below n 
                 when (> (aref Q* a run-num)
                         (aref Q* a* run-num))
                 do (setq a* a))
        do (init (if (eq alg 'oi) alpha 0.0))
        collect (loop for time-step below num-steps with A and R and sumR = 0 do
                  (if (eq alg 'gb)
                    (loop for a below n sum (setf (aref policy a) (exp (aref H a))) into sum-expH
                      finally (loop for a below n do (setf (aref policy a) (/ (aref policy a) sum-expH)))))
                  (setf A (ecase alg
                            (gb (random-from-simplex-array policy))
                            (eg (epsilon-greedy alpha))
                            (ucb (if (< time-step n) time-step (UCB-selection time-step alpha)))
                            (oi (greedy Q))))
                  (setf R (reward A run-num))
                  (incf sumR R)
                  (learn alg A R time-step)
                  finally (return sumR))))

(defvar data)
(defun ex (alg aa) (quiet (push (print (cons alg (cons aa (stats (runs alg 2000 1000 aa))))) data)))

#|
(progn
  (graph (cddr (sort (loop for d in data 
                 when (eq 'eg (first d)) 
                 collect (list (log (second d) 2) (/ (third d) 1000))) #'< :key #'first)))
  (graph+ (cdr (sort (loop for d in data 
                 when (eq 'gb (first d)) 
                 collect (list (log (second d) 2) (/ (third d) 1000))) #'< :key #'first)))
  (graph+ (butlast (cddddr (sort (loop for d in data 
                 when (eq 'ucb (first d)) 
                 collect (list (log (second d) 2) (/ (third d) 1000))) #'< :key #'first))))
  (graph+ (butlast (butlast (cdr (sort (loop for d in data 
                 when (eq 'oi (first d)) 
                 collect (list (log (second d) 2) (/ (third d) 1000))) #'< :key #'first)))))
  (y-tick-marks 0.9 1 1.1 1.2 1.3 1.4 1.5)
  (apply #'x-tick-marks (loop for e from -7 to 2 collect (list e (format nil "~a" (expt 2 e)))))
  (y-graph-limits 1 1.5))
(loop for e from -3 to 4 do (ex 'oi (float (expt 2 e))))
|#

(defun learn (alg A R time-step)
  (ecase alg
    (gb 
     (incf Rbar (/ (- R Rbar) (1+ time-step)))
     (let ((alpha-delta (* alpha (- R Rbar))))
       (loop for a below n do (decf (aref H a) (* alpha-delta (aref policy a))))
       (incf (aref H A) alpha-delta)))
    ((ucb eg)
     (incf (aref n_a A))
     (incf (aref Q A) (/ (- R (aref Q A))
                         (aref n_a A))))
    (oi
     (incf (aref Q A) (* 0.1 (- R (aref Q A)))))))

(defun reward (a task-num)
  (+ (aref Q* a task-num)
     (random-normal)))

(defun max-Q* (num-tasks)
  (mean (loop for task below num-tasks 
              collect (loop for a below n 
                            maximize (aref Q* a task)))))

(defun UCB-selection (time c)
  (loop for a below n
    with lnt = (log time)
    do (setf (aref Qtemp a) (+ (aref Q a) (* c (sqrt (/ lnt (aref n_a a))))))
    finally (return (arg-max-random-tiebreak Qtemp))))
  
(defun epsilon-greedy (epsilon)
  (with-prob epsilon 
    (random n)
    (arg-max-random-tiebreak Q)))

(defun greedy (Q)
  (arg-max-random-tiebreak Q))

(defun arg-max (q)
  (position (reduce #'max q) q))

(defun arg-max-random-tiebreak (array)
  "Returns index to a random instance of the largest value in the array"
  (loop with best-args = (list 0)
        with best-value = (aref array 0)
        for i from 1 below (length array)
        for value = (aref array i) 
        do (cond ((< value best-value))
                 ((> value best-value)
                  (setq best-value value)
                  (setq best-args (list i)))
                 ((= value best-value)
                  (push i best-args)))
        finally (return (values (nth (random (length best-args))
                                     best-args)
                                best-value))))





