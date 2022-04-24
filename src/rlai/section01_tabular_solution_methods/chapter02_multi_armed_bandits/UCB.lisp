; To run, call (setup), (init), and then, e.g., (runs 2000 1000 .1)

(defvar n)
(defvar c 1)
(defvar Q*)
(defvar Q)
(defvar Qtemp)
(defvar n_a)
(defvar max-num-tasks 10000)

(defun setup ()
  (setq n 10)
  (setq Q (make-array n))
  (setq Qtemp (make-array n))
  (setq n_a (make-array n))
  (setq Q* (make-array (list n max-num-tasks)))
  (standardize-random-state)
  (advance-random-state 0)
  (loop for task below max-num-tasks do
    (loop for a below n do 
      (setf (aref Q* a task) (random-normal)))))

(defun init ()
  (loop for a below n do
        (setf (aref Q a) 0.0)
        (setf (aref n_a a) 0)))

(defun runs (&optional (num-runs 1000) (num-steps 100) (c-arg 0))
  (setq c c-arg)
  (loop with average-reward = (make-array num-steps :initial-element 0.0)
        with prob-a* = (make-array num-steps :initial-element 0)
        for run-num below num-runs
        for a* = 0
        do (loop for a from 1 below n 
                 when (> (aref Q* a run-num)
                         (aref Q* a* run-num))
                 do (setq a* a))
        do (init)
        collect (loop for time-step below num-steps
                      for a = (if (< time-step n) time-step (UCB-selection time-step));(epsilon-greedy 0.1)
                      for r = (reward a run-num)
                      do (learn a r)
                      do (incf (aref average-reward time-step) r)
                      do (when (= a a*) (incf (aref prob-a* time-step))))
        finally (return (let ((avefn (lambda (x) (/ x num-runs))))
                          (values (map 'list avefn average-reward)
                                  (map 'list avefn prob-a*))))))

;(multiple-value-bind (ave-rew prob-a*) (runs 2000 1000 2.0) (graph+ ave-rew :blue))
;(y-tick-marks 0 .5 1 1.5)
;(x-tick-marks 0 250 500 750 1000)

(defun learn (a r)
  (incf (aref n_a a))
  (incf (aref Q a) (/ (- r (aref Q a))
                      (aref n_a a))))

(defun reward (a task-num)
  (+ (aref Q* a task-num)
     (random-normal)))


(defun UCB-selection (time)
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

(defun max-Q* (num-tasks)
  (mean (loop for task below num-tasks 
              collect (loop for a below n 
                            maximize (aref Q* a task)))))