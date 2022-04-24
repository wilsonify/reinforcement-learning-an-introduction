; To run, call (setup), (init), and then, e.g., (runs 2000 1000 .1)

(defvar n)
(defvar alpha 0.2)
(defvar Q*)
(defvar H)
(defvar Rbar)
(defvar policy)
(defvar max-num-tasks 10000)

(defun setup ()
  (setq n 10)
  (setq H (make-array n))
  (setq policy (make-array n))
  (setq Q* (make-array (list n max-num-tasks)))
  (standardize-random-state)
  (advance-random-state 0)
  (loop for task below max-num-tasks do
    (loop for a below n do 
      (setf (aref Q* a task) (random-normal)))))

(defun init ()
  (setf Rbar 0.0)
  (loop for a below n do
        (setf (aref H a) 0.0)))

(defun runs (&optional (num-runs 1000) (num-steps 100) alpha-arg)
  (setq alpha alpha-arg)
  (loop with average-reward = (make-array num-steps :initial-element 0.0)
        with prob-a* = (make-array num-steps :initial-element 0)
        for run-num below num-runs
        for a* = 0
        do (loop for a from 1 below n 
                 when (> (aref Q* a run-num)
                         (aref Q* a* run-num))
                 do (setq a* a))
        do (init)
        collect (loop for time-step below num-steps with A and R do
                  (loop for a below n sum (setf (aref policy a) (exp (aref H a))) into sum-expH
                    finally (loop for a below n do (setf (aref policy a) (/ (aref policy a) sum-expH))))
                  (setf A (random-from-simplex-array policy))
                  (setf R (+ 4 (reward A run-num)))
                  (learn A R time-step)
                  (incf (aref average-reward time-step) R)
                  (when (= A a*) (incf (aref prob-a* time-step))))
        finally (return (let ((avefn (lambda (x) (/ x num-runs))))
                          (values (map 'list avefn average-reward)
                                  (map 'list avefn prob-a*))))))

;(multiple-value-bind (ave-rew prob-a*) (runs 2000 1000 .1) (graph+ prob-a* :blue))
;(y-tick-marks 0 .2 .4 .6 .8 1)
;(x-tick-marks 0 250 500 750 1000)

(defun learn (A R time-step)
  (incf Rbar (/ (- R Rbar) (1+ time-step)))
  (let ((alpha-delta (* alpha (- R Rbar))))
    (loop for a below n do (decf (aref H a) (* alpha-delta (aref policy a))))
    (incf (aref H A) alpha-delta)))

(defun reward (a task-num)
  (+ (aref Q* a task-num)
     (random-normal)))

(defun max-Q* (num-tasks)
  (mean (loop for task below num-tasks 
              collect (loop for a below n 
                            maximize (aref Q* a task)))))





