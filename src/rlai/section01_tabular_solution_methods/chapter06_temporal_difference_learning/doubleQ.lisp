;-*- Package: (cl-user) -*-

#|
Here we use Double Q-learning.

Hado van Hasselt's episodic MDP (figure 4.1 of his thesis) meant to
illustrate the maximization bias of Q-learning.  There are two nonterminal states, A and B, 
and two terminal states at each end. Each episode starts in A, with two possible actions.
The 'right' action deterministically takes the agent to termination with a reward of -1, 
which is the best that can be done in this problem. The 'wrong' action deterministically takes 
the agent to state B with a reward of -1.1. From B, there are 10 actions, all of which 
deterministically take the agent to termination. The reward on all 10 actions has expected 
value 0, but variance 1. We plot the probability (averaged over many random runs)
that the agent prefers the wrong action over the right action as a function of episode number.
We apply Q-learning with an e-greedy behavior policy. The initial action-value estimates are 
zero.

gamma is assumed 1
|#

(defparameter alpha 0.1)
(defparameter epsilon 0.1)

(defparameter Q (make-array (list 2 10)))
(defparameter Qright (make-array 2))
(defparameter Qwrong (make-array 2))

(defun init ()
  (fill Qright 0.0)
  (fill Qwrong 0.0)
  (fill-array Q 0.0))

(defun episode ()
  (let ((sum-Qright (+ (aref Qright 0) (aref Qright 1)))
        (sum-Qwrong (+ (aref Qwrong 0) (aref Qwrong 1))))
    (cond ((< (random 1.0) epsilon) (go-random))
          ((> sum-Qright sum-Qwrong) (go-right))
          ((< sum-Qright sum-Qwrong) (go-left))
          (t (go-random)))))

(defun go-random ()
  (if (= (random 2) 0)
    (go-right)
    (go-left)))

(defun go-right ()
  (let ((e (random 2)))  ;pick which estimator to update
    (incf (aref Qright e) (* alpha (- 0.0 (aref Qright e))))
    0))

(defun go-left ()
  (let ((e (random 2)))  ;pick which estimator to update
    (incf (aref Qwrong e) 
          (* alpha (+ 0.0 (aref Q (- 1 e) (argmax-single e)) (- (aref Qwrong e))))))
  (let ((a (argmax-double)))
    (when (< (random 1.0) epsilon)
      (setq a (random 10)))
    (let ((e (random 2)))  ;pick which estimator to update
      (incf (aref Q e a) (* alpha (- (random-normal) .1 (aref Q e a))))))
  1)

(defun run (num-episodes)
  (init)
  (loop repeat num-episodes 
    for a = (episode)
    collect a))
    ;do (show a Qright Qwrong Q)))
      
(defun argmax-single (e)
  "return first best action according to e-th estimator"
  (loop with best-arg = 0
        with best-value = (aref Q e 0)
        for i from 1 below 10
        for value = (aref Q e i) 
        do (when (> value best-value) 
             (setq best-value value)
             (setq best-arg i))
        finally (return best-arg)))

(defun argmax-double ()
  "return sample of best actions by summing both estimators"
  (loop with best-args = (list 0)
        with best-value = (+ (aref Q 0 0) (aref Q 1 0))
        for i from 1 below 10
        for value = (+ (aref Q 0 i) (aref Q 1 i)) 
        do (cond ((< value best-value))
                 ((> value best-value)
                  (setq best-value value)
                  (setq best-args (list i)))
                 ((= value best-value)
                  (push i best-args)))
        finally (return (values (nth (random (length best-args))
                                     best-args)
                                best-value))))

;do below first for the single definitions
;(graph+ (multi-mean (loop repeat 10000 collect (run 300))))
;(y-tick-marks 0 .25 .5 .75 1)
;(x-tick-marks 0 100 200 300)
      