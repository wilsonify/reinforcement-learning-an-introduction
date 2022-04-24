;-*- Package: (cl-user) -*-

#|
Here we implement Hado van Hasselt's episodic MDP (figure 4.1 of his thesis) meant to
illustrate the maximization bias of Q-learning.  There are two nonterminal states, A and B, 
and two terminal states at each end. Each episode starts in A, with two possible actions.
The 'right' action deterministically takes the agent to termination with a reward of -1, 
which is the best that can be done in this problem. The 'wrong' action deterministically takes 
the agent to state B with a reward of -1.1. From B, there are 10 actions, all of which 
deterministically take the agent to termination. The reward on all 10 actions has expected value 0, but variance 1. We plot the probability (averaged over many random runs)
that the agent prefers the wrong action over the right action as a function of episode number.
We apply Q-learning with an e-greedy behavior policy. The initial action-value estimates are 
zero.

gamma is assumed 1
|#

(defparameter alpha 0.1)
(defparameter epsilon 0.1)

(defparameter Q (make-array 10))
(defparameter Qright nil)
(defparameter Qwrong nil)

(defun init ()
  (setq Qright 0.0)
  (setq Qwrong 0.0)
  (fill Q 0.0))

(defun episode ()
  (cond ((< (random 1.0) epsilon) (go-random))
        ((> Qright Qwrong) (go-right))
        ((< Qright Qwrong) (go-left))
        (t (go-random))))

(defun go-random ()
  (if (= (random 2) 0)
    (go-right)
    (go-left)))

(defun go-right ()
  (incf Qright (* alpha (- 0.0 Qright)))
  0)

(defun go-left ()
  (incf Qwrong (* alpha (+ 0.0 (maximum Q) (- Qwrong))))
  (let ((a (argmax-random-tiebreak Q)))
    (when (< (random 1.0) epsilon)
      (setq a (random 10)))
    (incf (aref Q a) (* alpha (- (random-normal) .1 (aref Q a)))))
  1)

(defun run (num-episodes)
  (init)
  (loop repeat num-episodes 
    for a = (episode)
    collect a))
    ;do (show a Qright Qwrong Q)))
      
(defun argmax-random-tiebreak (array)
  "Returns index to random instance of the largest value in the array"
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

(defun maximum (Q)
  (loop for Q-i across Q maximize Q-i))

      