; baird's counterexample, continuing state-value case, with explicit feature vectors.
; comparing off-policy TD(0) with emphatic TD(0)
; the states are numbered 0-6; the special state is 6
; the last, special feature is here the 7th feature of 0-7

(use-package :matrix)

(defparameter NN 7)           ; number of states
(defparameter NN-1 (1- NN))
(defparameter spec-s NN-1)    ; the special state
(defparameter n 8)            ; number of features
(defparameter spec-f (1- n))  ; the special feature

(defparameter alpha 0.005)
(defparameter gamma 0.99)
(defparameter beta 0.05)

(defvar s)
(defvar th)
(defvar th-new)
(defvar w)
(defvar w-new)
(defvar phi)
(defvar MM)

(defvar mat)

(defun setup ()
  (setq phi (make-array NN))
  (loop for s below NN
    for x = (make-array n :initial-element 0) do
    (setf (aref phi s) x)
    (if (= s spec-s)
      (progn (setf (aref x s) 1)
             (setf (aref x spec-f) 2))
      (progn (setf (aref x s) 2)
             (setf (aref x spec-f) 1))))
  (setq mat (make-matrix n n))
  (loop for s below NN 
    for x = (aref phi s) do
    (loop for i below n do
      (loop for j below n do
        (incf (aref mat i j) (* (aref x i) (aref x j))))))
  (minverse mat)
  (setq mat (m* mat NN))
  (setq th (make-array n))
  (setq th-new (make-array n))
  (setq w (make-array n))
  (setq w-new (make-array n)))

(defun dot (w x)
  (loop for i below n sum (* (aref w i) (aref x i))))

(defun init ()
  (setq MM 1)
  (setq s (random NN))
  (fill w 0)
  (fill th 1)
  (setf (aref th NN-1) 10))

(defun vth (s)
  (if (= s spec-s)
    (+ (aref th s) (* 2 (aref th spec-f)))
    (+ (* 2 (aref th s)) (aref th spec-f))))

(defun predw (s)
  (if (= s spec-s)
    (+ (aref w s) (* 2 (aref w spec-f)))
    (+ (* 2 (aref w s)) (aref w spec-f))))

(defun exp-delta-phi-TDC ()
  (loop for s below NN
    with edphi = (make-array n :initial-element 0)
    for rho = (/ 7);(if a 7 0)
    for sp = spec-s;(if a spec-s (random NN-1))
    for x = (aref phi s)
    for delta = (- (* gamma (vth sp)) (vth s))
    for rho-delta = (* rho delta)
    do (loop for i below n do
         (incf (aref edphi i) (* rho-delta (aref x i))))
    finally (return edphi)))

(defun exp-delta-phi ()
  (loop for s below NN
    with edphi = (make-array n :initial-element 0)
    with rho = (/ 7);(if a 7 0)
    with sp = spec-s;(if a spec-s (random NN-1))
    for x = (aref phi s)
    for delta = (- (* gamma (vth sp)) (vth s))
    for MM = (if (= s spec-s) 100 1)
    for rho-MM-delta = (* rho MM delta)
    do (loop for i below n do
         (incf (aref edphi i) (* rho-MM-delta (aref x i))))
    finally (return edphi)))

(defun rmspbe ()
  (let ((edphi (exp-delta-phi)))
    (sqrt (dot edphi (m* mat edphi)))))

(defun rmsve ()
  (sqrt (/ (loop for s below NN sum (square (vth s))) nn)))

(defun sweeps-exp-TDC (num-sweeps)
  (loop repeat num-sweeps do
    (loop for i below n do (setf (aref th-new i) (aref th i)))
    (loop for i below n do (setf (aref w-new i) (aref w i)))
    (loop for s below NN
      ;for a = t;(with-probability (/ 1 7.0))   ; true action is solid, on-policy action
      for rho = (/ 7);(if a 7 0)
      for sp = spec-s;(if a spec-s (random NN-1))
      for x = (aref phi s)
      for xp = (aref phi sp)
      for delta = (- (* gamma (vth sp)) (vth s))
      for alpha-rho-delta = (* alpha rho delta)
      for alpha-rho-gamma-pred = (* alpha rho gamma (predw s))
      for beta-rho-error = (* beta rho (- delta (predw s)))
      do (loop for i below n do
           (incf (aref th-new i) (- (* alpha-rho-delta (aref x i))
                                (* alpha-rho-gamma-pred (aref xp i))))
           (incf (aref w-new i) (* beta-rho-error (aref x i)))))
    (swap th th-new)
    (swap w w-new)
    collect (cons (rmspbe) (cons (rmsve) (l th)))))

(defun steps-TDC (num-steps)
  (loop repeat num-steps
    for a = (with-probability (/ 1 7.0))   ; true action is solid, on-policy action
    for rho = (if a 7 0)
    for sp = (if a spec-s (random NN-1))
    for x = (aref phi s)
    for xp = (aref phi sp)
    for delta = (- (* gamma (vth sp)) (vth s))
    for alpha-rho-delta = (* alpha rho delta)
    for alpha-rho-gamma-pred = (* alpha rho gamma (predw s))
    for beta-rho-error = (* beta rho (- delta (predw s)))
    do (loop for i below n do
         (incf (aref th i) (- (* alpha-rho-delta (aref x i))
                              (* alpha-rho-gamma-pred (aref xp i))))
         (incf (aref w i) (* beta-rho-error (aref x i))))
    do (setq s sp)
    collect (cons (rmspbe) (cons (rmsve) (l th)))))

(defun sweeps-exp-ETD (num-sweeps)
  (loop repeat num-sweeps do
    (loop for i below n do (setf (aref th-new i) (aref th i)))
    (loop for s below NN
      for sp = spec-s;(if a spec-s (random NN-1))
      for x = (aref phi s)
      for delta = (- (* gamma (vth sp)) (vth s))
      for exp-MM = (if (= s spec-s) 100 1)
      for alpha-delta = (* (/ 7.0) alpha exp-MM delta)
      do (loop for i below n do
           (incf (aref th-new i) (* alpha-delta (aref x i)))))
    (swap th th-new)
    collect (cons (rmsve) (l th))))

(defun steps-ETD-ave-update-test (num-steps)
  (loop for time from 1 to num-steps
    for a = (with-probability (/ 1 7.0))   ; true action is solid, on-policy action
    for rho = (if a 7 0)
    for sp = (if a spec-s (random NN-1))
    for x = (aref phi s)
    for delta = (- (* gamma (vth sp)) (vth s))
    for alpha-MM-rho-delta = (* alpha MM rho delta)
    sum (* alpha-MM-rho-delta (aref x 7)) into total
;    do (loop for i below n do
;         (incf (aref th i) (* alpha-MM-rho-delta (aref x i))))
    do (setq s sp)
    do (setq MM (+ 1 (* gamma rho MM))) ; 
    finally (return (/ total num-steps))))
;    when (= 0 (mod time 10000)) collect (cons (rmsve) (l th))))

(defun steps-ETD (num-steps)
  (loop for time from 1 to num-steps
    for a = (with-probability (/ 1 7.0))   ; true action is solid, on-policy action
    for rho = (if a 7 0)
    for sp = (if a spec-s (random NN-1))
    for x = (aref phi s)
    for delta = (- (* gamma (vth sp)) (vth s))
    for alpha-MM-rho-delta = (* alpha MM rho delta)
    do (loop for i below n do
         (incf (aref th i) (* alpha-MM-rho-delta (aref x i))))
    do (setq s sp)
    do (setq MM (+ 1 (* gamma rho MM))) ; 
    when (= 0 (mod time 10000)) collect (cons (rmsve) (l th))))

#| 
(progn
  (init)
  (graph (reorder-list-of-lists (cons (cons (rmsve) (l th)) (steps-ETD 100000000))) nil "ETD"))

(progn
  (init)
  (graph (reorder-list-of-lists (cons (cons (rmsve) (l th)) (sweeps-exp-ETD 1000))) nil "exp-ETD"))

(progn
  (init)
  (graph (reorder-list-of-lists (cons (cons (rmspbe) (cons (rmsve) (l th))) (steps-TDC 1000))) nil "TDC"))

(progn
  (init) ; 
  (graph (reorder-list-of-lists (cons (cons (rmspbe) (cons (rmsve) (l th))) (sweeps-exp-TDC 1000))) nil "Exp TDC"))

(progn
  (init)
  (graph (reorder-list-of-lists (cons (l th) (stepsTD 1000))) nil "TD"))

(progn
  (init)
  (graph (reorder-list-of-lists (cons (l th) (sweeps-exp-TD 1000))) nil "Exp TD"))

(progn
  (init)
  (graph (reorder-list-of-lists (cons (l th) (loop repeat 10000 collect (episodeETD)))) nil "ETD"))

(grid-graph nil)
(y-tick-marks -5 0 2 5 10)
(y-tick-marks 1 10 100 200 300)
(x-tick-marks '(1 "0") '(1001 "1000"))
(y-graph-limits -2.67 10)
(y-graph-limits .8 336)
(y-graph-limits nil)

(defun runETD (num-steps)
  ;(init)
  (loop for step below num-steps
    for s = last-sp then sp
    for sp = (with-prob (/ 6 7.0) (random NN-1) spec-s)
    for f = 1 then (+ f 1)
    for rho = (if (= spec-s sp) 7 0)
    for delta = (- (* gamma (vth sp)) (vth s))
    for alpha-f-rho = (* alpha f rho)
    ;when (= sp spec-s) do (print (list s f))
    do 
    (when (> alpha-f-rho max-step) 
      (print (list alpha-f-rho alpha))
      (setq alpha (* alpha (/ max-step alpha-f-rho)))
      (setq alpha-f-rho (* alpha f rho))
      (print (list alpha-f-rho alpha "new")))
    (setq delta (* delta alpha-f-rho))
    (if (= s spec-s)
      (progn 
        (incf (aref th s) delta)
        (incf (aref th spec-f) (* 2 delta)))
      (progn
        (incf (aref th s) (* 2 delta))
        (incf (aref th spec-f) delta)))
    (setf f (* gamma rho f))
    when (= 0 (mod step 100000)) collect (loop for s below NN collect (vth s));(msve)
    finally (setq last-sp sp)))

(defun msve ()
  (/ (loop for s below NN sum (square (vth s)))
     7.0))


(defparameter d (make-array 5 :initial-contents '(0 0 0 0 1 )))
(defparameter newd (make-array 5))
(defparameter lprob 0.7)

(loop repeat 10 do
  (setf (aref newd 0) (* lprob (+ (aref d 0) (aref d 1))))
  (loop for s from 1 to 3
    for r = (min (+ s 1) 4)
    for l = (max (- s 1) 0)
    do (setf (aref newd s) (+ (* lprob (aref d r)) (* (- 1 lprob) (aref d l)))))
  (setf (aref newd 4) (* (- 1 lprob) (+ (aref d 4) (aref d 3))))
  (swap d newd)
  finally (print (loop for s below 5 sum (aref d s))) (return d))

; 0.6  #(0.38388643 0.25592428 0.17061615 0.11374411 0.075829394) 
; 2/3  #(0.51612914 0.25806454 0.12903228 0.06451613 0.032258067)
; 0.7  #(0.5798116 0.24849069 0.10649602 0.045641158 0.019560497)
; 0.75 #(0.66942155 0.22314052 0.074380174 0.024793392 0.008264464)

|#






