(defun throw-die ()
  (1+ (random 7)))

(defun throw-dice ()
  (let ((roll1 (throw-die))
        (roll2 (throw-die)))
    (cons roll1 roll2)))

(defun snake-eyes-p (sling)
  (equal sling '(1 . 1)))

(defun boxcars-p (sling)
  (equal sling '(6 . 6)))

(defun instant-win-p (sling)
  (let ((roll (+ (car sling) (cdr sling))))
    (or (equal roll 7)
        (equal roll 11))))

(defun instant-loss-p (sling)
  (let ((roll (+ (car sling) (cdr sling))))
    (or (equal roll 2)
        (equal roll 3)
        (equal roll 12))))

(defun say-throw (sling)
  (let ((roll (+ (car sling) (cdr sling))))
    (cond ((equal roll 2) 'snake-eyes)
          ((equal roll 12) 'boxcars)
          (t roll))))

(defun craps ()
  (let* ((roll (throw-dice))
        (sum-roll (+ (car roll) (cdr roll))))
    (cond ((and
            (instant-loss-p roll)
            (snake-eyes-p roll))
           'snake-eyes-you-lose)
          ((and
            (instant-loss-p roll)
            (boxcars-p roll))
           'boxcars-you-lose)
          ((instant-loss-p roll) sum-roll 'you-lose)
          ((instant-win-p roll) sum-roll 'you-win)
          (t (list 'your-point-is sum-roll)))))

(defun try-for-point (x)
  (let* ((roll (throw-dice))
         (sum-roll (+ (car roll) (cdr roll))))
    (cond ((equal sum-roll x) 'you-win)
          ((equal sum-roll 7) 'you-lose)
          (t (list 'your-point-is sum-roll)))))





