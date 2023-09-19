#lang racket

(define (throw-die)
  (random 1 7))

(define (throw-dice)
  (cons (throw-die) (cons (throw-die) '())))

(define (snake-eyes? sling)
  (equal? sling '(1 1)))

(define (boxcar? sling)
  (equal? sling '(6 6)))

(define (instant-win? sling)
  (let ((score (+ (car sling) (cadr sling))))
    (cond ((or (equal? score 7) (equal? score 11)))
          (true #f))))

(define (instant-loss? sling)
  (let ((score (+ (car sling) (cadr sling))))
  (cond ((or (equal? score 2) (equal? score 3) (equal? score 12)))
        (true #f))))

(define (say-throw sling)
  (let ((score (+ (car sling) (cadr sling))))
    (cond ((snake-eyes? sling) 'snake-eyes)
          ((boxcar? sling) 'boxcar)
          (true score))))

(define (craps)
  (let* ((throw (throw-dice)))
    (let ((point (+ (car throw) (cadr throw))))
      (cond ((snake-eyes? throw) (list 'throw 1 'and 1 '-- 'snake-eyes '-- 'you 'lose))
            ((boxcar? throw) (list 'throw 6 'and 6 '-- 'boxcar '-- 'you 'lose))
            ((instant-win? throw) (list 'throw (car throw) 'and (cadr throw) '-- point '-- 'you 'win))
            (true (list 'throw (car throw) 'and (cadr throw) '-- 'your 'point 'is point))))))

(define (try-for-point point)
  (let* ((throw (throw-dice)))
    (let ((new-point (+ (car throw) (cadr throw))))
    (cond ((equal? point new-point) (list 'throw (car throw) 'and (cadr throw) '-- new-point '-- 'you 'win))
          ((equal? new-point 7) (list 'throw (car throw) 'and (cadr throw) '-- new-point '-- 'you 'lose))
          (true (list 'throw (car throw) 'and (cadr throw) '-- 'your 'point 'is new-point '-- 'throw 'again))))))
