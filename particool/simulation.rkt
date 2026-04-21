#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define paused #f)
(define background (rectangle 200 200 "solid" "white"))
(struct particle (c v mass))

(define (particles world)
  (cadr world))

(define (draw-particle p bg)
  (let ((radius 4) ; todo: vary this based on p's mass
        )
    (place-image (circle radius "solid" "blue") (vector-ref (particle-c p) 0) (vector-ref (particle-c p) 1) bg)))

(define (draw-state s)
  ; draw every particle!
  (define (helper p)
    (if (null? p)
        background
        (draw-particle (car p) (helper (cdr p)))))
  (helper (particles s)))
; draw all particles stored in s:
; that means draw the first particle on top of the drawing the rest

;; START: AI-generated code     
(define G 0.5)
(define DT 1)
(define MIN-DIST 10.0)
(define COLLISION-DIST 12.0)
(define RESTITUTION 0.5)
(define MAX-SPEED 5.0)

(define (vec+ a b)
  (vector (+ (vector-ref a 0) (vector-ref b 0))
          (+ (vector-ref a 1) (vector-ref b 1))))

(define (vec- a b)
  (vector (- (vector-ref a 0) (vector-ref b 0))
          (- (vector-ref a 1) (vector-ref b 1))))

(define (vec* s v)
  (vector (* s (vector-ref v 0))
          (* s (vector-ref v 1))))

(define (vec-dot a b)
  (+ (* (vector-ref a 0) (vector-ref b 0))
     (* (vector-ref a 1) (vector-ref b 1))))

(define (vec-mag v)
  (sqrt (vec-dot v v)))

(define (gravity-accel p other)
  (let* ((diff (vec- (particle-c other) (particle-c p)))
         (dist (max MIN-DIST (vec-mag diff)))
         (force-mag (/ (* G (particle-mass other)) (* dist dist))))
    (vec* (/ force-mag dist) diff)))

(define (total-accel p others)
  (foldl (lambda (other acc)
           (if (eq? p other) acc (vec+ acc (gravity-accel p other))))
         (vector 0 0) others))

(define (elastic-collision p1 p2)
  (let* ((dx (vec- (particle-c p1) (particle-c p2)))
         (dv (vec- (particle-v p1) (particle-v p2)))
         (dist-sq (max 1 (vec-dot dx dx)))
         (m1 (particle-mass p1))
         (m2 (particle-mass p2))
         (impulse (/ (* (+ 1 RESTITUTION) m2 (vec-dot dv dx))
                     (* (+ m1 m2) dist-sq))))
    (vec- (particle-v p1) (vec* impulse dx))))

(define (resolve-collisions p others)
  (foldl (lambda (other vel)
           (if (eq? p other) vel
               (let ((dist (vec-mag (vec- (particle-c p) (particle-c other)))))
                 (if (< dist COLLISION-DIST)
                     (elastic-collision
                      (struct-copy particle p [v vel])
                      other)
                     vel))))
         (particle-v p) others))

(define (wrap x lo hi)
  (cond [(< x lo) hi]
        [(> x hi) lo]
        [else x]))

(define (sim-size s)
  (car (car s)))

(define (clamp-velocity vel)
  (let ((spd (vec-mag vel)))
    (if (> spd MAX-SPEED)
        (vec* (/ MAX-SPEED spd) vel)
        vel)))

(define (update-particle p all-particles size)
  (let* ((acc (total-accel p all-particles))
         (new-vel (clamp-velocity (vec+ (particle-v p) (vec* DT acc))))
         (post-col-vel (clamp-velocity
                        (resolve-collisions
                         (struct-copy particle p [v new-vel])
                         all-particles)))
         (new-pos (vec+ (particle-c p) (vec* DT post-col-vel)))
         (w (vector-ref size 0))
         (h (vector-ref size 1))
         (wrapped-pos (vector (wrap (vector-ref new-pos 0) 0 w)
                              (wrap (vector-ref new-pos 1) 0 h))))
    (particle wrapped-pos post-col-vel (particle-mass p))))

(define (update s)
  (if paused
      s
      (let ((ps (particles s))
            (size (sim-size s)))
        (list (car s) (map (lambda (p) (update-particle p ps size)) ps)))))

(define (check-key s key)
  (cond [(key=? key "space")
         (begin (set! paused (not paused)) s)]
        [else s]))
;; END: AI-generated code


(define (simulate world)
  (set! background (rectangle (vector-ref (caar world) 0) (vector-ref (caar world) 1) "solid" "white"))
  (big-bang world
    (to-draw draw-state)
    (on-tick update)
    (on-key check-key))
  (void)) ; prevent displaying world at end

(provide particle simulate)