;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 50 0 12))
(define I5 (make-invader 100 0 12))
(define I6 (make-invader 200 0 12)) 


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I4 I5 I6) (list M1 M2) T1))



;; Main function

(define (main s)
  (big-bang (make-game empty empty (make-tank (/ WIDTH 2) s))
    (on-key handle-key)
    (on-tick  next-game)
    (stop-when game-over? game-over-text)
    (to-draw  render-game)))


;; Game -> Game
;; update positions of Tank, Missiles and Invaders, remove off-screen Invaders, remove destroyed invaders and rockets

;(define (next-game game) (make-game (empty empty (make-tank 0 1)))) 

(define (next-game g)
  (make-game
   (destroy-invaders (game-missiles g) (create-invaders (next-invaders (game-invaders g))))
   (next-missiles (game-missiles g))
   (next-tank (game-tank g))))

;; ListOfInvaders -> ListOfInvaders
;; if the missile hits an invader, remove it from the game

(define (destroy-invaders lomissile loinvader)
  (cond [(empty? loinvader) empty]
        [(empty? lomissile) loinvader]
        [else
         (if (hit-invader? (first loinvader) lomissile)
             (rest loinvader)
             (cons (first loinvader) (destroy-invaders lomissile (rest loinvader))))]))

;; Invader, ListOfMissile -> Boolean
;; determine if invader got hit by a missile

(define (hit-invader? i lomissile)
  (cond [(empty? lomissile) false]
        [else
         (if (and (<= (- (invader-y i) HIT-RANGE) (missile-y (first lomissile)) (+ (invader-y i) HIT-RANGE)) (<= (- (invader-x i) HIT-RANGE) (missile-x (first lomissile)) (+ (invader-x i) HIT-RANGE)))
             true
             (hit-invader? i (rest lomissile)))]))

;; ListOfInvader -> ListOfInvader
;; add invaders to the game at random X positions and direction according to INVADE-RATE

(define (create-invaders loinvader)
  (cond [(< (random 5000) INVADE-RATE) (cons (make-invader (random WIDTH) 0 (random-direction 2)) loinvader)]
        [else loinvader]))

;; None -> [-1, 1]
;; Generate a random number to determine invader's direction

(define (random-direction n)
  (if (= (modulo (random 11) n) 0) 10 -10))
  
;; ===================INVADERS===================

;; ListOfInvader -> ListOfInvader
;; produce next positions of invaders and remove off-screen landed invaders
(check-expect (next-invaders (list I1)) (list (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))))

;(define (next-invaders loinvader) empty)
(define (next-invaders loinvader)
  (tick-invaders loinvader))

;; ListOfInvader -> ListOfInvader
;; update positions of invaders
               
;(define (tick-invaders loinvaders) empty)

(define (tick-invaders loinvaders)
  (cond [(empty? loinvaders) empty]
        [else
         (cons (tick-invader (first loinvaders))
               (tick-invaders (rest loinvaders)))]))

;; Invader -> Invader
;; update position of invader on screen
(check-expect (tick-invader I1) (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1)))
(check-expect (tick-invader I2) (make-invader (+ (invader-x I2) (invader-dx I2)) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2)))
(check-expect (tick-invader I3) (make-invader (+ (invader-x I3) (invader-dx I3)) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dx I3)))

;(define (tick-invader i) 0)

(define (tick-invader i)
  (cond [(>= (invader-x i) WIDTH) (make-invader (- WIDTH 1) (invader-y i) (- (invader-dx i)))]
        [(<= (invader-x i) 0) (make-invader 1 (invader-y i) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))


;; ===================MISSILES===================

;; ListOfMissile -> ListOfMissile
;; produce next positions of missiles and remove off-screen missed missiles
(check-expect (next-missiles (list (make-missile 150 -10)
                                   (make-missile 130 -200)
                                   (make-missile 200 0)
                                   (make-missile 200 200)
                                   (make-missile 250 50)))
              (list (make-missile 200 190)
                    (make-missile 250 40)))

;(define (next-missiles lomissile) empty)

(define (next-missiles lomissile)
  (on-screen-only-missiles (tick-missiles lomissile)))

;; ListOfMissile -> ListOfMissile
;; update positions of missiles
(check-expect (tick-missiles (list M1 M2 M3)) (list
                                               (make-missile 150 290)
                                               (make-missile 150 100)
                                               (make-missile 150 95)))
               
;(define (tick-missiles lomissile) empty)

(define (tick-missiles lomissile)
  (cond [(empty? lomissile) empty]
        [else
         (cons (tick-missile (first lomissile))
               (tick-missiles (rest lomissile)))]))

;; Missile -> Missile
;; update position of missile on screen
(check-expect (tick-missile M1) (make-missile 150 290))

;(define (tick-missile missile) 0)

(define (tick-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; remove the missiles that have crossed the upper screen boundary
(check-expect (on-screen-only-missiles (list (make-missile 100 -10)
                                             (make-missile 150 -30)
                                             (make-missile 299 150)))
              (list (make-missile 299 150)))

;(define (on-screen-only-missiles lomissile) empty)

(define (on-screen-only-missiles lomissile)
  (cond [(empty? lomissile) empty]
        [else
         (if (onscreen-missile? (first lomissile))
             (cons (first lomissile) (on-screen-only-missiles (rest lomissile)))
             (on-screen-only-missiles (rest lomissile)))]))


;; Missile -> Boolean
;; determine if given missile is positioned on screen
(check-expect (onscreen-missile? (make-missile 100 200)) true)
(check-expect (onscreen-missile? (make-missile 60 50)) true)
(check-expect (onscreen-missile? (make-missile 250 -1)) false)

;(define (onscreen-missile? missile) false)

(define (onscreen-missile? missile)
  (if (>= (missile-y missile) 0) true false))

;; ===================TANK===================

;; Tank -> Tank
;; produce next tank position. If the tank's x coordinate has reached WIDTH or 0 - change tank's direction
(check-expect (next-tank T0) (make-tank (+ TANK-SPEED (tank-x T0)) (tank-dir T0)))

;(define (next-tank tank) T0)

(define (next-tank t)
  (cond [(<= (tank-x t) 0) (make-tank 1 1)]
        [(>= (tank-x t) WIDTH) (make-tank (- WIDTH 1) -1)]
        [else
         (if (= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)) (make-tank (+ (tank-x t) TANK-SPEED) 1))]))


;; ===================RENDERING===================

;; Game -> IMAGE
;; render all game props on screen

;(define (render-game g) MTS)

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; ===RENDER INVADERS===
;; ListOfInvaders -> Image
;; render invaders on MTS

;(define (render-invaders loinvader) MTS)

(define (render-invaders loinvader img)
  (cond [(empty? loinvader) img]
        [else
         (place-image INVADER
                      (invader-x (first loinvader))
                      (invader-y (first loinvader))
                      (render-invaders (rest loinvader) img))]))

;; ===RENDER MISSILES===
;; ListOfMissile -> Image
;; render missiles on MTS

;(define (render-missiles lomissile) MTS)

(define (render-missiles lomissile img)
  (cond [(empty? lomissile) img]
        [else
         (place-image MISSILE
                      (missile-x (first lomissile))
                      (missile-y (first lomissile))
                      (render-missiles (rest lomissile) img))]))

;; ===RENDER TANK===

;; Tank -> Image
;; render tank on appropriate position on MTS

;(define (render-tank tank) MTS)

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) MTS))

;; ===================KEY HANDLERS===================

;; Game KeyEvent -> Game
;; handle key events to fire rockets and change tank's direction

;(define (handle-key s ke) 0)

(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game (game-invaders s) (fire (game-missiles s) (game-tank s)) (game-tank s))]
        [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (turn-tank (game-tank s) "left"))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (turn-tank (game-tank s) "right"))]
        [else s]))


;; Tank, String -> Tank
;; change the direction of the tank depending on which arrow was pressed

(define (turn-tank t s)
  (cond [(string=? s "left") (make-tank (tank-x t) -1)]
        [(string=? s "right") (make-tank (tank-x t) 1)]
        [else t]))

;; ListOfMissile Tank -> ListOfMissile
;; Create a new missile at current position of a tank

(define (fire lomissile t)
  (cond [(empty? lomissile) (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) empty)]
        [else (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) lomissile)]))



;; Game -> Boolean
;; produce true if an invader has landed

(define (game-over? s)
  (landed? (game-invaders s)))

;; ListOfInvader -> Boolean
;; helper to check if any of existing invaders have landed

(define (landed? loinvader)
  (cond [(empty? loinvader) false]
        [else
         (if (>= (invader-y (first loinvader)) HEIGHT)
             true
             (landed? (rest loinvader)))]))

(define (game-over-text s)
  (place-image(text "Game over" 30 "red") (/ WIDTH 2) (/ HEIGHT 2) MTS))


