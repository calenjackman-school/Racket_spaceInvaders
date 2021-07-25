;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname SpaceInvaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Play a game of Space invaders

;; Contsants

(define actual_tank_img (overlay/align "middle" "bottom"
               (rectangle 40 5 "solid" "light brown")
               (rectangle 30 15 "solid" "medium brown")
               (rectangle 5 25 "solid" "black")))
(define actual_alien_img (overlay/align/offset "middle" "middle"
                      (ellipse 40 15 "solid" "blue")
                      0 -5
                      (circle 10 "outline" "blue")))
(define tank_img (scale 0.5 actual_tank_img))
(define alien_img (scale 0.5 actual_alien_img))
  
(define tank_img_w (image-width tank_img))
(define tank_img_h (image-height tank_img))
(define alien_img_w (image-width alien_img))
(define alien_img_h (image-height alien_img))
(define laser_img_w (* tank_img_w 0.25))
(define laser_img_h tank_img_h)

(define laser_img (ellipse laser_img_w tank_img_h "solid" "red"))

(define board_w 300)
(define board_h 600)

(define MTS_w (+ board_w tank_img_w))
(define MTS_h (+ board_h (/ alien_img_h 2) (/ tank_img_h 2)))

(define MTS (empty-scene MTS_w MTS_h))

(define tank_speed (/ 6 board_w))
(define laser_speed (/ 8 board_w))
(define alien_speed_x (/ 2 board_w))
(define alien_speed_y (/ 2 board_h))

(define alien_chance 1000)
(define prob_of_alien 0.005)



;; ===================================== Data =====================================

;; -------------- tank --------------

(define-struct tank (x y direc))
;; tank is (make-tank Number[0, 1] Number[0, 1] Boolean)
;; a tank that is at position (x, y) realtive to the board variables

(define tank_1 (make-tank 0.5 0.5 false))

(define (fn_for_tank t)
  (... (tank-x t)
       (tank-y t)
       (tank-direc t)))
;; Template rules used:
;; - compound: 3 fields



;; -------------- laser and ListOfLaser --------------

(define-struct laser (x y collide))
;; laser is (make-laser Number[0, 1] Number[0, 1] Boolean)
;; a laser that is at position (x, y) realtive to the board variables with res being true if it has hit an alien

(define laser_1 (make-laser 0.5 0.5 false))

(define (fn_for_laser l)
  (... (laser-x l)
       (laser-y l)
       (laser-collide l)))
;; Template rules used:
;; - compound: 3 fields



;; ListOfLaser is one of:
;; - empty
;; - (cons laser ListOfLaser)
;; a list of lasers

(define LOL-1 empty)
(define LOL-2 (cons (make-laser 0 0 false) empty))
(define LOL-3 (cons (make-laser 0 0 false) (cons (make-laser 0.5 0.5 false) empty)))

(define (fn_for_LOL lol)
  (cond [(empty? lol) (...)]
        [else (... (fn_for_laser (first lol))
                   (fn_for_LOL (rest lol)))]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct: empty
;; - compound: (cons Laser ListOfLaser)
;; - reference: (first lol) is Laser
;; - self-reference: (rest lol) is ListOfLaser



;; -------------- alien and ListOfAlien --------------

(define-struct alien (x y collide direc))
;; alien is (make-alien Number[0, 1] Number[0, 1] Boolean Boolean)
;; an alien that is at position (x, y) realtive to the board variables with collide being true if the alien has been hit by a laser and direc being t(r)ue if the alien is moving to the right

(define alien_1 (make-alien 0.5 0.5 false true))

(define (fn_for_alien a)
  (... (alien-x a)
       (alien-y a)
       (alien-collide a)
       (alien-direc a)))
;; Template rules used:
;; - compound: 4 fields



;; ListOfAlien is one of:
;; - empty
;; - (cons alien ListOfAlien)
;; a list of aliens

(define LOA-1 empty)
(define LOA-2 (cons (make-alien 0 0 false true)
                    empty))
(define LOA-3 (cons (make-alien 0 0 false true)
                    (cons (make-alien 0.5 0.5 false true)
                          empty)))

(define (fn_for_LOA loa)
  (cond [(empty? loa) (...)]
        [else (... (fn_for_alien (first loa))
                   (fn_for_LOA (rest loa)))]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct: empty
;; - compound: (cons alien ListOfAlien)
;; - refernce: (first loa) is alien
;; - self-reference: (rest loa) is ListOfAlien



;; -------------- Space Invaders (SI) world --------------

(define-struct SI (tank lol loa))
;; SI is (make-SI tank ListOfLaser ListOfAlien)
;; Space Invaders game with current positions of tank, lasers, and aliens

(define SI-1 (make-SI (make-tank 0.5 0 true)
                      (cons (make-laser 0.3 0.7 false) empty)
                      (cons (make-alien 0.7 0.3 false true) empty)))

(define (fn_for_SI game)
  (... (fn_for_tank (SI-tank game))
       (fn_for_LOL (SI-lol game))
       (fn_for_LOA (SI-loa game))))
;; Template rules used:
;; - compound: 3 fields
;; - reference: tank is tank
;; - reference: lol is ListOfLaser
;; - reference: loa is ListOfAlien



;; ===================================== For Tests =====================================

(define SI_base (make-SI (make-tank 0.5 1 true)
                         (cons (make-laser 0.252 0.217 false)
                               empty)
                         (cons (make-alien 0.077 0.08 false false)
                               empty)))



;; ===================================== Functions =====================================

;; -------------- main() --------------

(define (main si)
  (big-bang (make-SI (make-tank 0.5 1 false) empty empty)
    (on-tick tock) ; SI >> SI
    (to-draw render) ; SI >> Image
    (on-key response) ; SI KeyEvent >> SI
    (stop-when End_Of_Game End_Screen))) ; SI >> Boolean



;; -------------- tock --------------

;; SI >> SI
;; move existing pieces, check for laser and alien intersections, and generate new alien piece

(check-expect (tock SI_base) (make-SI (move_tank (SI-tank SI_base))
                                      (move_ListOfLaser (check_for_laser_hits (SI-lol SI_base) (move_ListOfAlien (SI-loa SI_base))))
                                      (move_ListOfAlien (create_alien (check_for_alien_hits (move_ListOfLaser (SI-lol SI_base))
                                                                                            (SI-loa SI_base))
                                                                      1))))
;(define (tock si) si) ; stub

(define (tock game)
  (make-SI (move_tank (SI-tank game))
           (move_ListOfLaser (check_for_laser_hits (SI-lol game) (move_ListOfAlien (SI-loa game))))
           (move_ListOfAlien (create_alien (check_for_alien_hits (move_ListOfLaser (SI-lol game))
                                                                 (SI-loa game))
                                           (/ (random (+ alien_chance 1)) alien_chance)))))



;; -------------- move_tank --------------

;; Tank >> Tank
;; move the tank piece in the current direction, if tank is on the edge of the screen flip the direction

(check-expect (move_tank (make-tank 0.5 1 true))
              (make-tank (+ 0.5 tank_speed) 1 true))
(check-expect (move_tank (make-tank 1 1 true))
              (make-tank 1 1 false))
(check-expect (move_tank (make-tank 0.5 1 false))
              (make-tank (- 0.5 tank_speed) 1 false))
(check-expect (move_tank (make-tank 0 1 false))
              (make-tank 0 1 true))
;(define (move_tank t) t) ; stub

(define (move_tank t)
  (cond [(false? (tank-direc t))
         (if (<= (- (tank-x t) tank_speed) 0)
             (make-tank (tank-x t) (tank-y t) (not (tank-direc t)))
             (make-tank (- (tank-x t) tank_speed) (tank-y t) (tank-direc t)))]
        [else (if (>= (+ (tank-x t) tank_speed) 1)
                  (make-tank (tank-x t) (tank-y t) (not (tank-direc t)))
                  (make-tank (+ (tank-x t) tank_speed) (tank-y t) (tank-direc t)))]))



;; -------------- coord_x --------------

;; Number[0,1] >> Number
;; to find the x coordinate on the board for a given decimal value displacement
(define (coord_x n)
  (+ (/ tank_img_w 2) (* n board_w)))



;; -------------- coord_y --------------

;; Number[0,1] >> Number
;; to find the y coordinate on the board for a given decimal value displacement
(define (coord_y n)
  (+ (/ alien_img_h 2) (* n board_h)))



;; -------------- move_ListOfLaser --------------

;; ListOfLaser >> ListOfLaser
;; move a list of laser pieces to their next position
(check-expect (move_ListOfLaser (list (make-laser 0.4 1 false)
                                      (make-laser 0.5 1 false)))
              (list (make-laser 0.4 (- 1 laser_speed) false)
                    (make-laser 0.5 (- 1 laser_speed) false)))
(check-expect (move_ListOfLaser (list (make-laser 0.4 0.3 true)
                                      (make-laser 0.5 0.5 false)))
              (list (make-laser 0.5 (- 0.5 laser_speed) false)))
;(define (move_ListOfLaser lol) lol) ; stub

(define (move_ListOfLaser lol)
  (cond [(empty? lol) empty]
        [else (remove-all empty (remove_lasers (cons (move_laser (first lol))
                                                     (move_ListOfLaser (rest lol)))))]))



;; -------------- move_laser (HELPER >> move_ListOfLaser) --------------

;; Laser >> Laser
;; move laser to next postion on the board, if laser is at the top of the board change to true
(check-expect (move_laser (make-laser 0.4 0.9 false))
              (make-laser 0.4 (- 0.9 laser_speed) false))
(check-expect (move_laser (make-laser 0.4 (+ 0 laser_speed) false))
              (make-laser 0.4 0 false))
(check-expect (move_laser (make-laser 0.4 0 false))
              (make-laser 0.4 0 true))
(check-expect (move_laser (make-laser 0.4 0.6 true))
              (make-laser 0.4 0.6 true))
;(define (move_laser l) l) ; stub

(define (move_laser l)
  (if (false? (laser-collide l))
      (if (< (- (laser-y l) laser_speed) 0)
          (make-laser (laser-x l) 0 true)
          (make-laser (laser-x l) (- (laser-y l) laser_speed) (laser-collide l)))
      l))



;; -------------- remove_lasers (HELPER >> move_ListOfLaser) --------------

;; ListOfLaser >> ListOfLaser
;; go through list and convert any lasers with (= laser-collide true) to empty
(check-expect (remove_lasers empty) empty)
(check-expect (remove_lasers (list (make-laser 0.4 0.9 false)
                                   (make-laser 0.4 0.6 true)))
              (list (make-laser 0.4 0.9 false)
                    empty))
(check-expect (remove_lasers (list (make-laser 0.4 0.9 false)
                                   (make-laser 0.4 0.6 false)))
              (list (make-laser 0.4 0.9 false)
                    (make-laser 0.4 0.6 false)))
;(define (remove_laser lol) lol) ; stub

(define (remove_lasers lol)
  (cond [(empty? lol) empty]
        [else (cons (if (false? (laser-collide (first lol))) (first lol) empty)
                    (remove_lasers (rest lol)))]))



;; -------------- check_for_laser_hits --------------

;; ListOfLaser ListOfAlien >> ListOfLaser
;; mark lasers that will hit alien as so
(check-expect (check_for_laser_hits (list (make-laser 0 0 false)
                                          (make-laser 1 0 false))
                                    (list (make-alien 0 0 false false)
                                          (make-alien 1 0 false false)))
              (list (make-laser 0 0 true)
                    (make-laser 1 0 true)))
(check-expect (check_for_laser_hits (list (make-laser 0.1 1 false)
                                          (make-laser 1 (+ 0.6 laser_speed) false))
                                    (list (make-alien 0 0 false false)
                                          (make-alien 1 0.6 false false)))
              (list (make-laser 0.1 1 false)
                    (make-laser 1 (+ 0.6 laser_speed) true)))
;(define (check_for_laser_hits lol loa) lol) ; stub

(define (check_for_laser_hits lol loa)
  (cond [(empty? lol) empty]
        [else (cons (laser_hit (first lol) loa)
                    (check_for_laser_hits (rest lol) loa))]))



;; -------------- laser_hit (HELPER >> check_for_laser_hits) --------------

;; Laser ListOfAlien >> Laser
;; determine if the laser has hit one of aliens in the list
(check-expect (laser_hit (make-laser 0 (+ 0.1 laser_speed) false)
                         (list (make-alien 0 0.1 false true)
                               (make-alien 0.5 0.5 false true)))
              (make-laser 0 (+ 0.1 laser_speed) true))
;(define (laser_hit l loa) l) ; stub

(define (laser_hit l loa)
  (cond [(empty? loa) l]
        [(laser_intersect? (move_laser l) (first loa)) (make-laser (laser-x l) (laser-y l) true)]
        [else (laser_hit l (rest loa))]))



;; -------------- laser_intersect? (HELPER >> laser_hit and alien_hit) --------------

;; laser alien >> Boolean
;; determine if the coordinates of the laser instersect with the alien
(check-expect (laser_intersect? (make-laser 0 0.1 false) (make-alien 0 0.1 false true)) true)
(check-expect (laser_intersect? (make-laser 1 0.1 false) (make-alien 0 0.1 false true)) false)
;(define (laser_intersect? l a) true) ; stub

(define (laser_intersect? l a)
  (and (and (< (coord_x (laser-x l)) (+ (coord_x (alien-x a)) (/ alien_img_w 2)))
            (> (coord_x (laser-x l)) (- (coord_x (alien-x a)) (/ alien_img_w 2))))
       (and (< (coord_y (laser-y l)) (+ (coord_y (alien-y a)) (/ alien_img_h 2)))
            (> (coord_y (laser-y l)) (- (coord_y (alien-y a)) (/ alien_img_h 2))))))



;; -------------- move_ListOfAlien --------------

;; ListOfAlien >> ListOfAlien
;; move the alien pieces to their next position
(check-expect (move_ListOfAlien (list (make-alien 0.5 0.1 false true)
                                      (make-alien 0.4 0.2 false false)))
              (list (make-alien (+ 0.5 alien_speed_x) (+ 0.1 alien_speed_y) false true)
                    (make-alien (- 0.4 alien_speed_x) (+ 0.2 alien_speed_y) false false)))
(check-expect (move_ListOfAlien (list (make-alien (- 1 alien_speed_x) 0.1 false true)
                                      (make-alien (+ 0 alien_speed_x) 0.2 false false)))
              (list (make-alien 1 (+ 0.1 alien_speed_y) false false)
                    (make-alien 0 (+ 0.2 alien_speed_y) false true)))
(check-expect (move_ListOfAlien (list (make-alien 0.5 0.1 true true)
                                      (make-alien 0.4 0.2 false false)
                                      (make-alien 0.5 0.1 false true)))
              (list (make-alien (- 0.4 alien_speed_x) (+ 0.2 alien_speed_y) false false)
                    (make-alien (+ 0.5 alien_speed_x) (+ 0.1 alien_speed_y) false true)))
;(define (move_ListOfAlien loa) loa) ; stub

(define (move_ListOfAlien loa)
  (cond [(empty? loa) empty]
        [else (remove-all empty (remove_aliens (cons (move_alien (first loa))
                                                     (move_ListOfAlien (rest loa)))))]))



;; -------------- move_alien (HELPER >> move_ListOfAlien) --------------

;; alien >> alien
;; move alien to next postion on the board
(check-expect (move_alien (make-alien (+ 0 alien_speed_x) (- 1 alien_speed_y) false false))
              (make-alien 0 1 false true))
(check-expect (move_alien (make-alien (+ 0 alien_speed_x) 0.5 false false))
              (make-alien 0 (+ 0.5 alien_speed_y) false true))
(check-expect (move_alien (make-alien 0.5 0.5 false false))
              (make-alien (- 0.5 alien_speed_x) (+ 0.5 alien_speed_y) false false))
(check-expect (move_alien (make-alien (- 1 alien_speed_x) (- 1 alien_speed_y) false true))
              (make-alien 1 1 false false))
(check-expect (move_alien (make-alien (- 1 alien_speed_x) 0.5 false true))
              (make-alien 1 (+ 0.5 alien_speed_y) false false))
(check-expect (move_alien (make-alien 0.5 0.5 false true))
              (make-alien (+ 0.5 alien_speed_x) (+ 0.5 alien_speed_y) false true))
(check-expect (move_alien (make-alien 0.5 0.5 true true))
              (make-alien 0.5 0.5 true true))
;(define (move_alien a) a) ; stub

(define (move_alien a)
  (if (false? (alien-collide a))
      (if (false? (alien-direc a))
          (if (<= (- (alien-x a) alien_speed_x) 0)
              (if (>= (+ (alien-y a) alien_speed_y) 1)
                  (make-alien 0 1 (alien-collide a) (not (alien-direc a)))
                  (make-alien 0 (+ (alien-y a) alien_speed_y) (alien-collide a) (not (alien-direc a))))
              (make-alien (- (alien-x a) alien_speed_x) (+ (alien-y a) alien_speed_y) (alien-collide a) (alien-direc a)))
          (if (>= (+ (alien-x a) alien_speed_x) 1)
              (if (>= (+ (alien-y a) alien_speed_y) 1)
                  (make-alien 1 1 (alien-collide a) (not (alien-direc a)))
                  (make-alien 1 (+ (alien-y a) alien_speed_y) (alien-collide a) (not (alien-direc a))))
              (make-alien (+ (alien-x a) alien_speed_x) (+ (alien-y a) alien_speed_y) (alien-collide a) (alien-direc a))))
      a))



;; -------------- remove_aliens (HELPER >> move_ListOfAlien) --------------

;; ListOfAlien >> ListOfAlien
;; go through list and convert any aliens with (= alien-collide true) to empty
(check-expect (remove_aliens empty) empty)
(check-expect (remove_aliens (list (make-alien 0.4 0.9 false false)
                                   (make-alien 0.4 0.6 true false)))
              (list (make-alien 0.4 0.9 false false) empty))
(check-expect (remove_aliens (list (make-alien 0.4 0.9 false false)
                                   (make-alien 0.1 0.6 true false)
                                   (make-alien 0.4 0.6 false false)))
              (list (make-alien 0.4 0.9 false false)
                    empty
                    (make-alien 0.4 0.6 false false)))
;(define (remove_aliens loa) loa) ; stub

(define (remove_aliens loa)
  (cond [(empty? loa) empty]
        [else (cons (if (false? (alien-collide (first loa))) (first loa) empty)
                    (remove_aliens (rest loa)))]))



;; -------------- check_for_alien_hits --------------

;; ListOfLaser ListOfAlien >> ListOfAlien
;; mark aliens that will be hit by lasers as so
(check-expect (check_for_alien_hits (list (make-laser 0.5 0 false)
                                          (make-laser 1 0.4 false))
                                    (list (make-alien 0 0 false false)
                                          (make-alien 1 0 false false)))
              (list (make-alien 0 0 false false)
                    (make-alien 1 0 false false)))
(check-expect (check_for_alien_hits (list (make-laser 0 0 false)
                                          (make-laser 1 0 false))
                                    (list (make-alien 0 0 false false)
                                          (make-alien 1 0 false false)))
              (list (make-alien 0 0 true false)
                    (make-alien 1 0 true false)))
;(define (check_for_alien_hits lol loa) loa) ; stub

(define (check_for_alien_hits lol loa)
  (cond [(empty? loa) (create_alien loa (/ (random (+ alien_chance 1)) alien_chance))]
        [else (cons (alien_hit lol (first loa))
                                  (check_for_alien_hits lol (rest loa)))]))



;; -------------- create_alien (HELPER >> check_for_alien_hits) --------------

;; ListOfAlien Number[0,1] >> ListOfAlien
;; generate new alien if probability is low enough
(check-expect (create_alien empty 0.5)
              empty)
(check-expect (create_alien (list (make-alien 0.6 0.1 false true)) 0.5)
              (list (make-alien 0.6 0.1 false true)))
(check-random (create_alien (list (make-alien 0.6 0.1 false true)) (- prob_of_alien (/ prob_of_alien 10)))
              (list (make-alien 0.6 0.1 false true) (make-alien (/ (random 101) 100) 0 false (= (random 2) 0))))
;(define (create_alien loa n) loa) ; stub

(define (create_alien loa n)
  (if (< n prob_of_alien)
      (append loa (list (make-alien (/ (random 101) 100) 0 false (= (random 2) 0))))
      loa))



;; -------------- alien_hit (HELPER >> check_for_alien_hits) --------------

;; ListOfLaser alien >> alien
;; determine if the alien has been hit by one of the lasers in the list
(check-expect (alien_hit (list (make-laser 0 0.1 false) (make-laser 0.5 0.1 false)) (make-alien 0 0.1 false true)) (make-alien 0 0.1 true true))
;(define (alien_hit lol a) a) ; stub

(define (alien_hit lol a)
  (cond [(empty? lol) a]
        [(laser_intersect? (first lol) (move_alien a)) (make-alien (alien-x a) (alien-y a) true (alien-direc a))]
        [else (alien_hit (rest lol) a)]))



;; -------------- render --------------

;; SI >> Image
;; render an image of SI
(check-expect (render (make-SI (make-tank 0.5 1 false)
                               (list (make-laser 0.3 0.4 false))
                               (list (make-alien 0.2 0.1 false true))))
              (place-image tank_img (coord_x 0.5) (coord_y 1)
                           (place-image laser_img (coord_x 0.3) (coord_y 0.4)
                                        (place-image alien_img (coord_x 0.2) (coord_y 0.1) MTS))))
;(define (render si) empty-image) ; stub

(define (render si)
  (render_lol (SI-lol si)
              (render_loa (SI-loa si)
                          (render_tank (SI-tank si)))))



;; -------------- render_tank --------------

;; tank >> img
;; render the tank in the given position
(check-expect (render_tank (make-tank 0.5 1 true))
              (place-image tank_img (coord_x 0.5) (coord_y 1) MTS))
;(define (render_tank t) empty-image) ; stub

(define (render_tank t)
  (place-image tank_img (coord_x (tank-x t))
               (coord_y (tank-y t)) MTS))



;; -------------- render_loa --------------

;; loa img >> img
;; render the ListOfAlien for the given postions
(check-expect (render_loa (list (make-alien 0.5 0 false false) (make-alien 0.4 0.3 false false)) MTS)
              (place-image alien_img (coord_x 0.5) (coord_y 0) (place-image alien_img (coord_x 0.4) (coord_y 0.3) MTS)))
;(define (render_loa loa img) empty-image) ; stub

(define (render_loa loa img)
  (cond [(empty? loa) img]
        [else (place-image alien_img (coord_x (alien-x (first loa))) (coord_y (alien-y (first loa)))
                           (render_loa (rest loa) img))]))



;; -------------- render_lol --------------

;; lol img >> img
;; render the ListOfLaser for the given postions
(check-expect (render_lol (list (make-laser 0.5 0 false) (make-laser 0.4 0.3 false)) MTS)
              (place-image laser_img (coord_x 0.5) (coord_y 0) (place-image laser_img (coord_x 0.4) (coord_y 0.3) MTS)))
;(define (render_lol lol img) empty-image) ; stub

(define (render_lol lol img)
  (cond [(empty? lol) img]
        [else (place-image laser_img (coord_x (laser-x (first lol))) (coord_y (laser-y (first lol)))
                           (render_lol (rest lol) img))]))



;; -------------- response --------------

;; SI KeyEvent >> SI
;; respond to a KeyEvent based on the current state SI
(check-expect (response (make-SI (make-tank 0.5 1 false)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        " ")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false) (make-laser 0.5 1 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (response (make-SI (make-tank 0.5 1 false)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        "left")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (response (make-SI (make-tank 0.5 1 true)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        "left")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (response (make-SI (make-tank 0.5 1 true)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        "right")
              (make-SI (make-tank 0.5 1 true)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (response (make-SI (make-tank 0.5 1 false)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        "right")
              (make-SI (make-tank 0.5 1 true)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (response (make-SI (make-tank 0.5 1 false)
                                 (list (make-laser 0.3 0.4 false))
                                 (list (make-alien 0.2 0.1 false true)))
                        "a")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
;(define (response si ke) si) ; stub

(define (response si ke)
  (cond [(key=? ke " ") (create_laser si)]
        [(or (key=? ke "right") (key=? ke "left")) (si_flip_tank_direc si ke)]
        [else si]))



;; -------------- create_laser (HELPER >> response) --------------

;; SI >> SI
;; append the current list of lasers with an additional one at the x location of the tank and at the bottom of the board
(check-expect (create_laser (make-SI (make-tank 0.5 1 false)
                                     (list (make-laser 0.3 0.4 false))
                                     (list (make-alien 0.2 0.1 false true))))
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false) (make-laser 0.5 1 false))
                       (list (make-alien 0.2 0.1 false true))))
;(define create_laser si) si) ; stub

(define (create_laser si)
  (make-SI (SI-tank si)
           (append (SI-lol si) (list (make-laser (tank-x (SI-tank si)) (tank-y (SI-tank si)) false)))
           (SI-loa si)))



;; -------------- si_flip_tank_direc (HELPER >> response) --------------

;; SI ke >> SI
;; append the current list of lasers with an additional one at the x location of the tank and at the bottom of the board
(check-expect (si_flip_tank_direc (make-SI (make-tank 0.5 1 false)
                                           (list (make-laser 0.3 0.4 false))
                                           (list (make-alien 0.2 0.1 false true)))
                                  "left")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (si_flip_tank_direc (make-SI (make-tank 0.5 1 true)
                                           (list (make-laser 0.3 0.4 false))
                                           (list (make-alien 0.2 0.1 false true)))
                                  "left")
              (make-SI (make-tank 0.5 1 false)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (si_flip_tank_direc (make-SI (make-tank 0.5 1 true)
                                           (list (make-laser 0.3 0.4 false))
                                           (list (make-alien 0.2 0.1 false true)))
                                  "right")
              (make-SI (make-tank 0.5 1 true)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
(check-expect (si_flip_tank_direc (make-SI (make-tank 0.5 1 false)
                                           (list (make-laser 0.3 0.4 false))
                                           (list (make-alien 0.2 0.1 false true)))
                                  "right")
              (make-SI (make-tank 0.5 1 true)
                       (list (make-laser 0.3 0.4 false))
                       (list (make-alien 0.2 0.1 false true))))
;(define (si_flip_tank_direc si ke) si) ; stub

(define (si_flip_tank_direc si ke)
  (if (tank-direc (SI-tank si))
      (if (key=? ke "right")
          si
          (make-SI (make-tank (tank-x (SI-tank si)) (tank-y (SI-tank si)) (not (tank-direc (SI-tank si))))
                   (SI-lol si)
                   (SI-loa si)))
      (if (key=? ke "left")
          si
          (make-SI (make-tank (tank-x (SI-tank si)) (tank-y (SI-tank si)) (not (tank-direc (SI-tank si))))
                   (SI-lol si)
                   (SI-loa si)))))



;; -------------- End_Of_Game --------------

;; SI >> Boolean
;; end the game if alien has gotten to the bottom of the board
(check-expect (End_Of_Game (make-SI (make-tank 0.5 1 true)
                                    (list (make-laser 0.3 0.4 false))
                                    (list (make-alien 0.2 1 false true))))
              true)
;(define (End_Of_Game si) false) ; stub

(define (End_Of_Game si)
  (alien_invasion? (SI-loa si)))



;; -------------- alien_invasion? (HELPER >> End_Of_Game) --------------

;; ListOfAlien >> Boolean
;; return true if there is an alien with y coordinate of 1
(check-expect (alien_invasion? (list (make-alien 0.5 0 false false) (make-alien 0.8 0.5 false false)))
              false)
(check-expect (alien_invasion? (list (make-alien 0.5 1 false false) (make-alien 0.8 0.5 false false)))
              true)
;(define (alien_invasion? loa) false) ; stub

(define (alien_invasion? loa)
  (cond [(empty? loa) false]
        [else (or (= (alien-y (first loa)) 1)
                  (alien_invasion? (rest loa)))]))



;; -------------- End_Screen --------------

;; SI >> Image
;; display last render of the game with "GAME OVER" overlayed on the board
(check-expect (End_Screen (make-SI (make-tank 0.5 1 true)
                                   (list (make-laser 0.3 0.4 false))
                                   (list (make-alien 0.2 1 false true))))
              (place-image (text "GAME-OVER" 45 "black") (coord_x 0.5) (coord_y 0.5)
                           (render (make-SI (make-tank 0.5 1 true)
                                            (list (make-laser 0.3 0.4 false))
                                            (list (make-alien 0.2 1 false true))))))
;(define (End_Screen si) empty-image) ; stub

(define (End_Screen si)
  (place-image (text "GAME-OVER" 45 "black") (coord_x 0.5) (coord_y 0.5) (render si)))

(main 0)