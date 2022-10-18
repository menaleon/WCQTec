#lang racket/gui

;; importing genetic Algorithm
(require "geneticAlgorithm.rkt")

;; Important variables for the game
(define generations '0)
(define changeGeneration '0)
(define contador1 '0)
(define contador2 '0)
(define ball-position '(487.5 292.5))

;; Main window
(define frame
  (new frame%
       [label "World Cup QaTec"]
       [width 1000]
       [height 700]
       [stretchable-width #f]	 
       [stretchable-height #f]
      )
  )

;; Field area
(define mainpane
  (new pane%
       [parent frame]
       [min-width 1000]
       [min-height 565]
       ))

;; Menu area
(define menu
  (new pane%
       [parent frame]
       [min-width 1000]
       [min-height 135]))

;; Field
(define field-canvas
  (new canvas%
       [parent mainpane]
       [paint-callback (lambda (canvas dc)
                         (send dc set-brush "green" 'solid)
                         (send dc draw-rectangle 0 0 1000 565) ;; Se dibuja el campo de juego.
                         (send dc set-pen "white" 10 'solid)
                         (send dc set-brush "white" 'transparent)
                         (send dc draw-line 0 10 1000 10) ;; Se dibuja una de las líneas del campo.
                         (send dc draw-line 0 555 1000 555) ;; Se dibuja una de las líneas del campo.
                         (send dc draw-line 10 0 10 600) ;; Se dibuja una de las líneas del campo.
                         (send dc draw-line 975 0 975 600) ;; Se dibuja una de las líneas del campo.
                         (send dc draw-line 500 0 500 600) ;; Se dibuja una de las líneas del campo. 
                         (send dc draw-ellipse 425 225 150 150) ;; Se dibuja el círculo en el centro del campo.
                         (send dc draw-rectangle 0 200 100 200) ;; Zona de portero.
                         (send dc draw-rectangle 880 200 100 200) ;; Zona de portero. (ix, iy, width, height)
                         (send dc set-pen "brown" 10 'solid)
                         (send dc draw-line 10 200 10 400) ;; Marco de gol.
                         (send dc draw-line 970 200 970 400)

                         (draw_ball (car ball-position) (cadr ball-position))

                         
                         (game-on players_firstTeam players_secondTeam))]))

;; Menu
(define menu-canvas
  (new canvas%
       [parent menu]
       [paint-callback (lambda (canvas dc)
                         (send dc set-brush "gray" 'solid)
                         (send dc set-pen "green" 1 'transparent)
                         (send dc draw-rectangle 0 0 1000 700) ;; Menú inferior
                         (send dc set-brush "black" 'solid)
                         (send dc draw-rectangle 455 35 90 40) ;; Contador de tiempo
                         (send dc set-brush "red" 'solid)
                         (send dc draw-rectangle 360 35 90 40) ;; Contador de goles de equipo 1.
                         (send dc draw-rectangle 550 35 90 40) ;; Contador de goles de equipo 2.
                         (send dc set-text-foreground "white")
                         (send dc draw-text (~a contador1) 400 35)
                         (send dc draw-text (~a contador2) 590 35)
                         (send dc set-text-foreground "black")
                         (send dc draw-text "Costa Rica" 380 15)
                         (send dc draw-text "Tiempo" 475 15)
                         (send dc draw-text "España" 570 15))]))

;; Draws the players of each team
(define (game-on players1 players2)
  (draw_players players1)
  (draw_players players2))

;; Cut off the list and call the method to draw the first player 
(define (draw_players lista)
  (cond ((equal? (length lista) 1)  
         (draw_player (caar lista) (cadar lista) (caddar lista) (car (cdddar lista)) (cadr (cdddar lista)) ))
        (else
         (draw_player (caar lista) (cadar lista) (caddar lista) (car (cdddar lista)) (cadr (cdddar lista)))
         (draw_players (cdr lista)))))

;; draw a given player
(define (draw_player plx ply number generation type)
  (send (send field-canvas get-dc) set-pen "brown" 10 'transparent)
  (send (send field-canvas get-dc) set-brush "pink" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx ply 20 15)
  (cond ((equal? type 1)
         (send (send field-canvas get-dc) set-brush "blue" 'solid))
        ((equal? type 2)
         (send (send field-canvas get-dc) set-brush "red" 'solid)))
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 15) 20 30)
  (send (send field-canvas get-dc) set-brush "black" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 45) 20 10)
  (send (send field-canvas get-dc) set-text-foreground "gray")
  (send (send field-canvas get-dc) draw-text (~a number) plx (+ ply 25) 15) ;; number of player
  (send (send field-canvas get-dc) set-text-foreground "white")
  (send (send field-canvas get-dc) draw-text (~a generation) plx (+ ply 12) 15)) ;; generation of player

;; draw the ball
(define (draw_ball ballx bally)
  (send (send field-canvas get-dc) set-brush "white" 'solid)
  (send (send field-canvas get-dc) set-pen "black" 2 'solid)
  (send (send field-canvas get-dc) draw-ellipse ballx bally 25 25))


(define (clean-canvas)
    (sleep 0.005)
  (send (send field-canvas get-dc) erase)
  (send field-canvas on-paint)
  )

;; Render the menu each second
(define (render-menu seconds)
  (send (send menu-canvas get-dc) set-text-foreground "white")
  (send (send menu-canvas get-dc) draw-text (~a seconds) 495 35)
  (sleep 1)
  (send (send menu-canvas get-dc) erase)
  (send menu-canvas on-paint)
  )

;; Call render process each second while stop conditons are false
(define (update-menu seconds)
  (cond ((or (zero? generations) (equal? contador1 3) (equal? contador2 3)) (render-menu seconds))
        (else
         (render-menu seconds)
         (update-menu (+ seconds 1)))))
          
(define (threaded-menu)
  (update-menu 0))


(define (game)
  (cond ((> changeGeneration 5) (callGenetic)
                                (sleep 0.5)
                                (movePlayers)
                                        (set! changeGeneration 0)
                                        )
               (else
                    (set! changeGeneration (+ changeGeneration 1))
                    (sleep 1.5)
                    )
               )
  (game)
  )


;; moves all the player
(define (movePlayers)
  (move_player_aux last_playersAllGens_firstTeam playersAllGens_firstTeam)
  (sleep 0.5)
  (move_player_aux last_playersAllGens_secondTeam playersAllGens_secondTeam)
  (display playersAllGens_secondTeam)
  ;(newline)
  (display last_playersAllGens_secondTeam)
  ;(set! players_firstTeam '((50 50 1 1 1)));; this happened in callGenetic, apparently not
  ;(set! players_secondTeam '((650 50 1 1 1))
  ;(move_player_aux '1 '50 '50 '500 '300 '1 '1));; call this for all players, this makes animation
 )

(define (move_player_aux playersPast playersCurrent)
  (cond ((not(null? playersPast)) (move_player '1 (getPlayerPosX (car playersPast)) (getPlayerPosY (car playersPast))
                                                (getPlayerPosX (car playersCurrent)) (getPlayerPosY (car playersCurrent))
                                                (getPlayerNum (car playersCurrent)) (getPlayerGen (car playersCurrent))
                                                ))
  ))


;; Moves a player
(define (move_player player_type ix iy fx fy  number generation)
  (cond ((and (equal? ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player fx fy  number generation player_type))
        ((and (equal? ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type ix (+ iy 1) fx fy number generation))
        ((and (equal? ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type ix (- iy 1) fx fy number generation))
        ((and (< ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) iy fx fy number generation))
        ((and (> ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) iy fx fy number generation))
        ((and (< ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) (+ iy 1) fx fy number generation))
        ((and (< ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) (- iy 1) fx fy number generation))
        ((and (> ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) (- iy 1) fx fy number generation))
        ((and (> ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy number generation player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) (+ iy 1) fx fy number generation))
        (else
         (draw_player ix iy number generation player_type))))

(define (player-in-boundaries x y)
  (cond ((and (>= x 0) (<= x 980) (>= y 0) (<= y 510))
         #t)
        (else
         #f)))

(define (move_ball fx fy force)
  (cond ((zero? force)
         (set! ball-position (list (car ball-position) (cadr ball-position))))
        ((goal? (car ball-position) (cadr ball-position) 1)
         (set! contador1 (+ contador1 1))
         (set! ball-position (list 487.5 292.5))
         (clean-canvas))
        ((goal? (car ball-position) (cadr ball-position) 2)
         (set! contador2 (+ contador2 1))
         (set! ball-position (list 487.5 292.5))
         (clean-canvas))
        ((and (< (car ball-position) fx) (< (cadr ball-position) fy))
         (cond ((>= (car ball-position) 975)
                (move_ball 0 fy (- force 1)))
               ((>= (cadr ball-position) 540)
                (move_ball fx 0 (- force 1)))
               (else
                (set! ball-position (list (+ (car ball-position) 1) (+ (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball (+ fx 1) (+ fy 1) (- force 1)))))
        ((and (< (car ball-position) fx) (> (cadr ball-position) fy))
         (cond ((>= (car ball-position) 975)
                (move_ball 0 fy (- force 1)))
               ((<= (cadr ball-position) 0)
                (move_ball fx 540 (- force 1)))
               (else
                (set! ball-position (list (+ (car ball-position) 1) (- (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball (+ fx 1) (- fy 1) (- force 1)))))
        ((and (> (car ball-position) fx) (< (cadr ball-position) fy))
         (cond ((<= (car ball-position) 0)
                (move_ball 975 fy (- force 1)))
               ((>= (cadr ball-position) 540)
                (move_ball fx 0 (- force 1)))
               (else
                (set! ball-position (list (- (car ball-position) 1) (+ (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball (- fx 1) (+ fy 1) (- force 1)))))
        ((and (> (car ball-position) fx) (> (cadr ball-position) fy))
         (cond ((<= (car ball-position) 0)
                (move_ball 975 fy (- force 1)))
               ((<= (cadr ball-position) 0)
                (move_ball fx 540 (- force 1)))
               (else
                (set! ball-position (list (- (car ball-position) 1) (- (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball (- fx 1) (- fy 1) (- force 1)))))
        ((and (equal? (car ball-position) fx) (< (cadr ball-position) fy))
         (cond ((>= (cadr ball-position) 540)
                (move_ball fx 0 (- force 1)))
               (else
                (set! ball-position (list (car ball-position) (+ (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball fx ( + fy 1) (- force 1)))))
        ((and (equal? (car ball-position) fx) (> (cadr ball-position) fy))
         (cond ((<= (cadr ball-position) 0)
                (move_ball fx 540 (- force 1)))
               (else
                (set! ball-position (list (car ball-position) (- (cadr ball-position) 1)))
                (clean-canvas)
                (move_ball fx ( - fy 1) (- force 1)))))
        ((and (< (car ball-position) fx) (equal? (cadr ball-position) fy))
         (cond ((>= (car ball-position) 975)
                (move_ball 0 fy (- force 1)))
               (else
                (set! ball-position (list (+ (car ball-position) 1) (cadr ball-position)))
                (clean-canvas)
                (move_ball (+ fx 1) fy (- force 1)))))
        ((and (> (car ball-position) fx) (equal? (cadr ball-position) fy))
         (cond ((<= (car ball-position) 0)
                (move_ball 975 fy (- force 1)))
               (else
                (set! ball-position (list (- (car ball-position) 1) (cadr ball-position)))
                (clean-canvas)
                (move_ball (- fx 1) fy (- force 1)))))))


(define (goal? x y team)
  (cond ((and (>= x 975) (and (>= y 200) (<= y 400)) (equal? team 1))
         #t)
        ((and (<= x 0) (and (>= y 200) (<= y 400)) (equal? team 2))
         #t)
        (else
         #f)))

(define (collision player-x player-y ball-x ball-y)
  (cond ((and (< ball-x player-x) (> (+ (ball-x) 25) player-x) (> ball-y player-y) (< ball-y (+ player-y 55)))
         #t)
        ((and (< ball-x (+ player-x 20)) (> (+ ball-x 25) (+ player-x 20)) (> ball-y player-y) (< ball-y (+ player-y 55)))
         #t)
        ((and (< ball-y (+ player-y 55)) (> (+ ball-y 25) (+ player-y 55)) (> ball-x player-x) (< ball-x (+ player-x 20)))
         #t)
        (else
         #f)))


;; call the genetic algorithm
(define (callGenetic)
  (set! generations (- generations 1))
  (save_lastGenValues)
  (callGenetic-aux)
  (saveCurrentValues)
)

(define (callGenetic-aux)
         (set! team_tree_1 (geneticAlgorithm team_tree_1))
        ; (set! team_tree_2 (geneticAlgorithm team_tree_2))
         (display generations)
         (newline)
  )

;; save last gen values
(define (save_lastGenValues)
         (set! playersLastGen_firstTeam players_firstTeam)
         (set! playersLastGen_secondTeam players_secondTeam)
         (set! last_playersAllGens_firstTeam playersAllGens_firstTeam)
         (set! last_playersAllGens_secondTeam playersAllGens_secondTeam)
  )

;; vars of tree
(define team_tree_1 '())
(define team_tree_2 '())

;; last gen for positions only
(define playersLastGen_firstTeam '())
(define playersLastGen_secondTeam '())

;; last gen all gens
(define last_playersAllGens_firstTeam '())
(define last_playersAllGens_secondTeam '())

;; vars for all gens
(define playersAllGens_firstTeam '())
(define playersAllGens_secondTeam '())

(define (setPlayersTeamAllGens numberTeam team-tree)
  (cond ((zero? numberTeam) (set! playersAllGens_firstTeam (append (cons (getKeeper team-tree) (getDefenders team-tree))
                                                                   (getMids team-tree) (getForwards team-tree) ) ))
        (else
         (set! playersAllGens_secondTeam (append (cons (getKeeper team-tree) (getDefenders team-tree))
                                                                   (getMids team-tree) (getForwards team-tree) ) ))
         )
  )


;; positions for the current players
(define players_firstTeam '())
(define players_secondTeam '())

(define (setPlayersTeam numberTeam listOfPlayers)
  (cond ((zero? numberTeam) (set! players_firstTeam listOfPlayers))
        (else
         (set! players_secondTeam listOfPlayers)
         )))


;; getChars: get characteristics for position
(define (getChars player)
  (append (list (getPlayerPosX player)) (list (getPlayerPosY player)) (list (getPlayerNum player)) (list (getPlayerGen player)))
         )

(define (getIndividual playerList numberTeam)
  (cond ((null? playerList) '())
        (
         (cons (append (getChars (car playerList)) (list numberTeam)) (getIndividual (cdr playerList) numberTeam))
  )))


;; save current values of generation
(define (saveCurrentValues)
  (setPlayersTeamAllGens '0 team_tree_1)
  (setPlayersTeam '0  (getIndividual playersAllGens_firstTeam '1))
  (setPlayersTeamAllGens '1 team_tree_2)
  (setPlayersTeam '1  (getIndividual playersAllGens_secondTeam '2))
  )


;; function that start the game
(define (QaTec estrategy_1 estrategy_2 numberOfGenerations)
  (set! generations numberOfGenerations)
  (set! team_tree_1 (createFirstGen-aux estrategy_1 'CR))
  (set! team_tree_2 (createFirstGen-aux estrategy_2 'ESP))
  (saveCurrentValues)
  (send frame show #t)
  (thread threaded-menu)
  (thread game)
  )

(QaTec '(4 4 2) '(3 4 3) '20)