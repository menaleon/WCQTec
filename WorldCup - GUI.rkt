 #lang racket/gui

;; importing genetic Algorithm
(require "geneticAlgorithm.rkt")

;; Important variables for the game
(define generations '0)
(define changeGeneration '0)
(define contador1 '0)
(define contador2 '0)

(define current-ball-holder '(0 0 0))
(define ball-position '(487 292))

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

                        
                         (game-on players_firstTeam players_secondTeam)
                         )]))

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

;; function to repaint the canvas
(define (clean-canvas)
   (sleep 0.004)
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

;; function that call the update menu with 0 seconds
(define (threaded-menu)
  (update-menu 0))

;; moves ball randomly
(define (ball)
  (move_ball (random 980) (random  530) (* (+ (random 10) 3) 500))
  )

;; function to make the simulation
(define (game)
  (cond ((or (zero? generations) (equal? contador1 3) (equal? contador2 3)) (kill-thread (current-thread)))
    ((equal? generations 2) (callGenetic)
                                (sleep 0.5)
                                (movePlayers))
                                
    ((> changeGeneration 5) (callGenetic)
                                (sleep 0.5)
                                (movePlayers)
                                        (set! changeGeneration 0)
                                          (game)
                                        )
               (else
                    (set! changeGeneration (+ changeGeneration 1))
                    (sleep 1)
                      (game)
                    )
               )
  )


;; moves all the player
(define (movePlayers)
  (move_player_aux '1 last_playersAllGens_firstTeam playersAllGens_firstTeam)
  (sleep 0.5)
  (move_player_aux '2 last_playersAllGens_secondTeam playersAllGens_secondTeam)
  ;(newline)
 )

;; stops the players
(define (stop-ball x y)
  (set! ball-position (list x y)))

;; detect collision between player and ball
(define (collision player-x player-y ball-x ball-y)
  (cond ((and (< ball-x player-x) (> (+ ball-x 25) player-x) (or (and (> ball-y player-y) (< ball-y (+ player-y 55))) (and (> (+ ball-y 25) player-y) (< (+ ball-y 25) (+ player-y 55)))))
         #t)
        ((and (< ball-x (+ player-x 20)) (> (+ ball-x 25) (+ player-x 20)) (or (and (> ball-y player-y) (< ball-y (+ player-y 55))) (and (> (+ ball-y 25) player-y) (< (+ ball-y 25) (+ player-y 55)))))
          #t)
        ((and (< ball-y (+ player-y 55)) (> (+ ball-y 25) (+ player-y 55)) (> ball-x player-x) (< ball-x (+ player-x 20)))
          #t)
        (else
          #f)))

;; recursive call to move_player to cut off the player list
(define (move_player_aux type playersPast playersCurrent)
  (cond ((not(null? playersPast))
         (cond ((collision (getPlayerPosX (car playersPast)) (getPlayerPosY (car playersPast)) (car ball-position) (cadr ball-position))
                (stop-ball (getPlayerPosX (car playersPast)) (getPlayerPosY (car playersPast)))))
         (move_player type (getPlayerPosX (car playersPast)) (getPlayerPosY (car playersPast))
                                                (getPlayerPosX (car playersCurrent)) (getPlayerPosY (car playersCurrent))
                                                (getPlayerNum (car playersCurrent)) (getPlayerGen (car playersCurrent))
                                                )
                                  (move_player_aux type (cdr playersPast) (cdr playersCurrent))
                                  )
  ))


;; Moves a player and make the animation
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

;; detects if player is inside of the field
(define (player-in-boundaries x y)
  (cond ((and (>= x 0) (<= x 980) (>= y 0) (<= y 510))
         #t)
        (else
         #f)))

;; detect collision between players and ball
(define (collision-checker players)
  (cond ((null? players)
         #f)
        ((collision (getPlayerPosX (car players)) (getPlayerPosY (car players)) (car ball-position) (cadr ball-position))
         (set! current-ball-holder (list (getPlayerType (car players)) (getPlayerForce (car players)) (getPlayerAbility (car players))))
         #t)
        (else
         (collision-checker (cdr players)))))

;; move the ball
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
        ((collision-checker playersAllGens_firstTeam)
         (shoot (car current-ball-holder) (car current-ball-holder) (car current-ball-holder)))
        ((collision-checker playersAllGens_secondTeam)
         (shoot (car current-ball-holder) (car current-ball-holder) (car current-ball-holder)))
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

;; shoots the ball, according with the force and abilty of a player
(define (shoot player-type force ability)
  (cond ((equal? player-type 1)
         (move_ball 1000 (random (* 20 ability) (- 565 (* 16.5 ability))) (* 30 force)))
        ((equal? player-type 2)
         (move_ball 0 (random (* 20 ability) (- 565 (* 16.5 ability))) (* 30 force)))))

;; detect goal condition
(define (goal? x y team)
  (cond ((and (>= x 975) (and (>= y 200) (<= y 400)) (equal? team 1))
         #t)
        ((and (<= x 0) (and (>= y 200) (<= y 400)) (equal? team 2))
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

;; auxiliar function to call the genetic
(define (callGenetic-aux)
         (set! team_tree_1 (geneticAlgorithm team_tree_1))
        (set! team_tree_2 (geneticAlgorithm team_tree_2))
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


;; set the values of players with all gens
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

;; set values of players with their positions only
(define (setPlayersTeam numberTeam listOfPlayers)
  (cond ((zero? numberTeam) (set! players_firstTeam listOfPlayers))
        (else
         (set! players_secondTeam listOfPlayers)
         )))


;; getChars: get characteristics for position
(define (getChars player)
  (append (list (getPlayerPosX player)) (list (getPlayerPosY player)) (list (getPlayerNum player)) (list (getPlayerGen player)))
         )
;; get chars for each player
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

;; MAIN FUNCTION THAT STARTS THE GAME
;; Llamar a esta función mediante la consola, o bien descomentar la línea 468
(define (QaTec estrategy_1 estrategy_2 numberOfGenerations)
  (set! generations numberOfGenerations)
  (set! team_tree_1 (createFirstGen-aux estrategy_1 'CR))
  (set! team_tree_2 (createFirstGen-aux estrategy_2 'ESP))
  (saveCurrentValues)
  (send frame show #t)
  (thread threaded-menu)
  (thread game)
  (thread ball)
  )

; (QaTec '(4 4 2) '(3 4 3) '20)

