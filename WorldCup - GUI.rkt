#lang racket/gui

;; Ventana principal
(define frame
  (new frame%
       [label "World Cup QaTec"]
       [width 1000]
       [height 700]))

(define mainpane
  (new pane%
       [parent frame]
       [min-width 1000]
       [min-height 565]))

(define menu
  (new pane%
       [parent frame]
       [min-width 1000]
       [min-height 135]))

;; Campo de juego
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
                         (game-on '((50 50 1) (80 50 1) (50 125 1) (80 125 1) (50 200 1) (80 200 1)) '((930 50 2) (900 50 2) (930 125 2) (900 125 2) (930 200 2) (900 200 2)) '(487.5 270)))]))

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
                         (send dc draw-text "0" 400 35)
                         (send dc draw-text "0" 590 35)
                         (send dc set-text-foreground "black")
                         (send dc draw-text "Equipo1" 380 15)
                         (send dc draw-text "Tiempo" 475 15)
                         (send dc draw-text "Equipo2" 570 15))]))

(define (game-on players1 players2 ball)
  (draw_players players1)
  (draw_players players2)
  (draw_ball (car ball) (cadr ball)))

(define (draw_players lista)
  (cond ((equal? (length lista) 1)
         (draw_player (caar lista) (cadar lista) (caddar lista)))
        (else
         (draw_player (caar lista) (cadar lista) (caddar lista))
         (draw_players (cdr lista)))))

(define (draw_player plx ply type)
  (send (send field-canvas get-dc) set-pen "brown" 10 'transparent)
  (send (send field-canvas get-dc) set-brush "pink" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx ply 20 15)
  (cond ((equal? type 1)
         (send (send field-canvas get-dc) set-brush "blue" 'solid))
        ((equal? type 2)
         (send (send field-canvas get-dc) set-brush "red" 'solid)))
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 15) 20 30)
  (send (send field-canvas get-dc) set-brush "black" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 45) 20 10))


(define (draw_ball ballx bally)
  (send (send field-canvas get-dc) set-brush "white" 'solid)
  (send (send field-canvas get-dc) set-pen "black" 2 'solid)
  (send (send field-canvas get-dc) draw-ellipse ballx bally 25 25))

(define (clean-canvas)
  (sleep 0.002)
  (send (send field-canvas get-dc) erase)
  (send field-canvas on-paint))
         
(define (render-menu seconds)
  (send (send menu-canvas get-dc) set-text-foreground "white")
  (send (send menu-canvas get-dc) draw-text (~a seconds) 495 35)
  (sleep 1)
  (send (send menu-canvas get-dc) erase)
  (send menu-canvas on-paint))

(define (update-menu seconds)
  (cond ((= seconds 300000)
         (render-menu seconds))
        (else
         (render-menu seconds)
         (update-menu (+ seconds 1)))))
          
(define (threaded-menu)
  (update-menu 0))

(define (move_player player_type ix iy fx fy)
  (cond ((and (equal? ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player fx fy player_type))
        ((and (equal? ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type ix (+ iy 1) fx fy))
        ((and (equal? ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type ix (- iy 1) fx fy))
        ((and (< ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) iy fx fy))
        ((and (> ix fx) (equal? iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) iy fx fy))
        ((and (< ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) (+ iy 1) fx fy))
        ((and (< ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (+ ix 1) (- iy 1) fx fy))
        ((and (> ix fx) (> iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) (- iy 1) fx fy))
        ((and (> ix fx) (< iy fy) (player-in-boundaries ix iy))
         (draw_player ix iy player_type)
         (clean-canvas)
         (move_player player_type (- ix 1) (+ iy 1) fx fy))
        (else
         (draw_player ix iy player_type))))

(define (player-in-boundaries x y)
  (cond ((and (>= x 0) (<= x 980) (>= y 0) (<= y 510))
         #t)
        (else
         #f)))

(define (move_ball ix iy fx fy force)
  (cond ((zero? force)
         (draw_ball ix iy))
        ((and (< ix fx) (< iy fy))
         (cond ((equal? ix 975)
                (move_ball ix iy 0 fy (- force 1)))
               ((equal? iy 540)
                (move_ball ix iy fx 0 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (+ ix 1) (+ iy 1) (+ fx 1) (+ fy 1) (- force 1)))))
        ((and (< ix fx) (> iy fy))
         (cond ((equal? ix 975)
                (move_ball ix iy 0 fy (- force 1)))
               ((equal? iy 0)
                (move_ball ix iy fx 540 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (+ ix 1) (- iy 1) (+ fx 1) (- fy 1) (- force 1)))))
        ((and (> ix fx) (< iy fy))
         (cond ((equal? ix 0)
                (move_ball ix iy 975 fy (- force 1)))
               ((equal? iy 540)
                (move_ball ix iy fx 0 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (- ix 1) (+ iy 1) (- fx 1) (+ fy 1) (- force 1)))))
        ((and (> ix fx) (> iy fy))
         (cond ((equal? ix 0)
                (move_ball ix iy 975 fy (- force 1)))
               ((equal? iy 0)
                (move_ball ix iy fx 540 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (- ix 1) (- iy 1) (- fx 1) (- fy 1) (- force 1)))))
        ((and (equal? ix fx) (< iy fy))
         (cond ((equal? iy 540)
                (move_ball ix iy fx 0 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball ix (+ iy 1) fx ( + fy 1) (- force 1)))))
        ((and (equal? ix fx) (> iy fy))
         (cond ((equal? iy 0)
                (move_ball ix iy fx 540 (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball ix (- iy 1) fx ( - fy 1) (- force 1)))))
        ((and (< ix fx) (equal? iy fy))
         (cond ((equal? ix 975)
                (move_ball ix iy 0 fy (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (+ ix 1) iy (+ fx 1) fy (- force 1)))))
        ((and (> ix fx) (equal? iy fy))
         (cond ((equal? ix 0)
                (move_ball ix iy 975 fy (- force 1)))
               (else
                (draw_ball ix iy)
                (clean-canvas)
                (move_ball (- ix 1) iy (- fx 1) fy (- force 1)))))))

(send frame show #t)
(thread threaded-menu) 