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
                         (send dc draw-rectangle 880 200 100 200) ;; Zona de portero.
                         (send dc set-pen "brown" 10 'solid)
                         (send dc draw-line 10 200 10 400) ;; Marco de gol.
                         (send dc draw-line 970 200 970 400))]))

(define menu-canvas
  (new canvas%
       [parent menu]
       [paint-callback (lambda (canvas dc)
                         (send dc set-brush "gray" 'solid)
                         (send dc set-pen "green" 1 'transparent)
                         (send dc draw-rectangle 0 565 1000 700) ;; Menú inferior
                         (send dc set-brush "black" 'solid)
                         (send dc draw-rectangle 455 600 90 40) ;; Contador de tiempo
                         (send dc set-brush "red" 'solid)
                         (send dc draw-rectangle 360 600 90 40) ;; Contador de goles de equipo 1.
                         (send dc draw-rectangle 550 600 90 40) ;; Contador de goles de equipo 2.
                         (send dc set-text-foreground "white")
                         (send dc draw-text "0" 400 600)
                         (send dc draw-text "0" 590 600))]))
                       
(define (draw-object plx ply ballx bally)
  (send (send field-canvas get-dc) set-pen "brown" 10 'transparent)
  (send (send field-canvas get-dc) set-brush "pink" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx ply 20 15)
  (send (send field-canvas get-dc) set-brush "blue" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 15) 20 30)
  (send (send field-canvas get-dc) set-brush "black" 'solid)
  (send (send field-canvas get-dc) draw-rectangle plx (+ ply 45) 20 10)
  (send (send field-canvas get-dc) set-brush "white" 'solid)
  (send (send field-canvas get-dc) set-pen "black" 2 'solid)
  (send (send field-canvas get-dc) draw-ellipse ballx bally 25 25)
  (sleep 0.007)
  (send (send field-canvas get-dc) erase)
  (send field-canvas on-paint))
         
(define (render-menu seconds)
  (send (send menu-canvas get-dc) draw-text (~a seconds) 495 600)
  (sleep 1)
  (send (send menu-canvas get-dc) erase)
  (send menu-canvas on-paint))

(define (update-menu seconds)
  (cond ((zero? seconds)
         (render-menu seconds))
        (else
         (render-menu seconds)
         (update-menu (- seconds 1)))))
          
(define (threaded-menu)
  (update-menu 90))

(define (anim)
    (anim_aux 30 30 50 50))

(define (anim_aux plx ply bx by)
  (cond ((>= plx 1000)
         (draw-object plx ply bx by))
        (else
         (draw-object plx ply bx by)
         (anim_aux (+ plx 1) (+ ply 1) (+ bx 1) (+ by 1)))))

(send frame show #t)
(thread threaded-menu)
(thread anim)