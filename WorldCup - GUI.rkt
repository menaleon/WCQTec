#lang racket/gui

;; Ventana principal
(define frame
  (new frame%
       [label "World Cup QaTec"]
       [width 1000]
       [height 700]))

;; Campo de juego
(define field
  (new canvas%
       [parent frame]
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
                         (send dc draw-line 970 200 970 400) ;; Marco de gol.
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

(define (draw-player x y)
  (send (send field get-dc) set-brush "pink" 'solid)
  (send (send field get-dc) draw-rectangle x y 20 15)
  (send (send field get-dc) set-brush "blue" 'solid)
  (send (send field get-dc) draw-rectangle x (+ y 15) 20 30)
  (send (send field get-dc) set-brush "black" 'solid)
  (send (send field get-dc) draw-rectangle x (+ y 45) 20 10)
  (sleep 0.005)
  (send (send field get-dc) erase)
  (send field on-paint))

(define (draw-ball x y)
  (send (send field get-dc) set-brush "white" 'solid)
  (send (send field get-dc) set-pen "black" 2 'solid)
  (send (send field get-dc) draw-ellipse 20 20 150 150))

(define (anim)
  (cond ((>= posx 980)
         (draw-player posx posy))
        (else
         (draw-player posx posy)
         (anim (+ posx 1) posy))))

(define (render seconds)
  (send (send field get-dc) draw-text (~a seconds) 495 600)
  (sleep 1))

(define (update seconds)
  (cond ((zero? seconds)
         (render seconds))
        (else
         (render seconds)
         (update (- seconds 1)))))

(define (func-timer)
  (update 90))

(send frame show #t)
(thread func-timer)
(thread anim)