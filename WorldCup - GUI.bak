#lang racket/gui

(define mainframe
  (new frame%
       [label "World Cup QaTec"]
       [width 1000]
       [height 600]))

(define menu
  (new canvas%
       [parent mainframe]
       [paint-callback (lambda (canvas dc)
                         (send dc set-brush "green" 'solid)
                         (send dc set-pen "blue" 1 'solid)
                         (send dc draw-rectangle 0 0 1000 600)
                         (send dc set-pen "white" 10 'solid)
                         (send dc set-brush "white" 'transparent)
                         (send dc draw-line 0 10 1000 10)
                         (send dc draw-line 0 555 1000 555)
                         (send dc draw-line 10 0 10 600)
                         (send dc draw-line 975 0 975 600)
                         (send dc draw-line 500 0 500 600)
                         (send dc draw-ellipse 425 225 150 150)
                         (send dc draw-rectangle 0 200 100 200)
                         (send dc draw-rectangle 880 200 100 200)
                         (send dc set-pen "brown" 10 'solid)
                         (send dc draw-line 10 200 10 400)
                         (send dc draw-line 970 200 970 400))]))

(send mainframe show #t)
