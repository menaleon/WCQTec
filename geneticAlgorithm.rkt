#lang racket/gui
;; numDefenders?: gives the number of defenders
(define (numDefenders? estrategy)
  (car estrategy)
  )

;; numMidFielders?: gives the number of midfielders
(define (numMidFielders? estrategy)
  (cadr estrategy)
  )

;; numForwards?: gives the number of forwards
(define (numForwards? estrategy)
  (caddr estrategy)
  )

;; selection-aux:
(define (selection-aux estrategy team)
  (createFirstGen (numDefenders? estrategy) (numMidFielders? estrategy) (numForwards? estrategy) team)
  )

;; createFirstGen: creates the first generation for each team according to a given estrategy
;; numDefenders: number of defenders of the estrategy
;; numMidFielders: number of midfielders of the strategy
;; numForwards: number of forwards of the strategy
;; team: number or name of the team
(define (createFirstGen numDefenders numMidFielders numForwards team)
  (append (list team) (append (append (append (list(createGoalKeeper team)) (list (createDefenders team numDefenders))) (list (createMidFielders team numMidFielders)))
                              (list (createForwards team numForwards)) ))
)


;; asignVelocityGen: random
(define (randomValue)
  (list (random 10))
  )

;; genes por orden (equipo numero tipoJugador velocidad fuerza habilidad posX posY numGen)

;; createGoalKeeper: creates the goalkeeper
(define (createGoalKeeper team)
  (append (append (append (append (append (append (append (append (list team) '(1)) (list 'keeper)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) '(1))
)


;; createDefenders: creates the defenders
(define (createDefenders team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) (list (+ num 1))) (list 'defender)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) '(1)) (createDefenders team (- num 1))))
  ))
;; createMidFielders: creates the midfielders
(define (createMidFielders team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) (list (+ num 1))) (list 'mid)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) '(1)) (createMidFielders team (- num 1))))
  ))
;; createForwards: creates the forwards
(define (createForwards team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) (list (+ num 1))) (list 'forward)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) '(1)) (createForwards team (- num 1)))))
  )



(define (getKeeper teamPlayers)
  (car teamPlayers)
)

(define (getDefenders teamPlayers)
  (cadr teamPlayers)
)

(define (getMids teamPlayers)
  (caddr teamPlayers)
)

(define (getForwards teamPlayers)
  (cadddr teamPlayers)
)

;; genes por orden (equipo numeroJugador tipoJugador velocidad fuerza habilidad posX posY numGen)
(define (getPlayerTeam player)
  (car player)
  )

(define (getPlayerNum player)
  (cadr player)
  )

(define (getPlayerType player)
  (caddr player)
  )

(define (getPlayerVel player)
  (cadddr player)
  )

(define (getPlayerForce player)
  (cadr (cdddr player))
  )

(define (getPlayerAbility player)
  (caddr (cdddr player))
  )

(define (getPlayerPosX player)
  (cadddr (cdddr player))
  )

(define (getPlayerPosY player)
  (car (cddddr (cdddr player)))
  )

(define (getPlayerGen player)
  (cadr (cddddr (cdddr player)))
  )

(selection-aux '(4 4 2) 'CR)
;;(selection-aux '(5 4 1) 'SPA)
;;(selection-aux '(3 4 3) 'ENG)
(getPlayerPosX '(CR 5 forward 3 9 6 50 20 3))
(getPlayerGen '(CR 5 forward 3 9 6 50 20 3))