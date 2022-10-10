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



(define (convertBinary number)
  (cond ((zero? number) '(0))
        ((equal? number 1) '(1))
        (else
         (append (convertBinary (truncate (/ number 2))) (list(remainder number 2)))
)))

(define (convertDecimal binaryNum)
  (convertDecimal-aux binaryNum 3)
  )

(define (convertDecimal-aux binaryNum power)
  (cond ((null? binaryNum) 0)
        ((equal? (car binaryNum) 0) (convertDecimal-aux (cdr binaryNum) (- power 1)))
        (else
         (+ (powerTwo power) (convertDecimal-aux (cdr binaryNum) (- power 1)))
  )))

(define (powerTwo x)
    (cond ((zero? x)  1)
      ( else
          (* 2 (powerTwo (- x 1))))))

(define (dischardOverflow binaryNumber)
  (cond ((> (convertDecimal binaryNumber) 10) '(1 0 1 0))
        (else
         binaryNumber
  )))


(define (mutation)
  (display "Here goes mutation")
  )

(define (mutation-aux gen randomBit)
  (cond ((equal? randomBit 0)
         (binarySum gen randomBit))
  ))

(define (binarySum number bitPos)
  (cond ((zero? bitPos)
         (cond ((equal? (+ (cadddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(0)) (+ bitPos 1)))
               (else
                (dischardOverflow(append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(1)))
         )))
        ((equal? bitPos 1)
         (cond ((equal? (+ (caddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) '(0)) (list(cadddr number))) (+ bitPos 1)))
               (else
                dischardOverflow((append (append (append (list(car number)) (list(cadr number))) '(1)) (list(cadddr number))))))
         )
        ((equal? bitPos 2)
         (cond ((equal? (+ (cadr number) 1) 2) (binarySum (append (append (append (list(car number)) '(0)) (list(caddr number))) (list(cadddr number))) (+ bitPos 1)))
               (else
                (dischardOverflow (append (append (list(car number)) '(1)) (cddr number)))
          )))
        ((equal? (+ (car number) 1) 2) (append '(0) (cdr number)))
              (else
         (dischardOverflow (append '(1) (cdr number))))
  ))

;;(selection-aux '(4 4 2) 'CR)
;;(selection-aux '(5 4 1) 'SPA)
;;(selection-aux '(3 4 3) 'ENG)
;;(getPlayerPosX '(CR 5 forward 3 9 6 50 20 3))
;;(getPlayerGen '(CR 5 forward 3 9 6 50 20 3))
;;(convertBinary '3)
;;(convertDecimal '(0 1 1 1))
;;(dischardOverflow '(1 0 1 1))
;;(mutation-aux '(0 1 1 1) 3)

(binarySum '(1 0 1 0) '0)