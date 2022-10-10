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

;; genes por orden (equipo numeroJugador tipoJugador velocidad fuerza habilidad posX posY numGen)

;; createGoalKeeper: creates the goalkeeper
(define (createGoalKeeper team)
  (append (append (append (append (append (append (append (append (list team) '(1)) (list 'keeper)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) '(1))
)
;; createDefenders: creates the defenders
(define (createDefenders team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) '(1)) (list 'defender)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) (list (+ num 1))) (createDefenders team (- num 1))))
  ))
;; createMidFielders: creates the midfielders
(define (createMidFielders team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) '(1)) (list 'mid)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) (list (+ num 1))) (createMidFielders team (- num 1))))
  ))
;; createForwards: creates the forwards
(define (createForwards team num)
 (cond ((equal? num 0) '())
       (else
        (cons (append (append (append (append (append (append (append (append (list team) '(1)) (list 'forward)) (randomValue)) (randomValue)) (randomValue)) '(50)) '(20)) (list (+ num 1))) (createForwards team (- num 1))))
  ))

(selection-aux '(4 4 2) 'CR)
(selection-aux '(5 4 1) 'SPA)
(selection-aux '(3 4 3) 'ENG)