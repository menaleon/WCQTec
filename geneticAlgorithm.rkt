#lang racket/gui

;; MAIN GENETIC FUNCTIONS----------------------------------------------------------------------------------

;; createFirstGen: creates the first generation for each team according to a given estrategy
;; numDefenders: number of defenders of the estrategy
;; numMidFielders: number of midfielders of the strategy
;; numForwards: number of forwards of the strategy
;; team: number or name of the team
(define (createFirstGen numDefenders numMidFielders numForwards team)
  (append (list team) (append (append (append (list(createGoalKeeper team)) (list (createDefenders team numDefenders))) (list (createMidFielders team numMidFielders)))
                              (list (createForwards team numForwards)) ) )
)

;; Aptitude (fitness) functions
(define (aptitude?  player)
  (cond ((equal? (getPlayerType player) 'keeper)(aptitude-goalKeeper player))
        ((equal? (getPlayerType player) 'defender) (aptitude-defender player))
        ((equal? (getPlayerType player) 'forward) (aptitude-forward player))
  ))

(define (aptitude-goalKeeper player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerVel player)) (* 0.3 (getPlayerForce player))) (* 0.1 (getPlayerAbility player))) 6) #t)
        (else #f)
  ))

(define (aptitude-defender player)
    (cond ((>= (+ (+ (* 0.6 (getPlayerForce player)) (* 0.3 (getPlayerVel player))) (* 0.1 (getPlayerAbility player))) 6) #t)
        (else #f)
  ))

(define (aptitude-midfielder player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerVel player)) (* 0.3 (getPlayerAbility player))) (* 0.1 (getPlayerForce player))) 6) #t)
        (else #f)
  ))

(define (aptitude-forward player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerAbility player)) (* 0.3 (getPlayerForce player))) (* 0.1 (getPlayerVel player))) 6) #t)
        (else #f)
  ))




;; reproduction of player1 with player2
;;1.obtain each gen of each player
;;2.combine 2 bits of genPlayer1 with genPlayer2 -- for each gene = 1.convert to binary,
;;  take first or last 2 bits depending on childNumber and combine them with the bits in
;; the complementary order from the genPlayer2
;;3.return 2 new players in a list (newPlayer1 newPlayer2)
(define (reproduction player1 player2)
  (append (reproduction-aux player1 player2 1) (reproduction-aux player1 player2 2))
  )

(define (reproduction-aux player1 player2 childNumber)
  (append (append (append (append (append (append (append (append (list(getPlayerTeam player1))
                                                                  (list(getPlayerNum player1)))
                                                          (list(getPlayerType player1)))
                                          (list (combineTwoBits (convertBinary (getPlayerVel player1)) (convertBinary (getPlayerVel player2)) childNumber)))
                                  (list (combineTwoBits (convertBinary (getPlayerForce player1)) (convertBinary (getPlayerForce player2)) childNumber)))
                          (list (combineTwoBits (convertBinary (getPlayerAbility player1)) (convertBinary (getPlayerAbility player2)) childNumber)) )
                  (list(getPlayerPosX player1)) )
           (list(getPlayerPosY player1)))
   (list(+ (getPlayerGen player1) 1)))
  )

(define (combineTwoBits binaryGenPlayer1 binaryGenPlayer2 childNumber)
  (cond ((equal? childNumber 1)
         (convertDecimal (append (list (car binaryGenPlayer1) (cadr binaryGenPlayer1)) (list (caddr binaryGenPlayer2) (cadddr binaryGenPlayer2)))))
        
        (else
         (convertDecimal (append (list (car binaryGenPlayer2) (cadr binaryGenPlayer2)) (list (caddr binaryGenPlayer1) (cadddr binaryGenPlayer1)))))
   ))


;; implementation of main iterative function for the algorithm may be located in GUI file
(define (selection keeper defenders midfielders forwards)
  (append (append (append (list (selection-rec keeper)) (list (selection-rec defenders)))
                  (list (selection-rec midfielders)))
          (list (selection-rec forwards)))
  )


;; erase the disfunctional players from the corresponding list
(define (selection-rec listPlayers)
  (cond ((null? listPlayers) '())
        ((aptitude? (car listPlayers)) (cons (car listPlayers) (selection-rec (cdr listPlayers))))
        (else (selection-rec (cdr listPlayers)))
        )
  )


;; implementation of main iterative function for the algorithm may be located in GUI file
(define (selection-aux estrategy team)
  (createFirstGen (numDefenders? estrategy) (numMidFielders? estrategy) (numForwards? estrategy) team)
  )


;; Functions to make mutation
(define (mutation player)
  (append (append (append (append (append (append (append (append (list(getPlayerTeam player)) (list(getPlayerNum player))) (list(getPlayerType player))) (mutateSpecificGen (getPlayerVel player)))
          (mutateSpecificGen (getPlayerForce player))) (mutateSpecificGen (getPlayerAbility player)) ) (list(getPlayerPosX player)) ) (list(getPlayerPosY player))) (list(getPlayerGen player)))
  )

(define (mutateSpecificGen numberOfGen)
  (list (convertDecimal (mutation-aux (convertBinary numberOfGen) (random 4))))  )


(define (mutation-aux gen randomBit)
       (binarySum gen randomBit)
  )



;; CREATE LIST OF A SPECIFIC TYPE OF PLAYERS---------------------------------------------------------------------

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

;; asignVelocityGen: random
(define (randomValue)
  (list (random 10))
  )

;; OBTAIN HOW MANY PLAYERS OF A SPECIFIC TYPE-----------------------------------------------------

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

;; OBTAIN PLAYERS OF A SPECIFIC TYPE, FROM THE TREE------------------------------------------------

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

;; OBTAIN GENES OF A SINGLE PLAYER------------------------------------------------------------------

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


;; DEALING WITH BINARY ARITHMETIC (for reproduction and mutation)------------------------------------------

(define (convertBinary number)
  (convertBinary_aux number 4)
)
;; if number requieres less than four bits adds 0s instead
(define (convertBinary_aux number numBits)
  (cond ((zero? numBits) '())
        ((and (zero? number) (> numBits 0)) (append (convertBinary_aux 0 (- numBits 1)) '(0)))
        ((and (equal? number 1) (> numBits 0)) (append (convertBinary_aux 0 (- numBits 1)) '(1)))
        ((and (zero? number) (equal? number 0)) '(0))
        ((and (equal? number 1)(equal? number 1)) '(1))
        (else
         (append (convertBinary_aux (truncate (/ number 2)) (- numBits 1)) (list(remainder number 2)))
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
  (cond ((>= (convertDecimal binaryNumber) 10) '(1 0 1 0))
        (else
         binaryNumber
  )))

(define (binarySum number bitPos)
  (cond ((zero? bitPos)
         (cond ((equal? (+ (cadddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(0)) (+ bitPos 1)))
               (else
                (dischardOverflow(append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(1)))
         )))
        ((equal? bitPos 1)
         (cond ((equal? (+ (caddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) '(0)) (list(cadddr number))) (+ bitPos 1)))
               (else
                (dischardOverflow(append (append (append (list(car number)) (list(cadr number))) '(1)) (list(cadddr number))))))
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
;;(mutation-aux '(0 1 1 1) 0)

;;(binarySum '(1 0 1 0) '1)

;;(display "APTITUDE? ")
;;(aptitude? '(CR 1 keeper 7 4 7 50 20 1))
;;(aptitude? '(CR 5 defender 3 8 6 50 20 1))
;;(aptitude? '(CR 2 mid 5 4 7 50 20 1))
;;(aptitude? '(CR 3 forward 4 4 2 50 20 1))

;;(mutation '(CR 5 forward 5 9 6 50 20 3))

;;(binarySum '(1 0 0 1) '0)

(reproduction '(CR 5 defender 4 5 2 50 20 1) '(CR 4 defender 7 3 9 50 20 1))