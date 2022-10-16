#lang racket/gui

;; createFirstGen: creates the first generation for each team according to a given estrategy
(define (createFirstGen numDefenders numMidFielders numForwards team)
  (append (list team)
          (list (createGoalKeeper team))
          (list (createDefenders team numDefenders))
          (list (createMidFielders team numMidFielders numDefenders))
          (list (createForwards team numForwards (+ numMidFielders numDefenders)))))

(define (aptitude?  player)
  (cond ((equal? (getPlayerType player) 'keeper) (aptitude-goalKeeper player))
        ((equal? (getPlayerType player) 'defender) (aptitude-defender player))
        ((equal? (getPlayerType player) 'forward) (aptitude-forward player))
        (else (aptitude-midfielder player))))

(define (aptitude-goalKeeper player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerVel player)) (* 0.3 (getPlayerForce player))) (* 0.1 (getPlayerAbility player))) 6) #t)
        (else #f)))

(define (aptitude-defender player)
    (cond ((>= (+ (+ (* 0.6 (getPlayerForce player)) (* 0.3 (getPlayerVel player))) (* 0.1 (getPlayerAbility player))) 6) #t)
        (else #f)))

(define (aptitude-midfielder player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerVel player)) (* 0.3 (getPlayerAbility player))) (* 0.1 (getPlayerForce player))) 6) #t)
        (else #f)))

(define (aptitude-forward player)
  (cond ((>= (+ (+ (* 0.6 (getPlayerAbility player)) (* 0.3 (getPlayerForce player))) (* 0.1 (getPlayerVel player))) 6) #t)
        (else #f)))

(define (selection team-tree)
  (append (selectionKeeper (getKeeper team-tree))
          (selection-rec (getDefenders team-tree) '())
          (selection-rec (getMids team-tree) '())
          (selection-rec (getForwards team-tree) '())))

(define (selectionKeeper keeper)
  (cond ((aptitude? keeper) (append (list keeper)))))

(define (selection-rec players-type fitPlayers)
  (cond ((null? players-type) fitPlayers)
        (else (selection-rec (cdr players-type) fitPlayers))))

; reproduction makes 2 children (not less, not more)
(define (reproduce selectedPlayers children)
  (cond ((or (null? (car selectedPlayers)) (null? (cdr selectedPlayers))) children)
        (else
            (reproduce  (cddr selectedPlayers) (append (reproduction (car selectedPlayers) (cadr selectedPlayers))))))) 

(define (reproduction player1 player2)
  (append (list (reproduction-aux player1 player2)) (list (reproduction-aux player2 player1))))

(define (reproduction-aux player1 player2)
  (append (list(getPlayerTeam player1)) (list(getPlayerNum player1)) (list(getPlayerType player1))
          (list (combineTwoBits (convertBinary (getPlayerVel player1)) (convertBinary (getPlayerVel player2))))
          (list (combineTwoBits (convertBinary (getPlayerForce player1)) (convertBinary (getPlayerForce player2))))
          (list (combineTwoBits (convertBinary (getPlayerAbility player1)) (convertBinary (getPlayerAbility player2))))
          (list (getPlayerPosX player1))
          (list (getPlayerPosY player1))
          (list (+ (getPlayerGen player1) 1))))

(define (combineTwoBits binaryGenPlayer1 binaryGenPlayer2)
      (convertDecimal (append (list (car binaryGenPlayer1) (cadr binaryGenPlayer1))
                              (list (caddr binaryGenPlayer2) (cadddr binaryGenPlayer2)))))

; update team-tree by checking playerNum. Looks for childrenPlayer num in the tree and updates that in that position. recursive
(define (updateTree team-tree children)
  (cond ((null? children) team-tree)
        ((equal? (getPlayerType (car children)) 'keeper)
            (updateTree (append (list (car team-tree) (car children) (getDefenders team-tree) (getMids team-tree) (getForwards team-tree))) (cdr children)))
        ((equal? (getPlayerType (car children)) 'defender)
            (updateTree (append (list (car team-tree) (getKeeper team-tree) (replacePlayer (getDefenders team-tree) (car children)) (getMids team-tree) (getForwards team-tree))) (cdr children)))
        ((equal? (getPlayerType (car children)) 'mid)
            (updateTree (append (list (car team-tree) (getKeeper team-tree) (getDefenders team-tree) (replacePlayer (getMids team-tree) (car children)) (getForwards team-tree))) (cdr children)))
        (else
            (updateTree (append (list (car team-tree) (getKeeper team-tree) (getDefenders team-tree) (getMids team-tree) (replacePlayer (getForwards team-tree) (car children)))) (cdr children)))))

(define (replacePlayer playersList child)
  (cond ((null? playersList) '())
        ((equal? (getPlayerNum child)(getPlayerNum (car playersList))) (cons child (replacePlayer (cdr playersList) child)))
        (else (cons (car playersList) (replacePlayer (cdr playersList) child)))))

;; Functions to make mutation
(define (mutation player)
(define (mutationPlayer player)
  (append (list(getPlayerTeam player))
          (list(getPlayerNum player))
          (list(getPlayerType player))
          (mutateSpecificGen (getPlayerVel player) 0)
          (mutateSpecificGen (getPlayerForce player) 0)
          (mutateSpecificGen player 2)
          (list(getPlayerGen player))))


  (binarySum gen randomBit))
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(200) '(400)))
                      (else
                       (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(0) '(565))
                      (else
                      ((equal? (getPlayerType player) 'defender) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(100) '(300)))
               (else
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(880) '(980)))
               )))))                          

(define (geneticAlgorithm team-tree)
  (updateTree team-tree (reproduce (selection team-tree) '())))

;; this creates the team. DELETE OR RENAME LATER
(define (selection-aux estrategy team)
  (createFirstGen (numDefenders? estrategy) (numMidFielders? estrategy) (numForwards? estrategy) team))

;; (equipo numero tipoJugador velocidad fuerza habilidad posX posY numGen)
(define (createGoalKeeper team)
  (append (list team) '(1) (list 'keeper) (randomValue) (randomValue) (randomValue)  (randomPos 0 100)  (randomPos 200 400) '(1)))

(define (createDefenders team num)
 (cond ((equal? num 0) '())
       
       (else
         (cond ((equal? team 'CR)
                (cons (append (list team) (list (+ num 1)) (list 'defender) (randomValue) (randomValue) (randomValue) (randomPos 100 300) (randomPos 0 800) '(1))
                      (createDefenders team (- num 1))))
         
         (else (cons (append (list team) (list (+ num 1)) (list 'defender) (randomValue) (randomValue) (randomValue) (randomPos 600 880) (randomPos 0 800) '(1))
                     (createDefenders team (- num 1))))))))

(define (createMidFielders team num limit)
 (cond ((equal? num 0) '())
       (else
        (cons (append (list team) (list (+ (+ limit 1) num)) (list 'mid) (randomValue) (randomValue) (randomValue) (randomPos 300 600) (randomPos 0 800) '(1))
              (createMidFielders team (- num 1) limit)))))

(define (createForwards team num limit)
 (cond ((equal? num 0) '())
       (else
        (cond ((equal? team 'CR)
               (cons (append (list team) (list (+ (+ limit 1) num)) (list 'forward) (randomValue) (randomValue) (randomValue) (randomPos 600 880) (randomPos 0 800) '(1))
                     (createForwards team (- num 1) limit)))
              (else (cons (append (list team) (list (+ (+ limit 1) num)) (list 'forward) (randomValue) (randomValue) (randomValue) (randomPos 100 300) (randomPos 0 800) '(1))
                     (createForwards team (- num 1) limit)) )))))

(define (randomValue)
  (list (random 10)))

(define (randomFloat)
  (/ (random 4294967087) 4294967086.0))
     ;         ((equal? typePlayer 'defender) ())
     ;(cond

(define (randomPos minPos maxPos)
  (list (exact-round (- maxPos (* minPos (randomFloat))))))

;; numDefenders?: gives the number of defenders
(define (numDefenders? estrategy)
  (car estrategy))

;; numMidFielders?: gives the number of midfielders
(define (numMidFielders? estrategy)
  (cadr estrategy))

;; numForwards?: gives the number of forwards
(define (numForwards? estrategy)
  (caddr estrategy))

(define (getKeeper teamPlayers)
  (cadr teamPlayers))

(define (getDefenders teamPlayers)
  (caddr teamPlayers))

(define (getMids teamPlayers)
  (cadddr teamPlayers))

(define (getForwards teamPlayers)
  (car (cddddr teamPlayers)))

;; (equipo numeroJugador tipoJugador velocidad fuerza habilidad posX posY numGen)
(define (getPlayerTeam player)
  (car player))

(define (getPlayerNum player)
  (cadr player))

(define (getPlayerType player)
  (caddr player))

(define (getPlayerVel player)
  (cadddr player))

(define (getPlayerForce player)
  (cadr (cdddr player)))

(define (getPlayerAbility player)
  (caddr (cdddr player)))

(define (getPlayerPosX player)
  (cadddr (cdddr player)))

(define (getPlayerPosY player)
  (car (cddddr (cdddr player))))

(define (getPlayerGen player)
  (cadr (cddddr (cdddr player))))

(define (convertBinary number)
  (convertBinary_aux number 4))
  
;; if number requieres less than four bits adds 0s instead
(define (convertBinary_aux number numBits)
  (cond ((zero? numBits) '())
        ((and (zero? number) (> numBits 0)) (append (convertBinary_aux 0 (- numBits 1)) '(0)))
        ((and (equal? number 1) (> numBits 0)) (append (convertBinary_aux 0 (- numBits 1)) '(1)))
        ((and (zero? number) (equal? number 0)) '(0))
        ((and (equal? number 1)(equal? number 1)) '(1))
        (else
         (append (convertBinary_aux (truncate (/ number 2)) (- numBits 1)) (list(remainder number 2))))))

(define (convertDecimal binaryNum)
  (convertDecimal-aux binaryNum 3))

(define (convertDecimal-aux binaryNum power)
  (cond ((null? binaryNum) 0)
        ((equal? (car binaryNum) 0) (convertDecimal-aux (cdr binaryNum) (- power 1)))
        (else
         (+ (powerTwo power) (convertDecimal-aux (cdr binaryNum) (- power 1))))))

(define (powerTwo x)
    (cond ((zero? x)  1)
      ( else
          (* 2 (powerTwo (- x 1))))))

(define (dischardOverflow binaryNumber)
  (cond ((>= (convertDecimal binaryNumber) 10) '(1 0 1 0))
        (else
         binaryNumber)))

(define (binarySum number bitPos)
  (cond ((zero? bitPos)
         (cond ((equal? (+ (cadddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(0)) (+ bitPos 1)))
               (else
                (dischardOverflow(append (append (append (list(car number)) (list(cadr number))) (list(caddr number))) '(1)))
         )))
        ((equal? bitPos 1)
         (cond ((equal? (+ (caddr number) 1) 2) (binarySum (append (append (append (list(car number)) (list(cadr number))) '(0)) (list(cadddr number))) (+ bitPos 1)))
                                   (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) (indexLista number '7) '(1) (indexLista number '9))  minValue maxValue)
                )))
        ((equal? bitPos 2)
         (cond ((equal? (+ (car (indexLista number '7)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) '(0) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) '(1) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 3)
         (cond ((equal? (+ (car (indexLista number '6)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) '(0) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) '(1) (indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 4)
         (cond ((equal? (+ (car (indexLista number '5)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    '(0) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    '(1) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 5)
         (cond ((equal? (+ (car (indexLista number '4)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) '(0)
                                    (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) '(1)
                                    (indexLista number '5) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 6)
         (cond ((equal? (+ (car (indexLista number '3)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) '(0) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) '(1) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 7)
         (cond ((equal? (+ (car (indexLista number '2)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) '(0) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) '(1) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        ((equal? bitPos 8)
         (cond ((equal? (+ (car (indexLista number '1)) 1) 2)
                (binarySum (append (indexLista number '0) '(0) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) '(1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          )))
        (else
         (cond ((equal? (+ (car (indexLista number '0)) 1) 2)
                (binarySum (append '(0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8) (indexLista number '9))
                           (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append '(1) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                    (indexLista number '5) (indexLista number '6)(indexLista number '7) (indexLista number '8) (indexLista number '9)) minValue maxValue
                 )
          ))
         )))




;;(selection-aux '(4 4 2) 'CR)
;;(selection-aux '(5 4 1) 'SPA)
;;(selection-aux '(3 4 3) 'ENG)

;;(getPlayerPosX '(CR 5 forward 3 9 6 50 20 3))
;;(getPlayerGen '(CR 5 forward 3 9 6 50 20 3))

;;(convertBinary_aux '1000 10)
;;(convertDecimal-aux '(0 1 1 1 1 1 1 1 1 1) 9)
;;(dischardOverflow '(1 0 1 1))
;;(dischardOverflowPosition '(0 1 1 1 1 1 1 1 1 1) '1000)

;;(mutation-aux '(0 1 1 1) 0)
;;(binarySum '(0 1 1 1 1 1 0 1 1 1) '8)

;;(display "APTITUDE? ")
;;(aptitude? '(CR 1 keeper 7 4 7 50 20 1))
;;(aptitude? '(CR 5 defender 3 8 6 50 20 1))
;;(aptitude? '(CR 2 mid 5 4 7 50 20 1))
;;(aptitude? '(CR 3 forward 4 4 2 50 20 1))
;;(mutation '(CR 5 forward 5 9 6 50 20 3))

;;(binarySum '(1 0 0 1) '0)

;;(aptitude? '(CR 5 defender 3 8 6 50 20 1))

;(selection '(CR
 ; (CR 1 keeper 9 1 8 50 20 1)
  ;((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
  ;((CR 5 mid 2 2 9 50 20 1) (CR 4 mid 7 1 5 50 20 1) (CR 3 mid 1 2 3 50 20 1) (CR 2 mid 9 5 3 50 20 1))
  ;((CR 3 forward 1 1 2 50 20 1) (CR 2 forward 0 6 8 50 20 1))))

;(updateTree '(CR
;  (CR 1 keeper 9 1 8 50 20 1)
;  ((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
;  ((CR 5 mid 2 2 9 50 20 1) (CR 4 mid 7 1 5 50 20 1) (CR 3 mid 1 2 3 50 20 1) (CR 2 mid 9 5 3 50 20 1))
;  ((CR 3 forward 1 1 2 50 20 1) (CR 2 forward 0 6 8 50 20 1)))
 ;           (reproduce (selection '(CR
;  (CR 1 keeper 9 1 8 50 20 1)
 ; ((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
;  ((CR 5 mid 2 2 9 50 20 1) (CR 4 mid 7 1 5 50 20 1) (CR 3 mid 1 2 3 50 20 1) (CR 2 mid 9 5 3 50 20 1))
;  ((CR 3 forward 1 1 2 50 20 1) (CR 2 forward 0 6 8 50 20 1))))) '())


;(reproduce '((CR 1 keeper 9 1 8 50 20 1) (CR 2 defender 8 8 9 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 mid 9 5 3 50 20 1) (CR 2 forward 0 6 8 50 20 1)) '())

;(updateTree '(CR
;  (CR 1 keeper 9 1 8 50 20 1)
;  ((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
;  ((CR 6 mid 2 2 9 50 20 1) (CR 10 mid 7 1 5 50 20 1) (CR 3 mid 1 2 3 50 20 1) (CR 9 mid 9 5 3 50 20 1))
 ; ((CR 3 forward 1 1 2 50 20 1) (CR 7 forward 0 6 8 50 20 1)))
       ;     '((CR 3 defender 5 5 11 50 20 2) (CR 10 mid 11 5 1 50 20 2)) )

;(replacePlayer '((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
 ;              '(CR 3 defender 5 5 11 50 20 2))

;(reproduction '(CR 5 defender 4 5 2 50 20 1) '(CR 4 defender 7 3 9 50 20 1))

(geneticAlgorithm '(CR
  (CR 1 keeper 9 1 8 50 20 1)
  ((CR 5 defender 3 7 2 50 20 1) (CR 4 defender 4 1 2 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
  ((CR 5 mid 2 2 9 50 20 1) (CR 4 mid 7 1 5 50 20 1) (CR 3 mid 1 2 3 50 20 1) (CR 2 mid 9 5 3 50 20 1))
  ((CR 3 forward 1 1 2 50 20 1) (CR 2 forward 0 6 8 50 20 1))))
(convertDecimal-aux (mutatePos '(CR 1 keeper 3 9 6 50 120 1) 1) 9)

;(mutation '((CR 3 defender 320 320 704 50 20 2) (CR 2 mid 704 320 64 50 20 2)))
;(mutation '(CR 1 keeper 3 9 6 50 120 1))
;(mutation '(CR 2 defender 3 9 6 250 100 1))
;(mutation '(CR 3 midfielder 3 9 6 350 200 1))
;(mutation '(CR 10 forward 3 9 6 640 150 1))
;;(mutateSpecificGen '3 '0)
;;(convertBinary_aux '1023 '10)
;;(convertDecimal-aux '(0 0 0 0 0 0 1 0 1 0) '9)
;;(convertDecimal (mutation-aux '(0 0 0 0 0 0 0 0 1 1) '2 '(0) '(10)))