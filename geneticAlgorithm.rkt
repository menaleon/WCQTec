#lang racket/gui

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

(define (manageList children counter)
  (cond ((null? children) '())
        ((equal? counter 2) (cons (car(cdar children)) (manageList (cdr children) 1)))
        (else (cons (caar children) (manageList children (+ counter 1))))))

(define (reproduce selectedPlayers children)
  (cond ((or (null? selectedPlayers) (null? (cdr selectedPlayers))) children)
        (else
         (reproduce  (cddr selectedPlayers) (cons (reproduction (car selectedPlayers) (cadr selectedPlayers)) children))))) 

(define (reproduction player1 player2)
  (list (reproduction-aux player1 player2) (reproduction-aux player2 player1)))

(define (reproduction-aux player1 player2)
  (append (list (getPlayerTeam player1)) (list (getPlayerNum player1)) (list (getPlayerType player1))
          (list (combineTwoBits (convertBinary (getPlayerVel player1)) (convertBinary (getPlayerVel player2))))
          (list (combineTwoBits (convertBinary (getPlayerForce player1)) (convertBinary (getPlayerForce player2))))
          (list (combineTwoBits (convertBinary (getPlayerAbility player1)) (convertBinary (getPlayerAbility player2))))
          
          ; miniBinNum1 miniBinNum2 player1Type axis team
          (list (combineBits
                  (cutBinNum (convertBinary_aux (getPlayerPosX player1) 10) (exact-round (/ (countBits (convertBinary_aux (getPlayerPosX player1) 10)) 2)))
                  (cutBinNum (convertBinary_aux (getPlayerPosX player2) 10) (exact-round (/ (countBits (convertBinary_aux (getPlayerPosX player2) 10)) 2)))
                  (getPlayerType player1) 'x (getPlayerTeam player1)))

          (list (combineBits
                  (cutBinNum (convertBinary_aux (getPlayerPosY player1) 10) (exact-round (/ (countBits (convertBinary_aux (getPlayerPosY player1) 10)) 2)))
                  (cutBinNum (convertBinary_aux (getPlayerPosY player2) 10) (exact-round (/ (countBits (convertBinary_aux (getPlayerPosY player2) 10)) 2)))
                  (getPlayerType player1) 'y (getPlayerTeam player1)))

          (list (+ (getPlayerGen player1) 1)))) ;; increases number of generation

(define (countBits binNum)
  (cond ((null? binNum) 0)
        (else (+ 1 (countBits (cdr binNum))))))

(define (cutBinNum binNum bitLimit)
  (cond ((zero? bitLimit) '())
        (else
           (cons (car binNum) (cutBinNum (cdr binNum) (- bitLimit 1))))))

(define (combineBits miniBinNum1 miniBinNum2 player1Type axis team)
    (convertDecimal-aux (checkRange (append miniBinNum2 miniBinNum1) player1Type axis team) 9))

(define (checkRange binPosition playerType axis team)
  (cond ((equal? team 'CR)
         
             (cond ((equal? axis 'x)
                    (cond ((equal? playerType 'keeper) (dischardOverflow binPosition '(0) '(100)))
                          ((equal? playerType 'defender) (dischardOverflow binPosition '(100) '(300)))
                          ((equal? playerType 'mid) (dischardOverflow binPosition '(300) '(600)))
                          ((equal? playerType 'forward) (dischardOverflow binPosition '(600) '(800)))))
                   ; axis y
                   (else (cond ((equal? playerType 'keeper) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'defender) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'mid) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'forward) (dischardOverflow binPosition '(0) '(800)))))))
        
        (else ; Different Team
           (cond ((equal? axis 'x)
                    (cond ((equal? playerType 'keeper) (dischardOverflow binPosition '(880) '(980)))
                          ((equal? playerType 'defender) (dischardOverflow binPosition '(600) '(880)))
                          ((equal? playerType 'mid) (dischardOverflow binPosition '(300) '(600)))
                          ((equal? playerType 'forward) (dischardOverflow binPosition '(100) '(300)))))
                  ; axis y
                  (else (cond ((equal? playerType 'keeper) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'defender) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'mid) (dischardOverflow binPosition '(0) '(800)))
                          ((equal? playerType 'forward) (dischardOverflow binPosition '(0) '(800)))))))))

(define (combineTwoBits binaryGenPlayer1 binaryGenPlayer2)
      (convertDecimal (append (list (car binaryGenPlayer1) (cadr binaryGenPlayer1))
                              (list (caddr binaryGenPlayer2) (cadddr binaryGenPlayer2)))))

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

(define (selection team-tree)
  (append (selectionKeeper (getKeeper team-tree))
          (selection-rec (getDefenders team-tree) '())
          (selection-rec (getMids team-tree) '())
          (selection-rec (getForwards team-tree) '())))

(define (selectionKeeper keeper)
  (cond ((aptitude? keeper) (append (list keeper)))))

(define (selection-rec players-type fitPlayers)
  (cond ((null? players-type) fitPlayers)
        ((aptitude? (car players-type)) (selection-rec (cdr players-type) (cons (car players-type) fitPlayers)))
        (else (selection-rec (cdr players-type) fitPlayers))))

;; this creates the team. DELETE OR RENAME LATER
(define (selection-aux estrategy team)
  (createFirstGen (numDefenders? estrategy) (numMidFielders? estrategy) (numForwards? estrategy) team))

(define (mutation players)
  (cond ((null? players) '())
        (else
         (cons (mutationPlayer (car players)) (cdr players))
  )))

;; Functions to make mutation
(define (mutationPlayer player)
  (append (list(getPlayerTeam player))
          (list(getPlayerNum player))
          (list(getPlayerType player))
          (mutateSpecificGen (getPlayerVel player) 0)
          (mutateSpecificGen (getPlayerForce player) 0)
          (mutateSpecificGen (getPlayerAbility player) 0)
          (mutateSpecificGen player 1)
          (mutateSpecificGen player 2)
          (list(getPlayerGen player))))


;; type refers to gen, if type is 0, the max number could be 10, if type is 1 refers to pos x and finally 2 refers to pos in y
(define (mutateSpecificGen numberOfGen type)
  (cond ((zero? type)
                       (list (convertDecimal-aux (mutation-aux (convertBinary_aux numberOfGen 10) (random 4) '(0) '(10)) 9)) )
        (else
         (list (convertDecimal-aux (mutatePos numberOfGen type) 9))
        )))

;; type 1 is pos in x, type 2 is pos in y
(define (mutatePos player type)
  (cond ((equal? type 2)
         (cond ((equal? (getPlayerTeam player) 'CR)
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(200) '(400)))
                      (else
                       (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(0) '(565))
                       )))
               (else ;;change y position other team
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(200) '(400)))
                      (else
                       (mutation-aux (convertBinary_aux (getPlayerPosY player) 10) (random 9) '(0) '(565))
               )))))
        (else
         (cond ((equal? (getPlayerTeam player) 'CR)
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(0) '(100)))
                      ((equal? (getPlayerType player) 'defender) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(100) '(300)))
                      ((equal? (getPlayerType player) 'midfielder) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(300) '(600)))
                      (else
                       (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(600) '(880))
                       )))
               (else
                (cond ((equal? (getPlayerType player) 'keeper) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(880) '(980)))
                      ((equal? (getPlayerType player) 'defender) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(600) '(880)))
                      ((equal? (getPlayerType player) 'midfielder) (mutation-aux (convertBinary_aux (getPlayerPosX player) 10) (random 9) '(300) '(600)))
                      (else
                       (mutation-aux (convertBinary_aux (getPlayerPosX player) 9) (random 10) '(100) '(300))
                       ))
               )))))                          

(define (mutation-aux gen randomBit minValue maxValue)
  (binarySum gen randomBit minValue maxValue))


(define (geneticAlgorithm team-tree)
  (updateTree team-tree (mutation (manageList (reproduce (selection team-tree) '()) 1))))

;; genes por orden (equipo numero tipoJugador velocidad fuerza habilidad posX posY numGen)

(define (createGoalKeeper team)
  (append (list team) '(1) (list 'keeper) (randomValue 10) (randomValue 10) (randomValue 10)  (randomPos 0 100)  (randomPos 200 400) '(1)))

(define (createDefenders team num)
 (cond ((equal? num 0) '())
       (else
         (cond ((equal? team 'CR)
                (cons (append (list team) (list (+ num 1)) (list 'defender) (randomValue 10) (randomValue 10) (randomValue 10) (randomPos 100 300) (randomValue 800) '(1))
                      (createDefenders team (- num 1))))
         
         (else (cons (append (list team) (list (+ num 1)) (list 'defender) (randomValue 10) (randomValue 10) (randomValue 10) (randomPos 600 880) (randomValue 800) '(1))
                     (createDefenders team (- num 1))))))))

(define (createMidFielders team num limit)
 (cond ((equal? num 0) '())
       (else
        (cons (append (list team) (list (+ (+ limit 1) num)) (list 'mid) (randomValue 10) (randomValue 10) (randomValue 10) (randomPos 300 600) (randomValue 800) '(1))
              (createMidFielders team (- num 1) limit)))))
  
(define (createForwards team num limit)
 (cond ((equal? num 0) '())
       (else
        (cond ((equal? team 'CR)
               (cons (append (list team) (list (+ (+ limit 1) num)) (list 'forward) (randomValue 10) (randomValue 10) (randomValue 10) (randomPos 600 880) (randomValue 800) '(1))
                     (createForwards team (- num 1) limit)))
              (else (cons (append (list team) (list (+ (+ limit 1) num)) (list 'forward) (randomValue 10) (randomValue 10) (randomValue 10) (randomPos 100 300) (randomValue 800) '(1))
                     (createForwards team (- num 1) limit)) )))))

(define (randomValue max)
  (list (random max)))

(define (randomFloat)
  (/ (random 4294967087) 4294967086.0))

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


(define (indexLista lista numPos)
    (cond ((zero? numPos) (list (car lista)))
          (else
              (indexLista (cdr lista) (- numPos 1))
    )))

(define (dischardOverflow binaryNumber minPos maxPos)
  (cond ((> (convertDecimal-aux binaryNumber 9) (car maxPos)) (convertBinary_aux (car maxPos) 10))
        ((< (convertDecimal-aux binaryNumber 9) (car minPos)) (convertBinary_aux (car minPos) 10))
        (else
         binaryNumber)))

(define (binarySum number bitPos minValue maxValue)
  (cond ((zero? bitPos)
         (cond ((equal? (+ (car (indexLista number '9)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8)
                                   '(0)) (+ bitPos 1) minValue maxValue))
               (else
                (dischardOverflow (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) (indexLista number '7) (indexLista number '8)
                                   '(1))  minValue maxValue)
         )))
        ((equal? bitPos 1)
         (cond ((equal? (+ (car (indexLista number '8)) 1) 2)
                (binarySum (append (indexLista number '0) (indexLista number '1) (indexLista number '2) (indexLista number '3) (indexLista number '4)
                                   (indexLista number '5) (indexLista number '6) (indexLista number '7) '(0) (indexLista number '9))
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

; miniBinNum1 miniBinNum2 player1Type axis team
;(combineBits '(0 1 1 1 0)
;             '(0 1 0 0 0)
;             'forward 'x 'CR)

;(reproduction-aux '(CR 5 forward 3 9 6 50 723 3) '(CR 5 forward 3 9 6 50 715 3))

;(selection-aux '(4 4 2) 'CR)
;;(selection-aux '(5 4 1) 'SPA)
;;(selection-aux '(3 4 3) 'ENG)

;;(getPlayerPosX '(CR 5 forward 3 9 6 50 20 3))
;;(getPlayerGen '(CR 5 forward 3 9 6 50 20 3))

;;(convertBinary_aux '1000 10)
;(convertDecimal-aux '(0 1 1 1 1 1 1 1 1 1) 9)
;(dischardOverflow '(0 0 1 1) '(100) '(300))
;(dischardOverflowPosition '(0 1 1 1 1 1 1 1 1 1) '1000 )

;;(mutation-aux '(0 1 1 1) 0)
;;(binarySum '(0 1 1 1 1 1 0 1 1 1) '8)

(geneticAlgorithm '(CR
  (CR 1 keeper 7 5 7 100 223 1)
  ((CR 5 defender 4 4 7 210 373 1) (CR 4 defender 9 3 3 278 216 1) (CR 3 defender 4 5 7 284 507 1) (CR 2 defender 1 6 3 251 435 1))
  ((CR 9 mid 1 3 3 319 45 1) (CR 8 mid 9 2 9 557 199 1) (CR 7 mid 9 9 3 302 628 1) (CR 6 mid 1 5 1 351 271 1))
  ((CR 11 forward 6 9 6 367 375 1) (CR 10 forward 0 2 9 717 394 1))))

;(manageList (reproduce '((CR 5 defender 4 5 2 50 20 1) (CR 4 defender 7 3 9 50 20 1) (CR 2 defender 4 5 2 50 20 1) (CR 3 defender 7 3 9 50 20 1)) '()) 1)

;(checkRange '(1 1 1 1 0 0 1 1 1 1) 'defender 'x 'CR)

;(selection '(CR
;  (CR 1 keeper 9 1 8 50 20 1)
;  ((CR 5 defender 3 7 8 50 20 1) (CR 4 defender 9 9 9 50 20 1) (CR 3 defender 7 5 9 50 20 1) (CR 2 defender 8 8 9 50 20 1))
 ; ((CR 6 mid 8 9 9 50 20 1) (CR 10 mid 7 7 9 50 20 1) (CR 3 mid 7 5 8 50 20 1) (CR 9 mid 9 5 3 50 20 1))
 ; ((CR 3 forward 9 7 9 50 20 1) (CR 7 forward 0 6 8 50 20 1))))
;(selection-aux '(4 4 2) 'CR)
;(convertDecimal-aux (mutatePos '(CR 1 keeper 3 9 6 50 120 1) 1) 9)

;(mutation '((CR 3 defender 320 320 704 50 20 2) (CR 2 mid 704 320 64 50 20 2)))
;(mutation '(CR 1 keeper 3 9 6 50 120 1))
;(mutation '(CR 2 defender 3 9 6 250 100 1))
;(mutation '(CR 3 midfielder 3 9 6 350 200 1))
;(mutation '(CR 10 forward 3 9 6 640 150 1))
;;(mutateSpecificGen '3 '0)
;;(convertBinary_aux '1023 '10)
;;(convertDecimal-aux '(0 0 0 0 0 0 1 0 1 0) '9)
;;(convertDecimal (mutation-aux '(0 0 0 0 0 0 0 0 1 1) '2 '(0) '(10)))