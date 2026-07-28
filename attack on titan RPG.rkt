#lang racket

;; ==========================================
;; ATTACK ON TITAN: SURVEY CORPS RPG
;; ==========================================


;; ===============================
;; DATA STRUCTURES
;; ===============================

(struct scout (name health gas blades)
  #:transparent)


(struct titan (name health damage)
  #:transparent)



;; ===============================
;; CREATE SCOUT
;; ===============================

(displayln "================================")
(displayln " ATTACK ON TITAN: SURVEY CORPS ")
(displayln "================================")
(newline)


(display "Enter your Scout name: ")
(define player-name (read-line))


(display "Choose your health: ")
(define player-health (read))


(display "Choose your ODM gas: ")
(define player-gas (read))


(display "Choose your blades: ")
(define player-blades (read))


(define player
  (scout player-name
         player-health
         player-gas
         player-blades))



;; ===============================
;; TITANS
;; ===============================


(define pure-titan
  (titan "Pure Titan"
         100
         15))


(define armored-titan
  (titan "Armored Titan"
         200
         25))



;; ===============================
;; DISPLAY
;; ===============================


(define (show-player player)

  (displayln "---------- SCOUT ----------")

  (displayln
   (string-append
    "Name: "
    (scout-name player)))

  (displayln
   (string-append
    "Health: "
    (number->string
     (scout-health player))))

  (displayln
   (string-append
    "Gas: "
    (number->string
     (scout-gas player))))

  (displayln
   (string-append
    "Blades: "
    (number->string
     (scout-blades player))))

  (displayln "--------------------------"))



(define (show-titan titan)

  (displayln "---------- TITAN ----------")

  (displayln
   (string-append
    "Enemy: "
    (titan-name titan)))

  (displayln
   (string-append
    "HP: "
    (number->string
     (titan-health titan))))

  (displayln "--------------------------"))



;; ===============================
;; ATTACKS
;; ===============================


  (define (normal-attack enemy)

  (displayln "You slash the Titan!")

  (titan
   (titan-name enemy)
   (- (titan-health enemy) 30)
   (titan-damage enemy)))



(define (odm-attack player enemy)

  (if (> (scout-gas player) 0)

      (begin

        (displayln "You launch an ODM strike!")

        (list

         (scout
          (scout-name player)
          (scout-health player)
          (- (scout-gas player) 20)
          (scout-blades player))

         (titan
          (titan-name enemy)
          (- (titan-health enemy) 60)
          (titan-damage enemy))))

      (begin

        (displayln "No gas left!")

        (list player enemy))))


(define (titan-attack player enemy)

  (displayln
   (string-append
    (titan-name enemy)
    " attacks you!"))

  (scout
   (scout-name player)
   (- (scout-health player)
      (titan-damage enemy))
   (scout-gas player)
   (scout-blades player)))



;; ===============================
;; GAME LOOP
;; ===============================


(define (battle player enemy)

  (cond

    [(<= (titan-health enemy) 0)

     (displayln "============================")
     (displayln "MISSION COMPLETE!")
     (displayln "Enemy defeated!")
     (displayln "============================")]


    [(<= (scout-health player) 0)

     (displayln "============================")
     (displayln "MISSION FAILED")
     (displayln "You were eaten by the Enemy.")
     (displayln "============================")]


    [else

     (show-player player)
     (show-titan enemy)

     (newline)

     (displayln "Choose your action:")
     (displayln "1. Blade Attack")
     (displayln "2. ODM Strike")

     (display "> ")

     (let ([choice (read)])

       (cond

         [(= choice 1)

          (battle
           (titan-attack player enemy)
           (normal-attack enemy))]


         [(= choice 2)

          (let ([result (odm-attack player enemy)])

            (battle
             (titan-attack
              (first result)
              (second result))

             (second result)))]


         [else

          (displayln "Invalid choice.")

          (battle player enemy)]))]))

;; ===============================
;; START GAME
;; ===============================


(displayln "")
(displayln "A Titan has appeared!")
(displayln "")


(battle player pure-titan)