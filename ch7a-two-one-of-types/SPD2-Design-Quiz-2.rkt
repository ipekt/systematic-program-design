;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname SPD2-Design-Quiz-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; SPD2-Design-Quiz-2.rkt

;
;Problem 1:
;
;Suppose you have rosters for players on two opposing tennis team, and each
;roster is ordered by team rank, with the best player listed first. When both 
;teams play, the best players of each team play one another,
;and the second-best players play one another, and so on down the line. When
;one team has more players than the other, the lowest ranking players on
;the larger team do not play.
;
;Design a function that consumes two rosters, and produces true if all players 
;on both teams will play if the teams play each other. 
;No marks will be given to solution that do not use a cross product table. 


;; Player is String
;; interp.  the name of a tennis player.
(define P0 "Maria")
(define P2 "Serena")

#;
(define (fn-for-player p)
  (... p))



;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp.  a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))



(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))



;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))


;; Produce true if all players on both teams will play if the teams play each other. 
;; Roster Roster -> Boolean
(check-expect (all-play empty empty) true)
(check-expect (all-play R1 empty) false)
(check-expect (all-play empty R1) false)
(check-expect (all-play R2 R1) false)
(check-expect (all-play R1 (list "p1" "p2" "p3" "p4")) true)

;(define (all-play r1 r2) true) ;stub

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                     rosb
;;                           empty           (cons Player Roster)                
;;                                         |
;; r   empty                     t         |     f
;; o                         --------------------------------
;; s   (cons Player Roster)      f         | (all-play rest)  
;; a                                       |

(define (all-play r1 r2)
  (cond [(and (empty? r1) (empty? r2)) true]
        [(or (empty? r1) (empty? r2)) false]
        [else (all-play (rest r1) (rest r2))]))

;Problem 2:
;
;Now write a function that, given two teams, produces the list of tennis matches
;that will be played. 
;
;Assume that this function will only be called if the function you designed above
;produces true. In other words, you can assume the two teams have the same number
;of players. 


;;Roster Roster -> ListOfMatch
(check-expect (matches empty empty) empty)

(check-expect (matches (list "Mary") (list "Gabrial"))
              (list (make-match "Mary" "Gabrial")))

(check-expect (matches (list "Mary" "Susan") (list "Gabrial" "Stacy"))
              (list (make-match "Mary" "Gabrial")
                    (make-match "Susan" "Stacy")))

(check-expect (matches (list "Mary" "Susan" "Elanor") (list "Gabrial" "Stacy" "Belle"))
              (list (make-match "Mary" "Gabrial")
                    (make-match "Susan" "Stacy")
                    (make-match "Elanor" "Belle")))

;(define (matches r1 r2) empty) ; stub

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                     rosb
;;                           empty           (cons Player Roster)                
;;                                         |
;; r   empty                     empty     |     -
;; o                         --------------------------------
;; s   (cons Player Roster)      -         | (matches rest)  
;; a                                       |


(define (matches r1 r2)
  (cond [(or (empty? r1) (empty? r2)) empty]
        [else 
         (append (list (make-match (first r1) (first r2)))
                 (matches (rest r1) (rest r2)))]))

