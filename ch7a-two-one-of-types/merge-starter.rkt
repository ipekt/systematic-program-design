;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; merge-starter.rkt

;Problem:
;
;Design the function merge. It consumes two lists of numbers, which it assumes are 
;each sorted in ascending order. It produces a single list of all the numbers, 
;also sorted in ascending order. 
;
;Your solution should explicitly show the cross product of type comments table, 
;filled in with the values in each case. Your final function should have a cond 
;with 3 cases. You can do this simplification using the cross product table by 
;recognizing that there are subtly equal answers. 
;
;Hint: Think carefully about the values of both lists. You might see a way to 
;change a cell content so that 2 cells have the same value.

; ===================================

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                     lstb
;;                           empty           (cons Integer LOS)                
;;                                         |
;; l   empty                 lsta          | lstb
;; s                         --------------------------------
;; t   (cons Integer LOS)    lsta          | (asc (append lsta lstb))
;; a                                       |  

(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge (list 3 6 13) (list 1 2 4)) (list 1 2 3 4 6 13))

(define (merge lsta lstb)
  (cond [(empty? lstb) lsta]
        [(empty? lsta) lstb]
        [else (if (< (first lsta) (first lstb))
                  (cons (first lsta) (merge (rest lsta) lstb))
                  (cons (first lstb) (merge (rest lstb) lsta)))]))


