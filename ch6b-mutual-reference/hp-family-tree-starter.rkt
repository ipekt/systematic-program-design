;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-starter) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt
;
;In this problem set you will represent information about descendant family 
;trees from Harry Potter and design functions that operate on those trees.
;
;To make your task much easier we suggest two things:
;  - you only need a DESCENDANT family tree
;  - read through this entire problem set carefully to see what information 
;    the functions below are going to need. Design your data definitions to
;    only represent that information.
;  - you can find all the information you need by looking at the individual 
;    character pages like the one we point you to for Arthur Weasley.

;
;PROBLEM 1:
;
;Design a data definition that represents a family tree from the Harry Potter 
;wiki, which contains all necessary information for the other problems.  You 
;will use this data definition throughout the rest of the homework.

;; =====================================

;; Data definitions:

(define-struct des (name patronus wand kids))
;; Descendant is (make-des String String ListOfMaterials ListOfDescendants)
;; interp. A descendant in a family tree with name, patronus, wand wood an kids

;; ListOfDescendants is one of:
;;   - empty
;;   - (const Descendant ListOfDescendants)
;;  interp. a list of descendents in a family tree

(define LILY (make-des "Lily" "" "" empty))
(define MOLLY (make-des "Molly" "" "" empty))
(define ALBUS (make-des "Albus" "" "" empty))
(define JAMES (make-des "James" "" "" empty))
(define GINNY (make-des "Ginny" "Horse" "Yew" (list LILY ALBUS JAMES)))
(define CHARLIE (make-des "Charlie" "" "Ash" empty))
(define BILL (make-des "Bill" "" "" (list (make-des "Victoire" "" "" empty))))
(define PERCY (make-des "Percy" "" "" (list MOLLY)))
(define GEORGE (make-des "George" "" "" empty))
(define FRED (make-des "Fred" "" "Dogwood" empty))
(define RON (make-des "Ron" "Jack Russell Terrier" "Willow" empty))


#;
(define (fn-for-descendent d)
  (... (des-name d)      ;String
       (des-patronus d)  ;String
       (des-wand d)      ;String
       (fn-for-lod (des-kids d))))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-descendent (first lod))
              (fn-for-lod (rest lod)))]))

;; =====================================

;PROBLEM 2: 
;
;Define a constant named ARTHUR that represents the descendant family tree for 
;Arthur Weasley. You can find all the infomation you need by starting 
;at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;
;You must include all of Arthur's children and these grandchildren: Lily, 
;Victoire, Albus, James.
;
;
;Note that on the Potter wiki you will find a lot of information. But for some 
;people some of the information may be missing. Enter that information with a 
;special value of "" (the empty string) meaning it is not present. Don't forget
;this special value when writing your interp.


(define ARTHUR (make-des "Arthur" "Weasel" "" (list PERCY BILL CHARLIE FRED GEORGE RON GINNY)))


;; =====================================


;PROBLEM 3:
;
;Design a function that produces a pair list (i.e. list of two-element lists)
;of every person in the tree and his or her patronus. For example, assuming 
;that HARRY is a tree representing Harry Potter and that he has no children
;(even though we know he does) the result would be: (list (list "Harry" "Stag")).
;
;You must use ARTHUR as one of your examples.


;; ListOfPatronus is one of:
;;   - empty
;;   - (cons String String)
;;  interp. a list of descendant name and patronus


;; ListOfTwo is one of:
;;   - empty
;;   - (cons ListOfPatronus ListOfTwo)
;;  interp. a list of each descendant in a tree


;; Descendant -> ListOfTwo
;; ListOfDescendants -> ListOfTwo
(check-expect (two-pair--descendant LILY) (list (list "Lily" "")))
(check-expect (two-pair--lod empty) empty)
(check-expect (two-pair--descendant GINNY) (list (list "Ginny" "Horse")
                                                    (list "Lily" "")
                                                    (list "Albus" "")
                                                    (list "James" "")))
(check-expect (two-pair--descendant PERCY) (list (list "Percy" "")
                                                    (list "Molly" "")))


(check-expect (two-pair--descendant ARTHUR)
              (list (list "Arthur" "Weasel")
                    (list "Percy" "")
                    (list "Molly" "")
                    (list "Bill" "")
                    (list "Victoire" "")
                    (list "Charlie" "")
                    (list "Fred" "")
                    (list "George" "")
                    (list "Ron" "Jack Russell Terrier")
                    (list "Ginny" "Horse")
                    (list "Lily" "")
                    (list "Albus" "")
                    (list "James" "")))


;(define (two-pair--descendant d) empty)
;(define (two-pair--lod lod) empty)

(define (two-pair--descendant d)
  (cons (list (des-name d)
              (des-patronus d))
        (two-pair--lod (des-kids d))))

(define (two-pair--lod lod)
  (cond [(empty? lod) empty]
        [else
         (append (two-pair--descendant (first lod))
                 (two-pair--lod (rest lod)))]))

;; =====================================

;PROBLEM 4:
;
;Design a function that produces the names of all descendants of a given person 
;whose wands are made of a given material. 
;
;You must use ARTHUR as one of your examples.


;; Descendant -> ListOfNames
;; ListOfDescendants -> ListOfNames
(check-expect (find-wand--descendant "Cedar" ARTHUR) empty)
(check-expect (find-want--lod "Cedar" (list (make-des "Molly" "" "" empty))) empty)
(check-expect (find-wand--descendant "Ash" BILL) empty)
(check-expect (find-wand--descendant "Willow" ARTHUR) (list "Ron"))

;(define (find-wand--descendant w d) empty)
;(define (find-want--lod w d) empty)



(define (find-wand--descendant w d)
  (if  (string=? w (des-wand d))
       (cons (des-name d)
             (find-want--lod w (des-kids d)))
       (find-want--lod w (des-kids d))))

(define (find-want--lod w lod)
  (cond [(empty? lod) empty]
        [else
         (append (find-wand--descendant w (first lod))
              (find-want--lod w (rest lod)))]))
