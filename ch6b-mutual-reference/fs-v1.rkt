;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fs-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; fs-starter.rkt (type comments and examples)
;; fs-v1.rkt (complete data-definition plus function problems)

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))
#;
(define (fn-for-element e)
  (... (elt-name e)    ;String
       (elt-data e)    ;Integer
       (fn-for-loe (elt-subs e))))
#;
(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-element (first loe))
              (fn-for-loe (rest loe)))])) 


;; Functions:

;;================================================
; PROBLEM

; Design a function that consumes Element and produces the sum of all the file data in 
; the tree.
;;================================================

;; Element -> Integer
;; ListOfElement -> Integer
;; produce the sum of all the data in element (and its subs)
(check-expect (sum-data--element F1) 1)
(check-expect (sum-data--loe empty) 0)
(check-expect (sum-data--element D5) 3)
(check-expect (sum-data--element D4) (+ 1 2))
(check-expect (sum-data--element D6) (+ 1 2 3))

;(define (sum-data--element e) 0)
;(deifne (sum-data--loe loe) 0)

(define (sum-data--element e)
  (if (zero? (elt-data e))
      (sum-data--loe (elt-subs e))
      (elt-data e)))

(define (sum-data--loe loe)
  (cond [(empty? loe) 0]
        [else
         (+ (sum-data--element (first loe))
            (sum-data--loe (rest loe)))])) 

;;================================================
; PROBLEM

; Design a function that consumes Element and produces a list of the names of all the elements in 
; the tree.
;;================================================

;; ListOfNames is one of:
;;  - empty
;;  - (cons Element ListOfNames)
;; interp. A list of names in element tree


;; Element -> ListOfNames
;; ListOfElement -> ListOfNames
;; produces list of the names of all the elements in the tree
(check-expect (list-names--element F1) (list "F1"))
(check-expect (list-names--loe empty) empty)
(check-expect (list-names--element D5) (list "D5" "F3"))
(check-expect (list-names--element D4) (list "D4" "F1" "F2"))
(check-expect (list-names--element D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))

;(define (list-names--element e) empty)
;(define (list-names--loe loe) empty)

(define (list-names--element e)
  (if (zero? (elt-data e))
      (append
       (list (elt-name e))
       (list-names--loe (elt-subs e)))
      (list (elt-name e))))

(define (list-names--loe loe)
  (cond [(empty? loe) empty]
        [else
         (append (list-names--element (first loe))
                 (list-names--loe (rest loe)))]))

;;================================================

; PROBLEM

; Design a function that consumes String and Element and produces true if there is an element in 
; the tree with the given name.
;;================================================

;; String Element -> Boolean
;; String ListOfElement -> Boolean
;; produces true if given name is tree
(check-expect (contains--element F1 "F45") false)
(check-expect (contains--loe empty "empty") false)
(check-expect (contains--element D5 "F3") true)
(check-expect (contains--element D4 "D4") true)
(check-expect (contains--element D6 "D7") false)


;(define (contains--element e name) false)
;(define (contains--loeloe name) false)

(define (contains--element e name)
  (cond [(equal? (elt-name e) name) true]
        [(zero? (elt-data e)) (contains--loe (elt-subs e) name)]
        [else false]))

(define (contains--loe loe name)
  (cond [(empty? loe) false]
        [else
         (if (contains--element (first loe) name)
             true
             (contains--loe (rest loe) name))])) 


;;================================================
;
;PROBLEM
;
;Design a function that consumes Element and produces a rendering of the tree. For example: 
;
;(render-tree D6) should produce something like the following.
;
;
;HINTS:
;  - This function is not very different than the first two functions above.
;  - Keep it simple! Start with a not very fancy rendering like the one above.
;    Once that works you can make it more elaborate if you want to.
;  - And... be sure to USE the recipe. Not just follow it, but let it help you.
;    For example, work out a number of examples BEFORE you try to code the function. 
    
;;================================================

(define MTTREE (square 0 "solid" "white"))
(define FONT-SIZE 24)
(define COLOR "black")
(define RECT-SIZE 60)

;; Element -> Image
;; ListOfElements -> Image
;; produces text image of element and lines between all elements in tree

(check-expect (render-tree--element F1) (draw-text "F1"))
(check-expect (render-tree--loe empty 30) MTTREE)
(check-expect (render-tree--element D5) (above
                                         (draw-text "D5")
                                         (lines (image-width (text "D5" FONT-SIZE COLOR)))
                                         (draw-text "F3")))
(check-expect (render-tree--element D4) (above
                                         (draw-text "D4")
                                         (beside
                                          (above
                                           (lines (image-width (text "D4" FONT-SIZE COLOR)))
                                           (draw-text "F1"))
                                          (above
                                           (lines (image-width (text "D4" FONT-SIZE COLOR)))
                                           (draw-text "F2")))))


;(define (render-tree--element e) MTTREE)
;(define (render-tree--loe loe) MTTREE)

(define (render-tree--element e)
  (if (zero? (elt-data e))
      (above
       (draw-text (elt-name e))
       (render-tree--loe
        (elt-subs e)
        (image-width (text (elt-name e) FONT-SIZE COLOR))))
      (draw-text (elt-name e))))


(define (render-tree--loe loe w)
  (cond [(empty? loe) MTTREE]
        [else
         (beside
          (above
           (lines w)
           (render-tree--element (first loe)))
          (render-tree--loe (rest loe) w))])) 

;; Image -> Image
;; Draw line
(check-expect (lines RECT-SIZE)
              (add-line
               MTTREE
               (/ (image-width (draw-text "D5")) 2)
               0
               (/ (image-width (draw-text "D5")) 2)
               24
               "maroon"))

(define (lines w) (add-line MTTREE
                            (/ w 2) 0
                            (/ w 2) 24
                            "maroon"))


;; String -> image
;; Draw text
(check-expect (draw-text "F1")
              (overlay
               (text "F1" FONT-SIZE COLOR)
               (rectangle RECT-SIZE FONT-SIZE "solid" "white")))

(define (draw-text t)
  (overlay
   (text t FONT-SIZE COLOR)
   (rectangle RECT-SIZE FONT-SIZE "solid" "white")))