(require 2htdp/image)
(require 2htdp/universe)

;; Simple one line editor

;; ================
;; Constants

(define WIDTH  300)
(define HEIGHT 40)
(define CTR-Y (/ HEIGHT 2))
(define X 2)

(define FONT-SIZE 24)
(define FONT-COLOR "olive")

(define MTS (empty-scene WIDTH HEIGHT))
(define CURSOR (line 0 90 "red"))

;; ================
;; Data Definitions

(define-struct editor (text cursor))
;; Editor is (make-editor String Natural)
;; interp. The state of editor.
;;         text is text
;;         cursor is the position of cursor relation to text

(define E1 (make-editor "" 0))  ; |
(define E2 (make-editor "klingon" 0)) ; |klignon
(define E3 (make-editor "romulan" 1)) ; r|omulan
(define E4 (make-editor "cardassian" 4)) ; carda|ssian
(define E5 (make-editor "dominion" 7)) ; dominion|

#;
(define (fn-for-editor e)
  (... (editor-text   e)   ; String
       (editor-cursor e))) ; Natural

;; Template rules used:
;; - compound: 2 fields

;; ================
;; Functions

;; Editor -> Editor
;; Start with (main (make-editor "hello" 5))
;; <no tests for main function>
(define (main e)
  (big-bang e
            (to-draw render-editor)
            (on-key  handle-key)))

;; ==============
;; Editor -> Image
;; place text and cursor in right position on MTS
(check-expect (render-editor (make-editor "" 0))
              (place-image/align
               (beside (text "" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "" FONT-SIZE FONT-COLOR))
               X CTR-Y "left" "middle" MTS))

(check-expect (render-editor (make-editor "klignon" 0))
              (place-image/align
               (beside (text "" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "klignon" FONT-SIZE FONT-COLOR))
               X CTR-Y "left" "middle" MTS))

(check-expect (render-editor (make-editor "klignon" 1))
              (place-image/align
               (beside
                (text "k" FONT-SIZE FONT-COLOR)
                CURSOR
                (text "lignon" FONT-SIZE FONT-COLOR))
               X CTR-Y "left" "middle" MTS))

;(define (render-editor e) MTS) ;stub

; took template from Editor
(define (render-editor e)
  (place-image/align
   (beside
    (text-before e)
    CURSOR
    (text-after e))
   X CTR-Y "left" "middle" MTS))


;; ===============
;; Editor -> Image
;; produce text to render before cursor depending on cursor position
(check-expect (text-before (make-editor "hello" 0)) (text ""      FONT-SIZE FONT-COLOR))
(check-expect (text-before (make-editor "hello" 1)) (text "h"     FONT-SIZE FONT-COLOR))
(check-expect (text-before (make-editor "hello" 2)) (text "he"    FONT-SIZE FONT-COLOR))
(check-expect (text-before (make-editor "hello" 5)) (text "hello" FONT-SIZE FONT-COLOR))

;(define (text-before e) (text "" FONT-SIZE FONT-COLOR)) ;stub
; took template from Editor
(define (text-before e)
  (text
   (substring (editor-text e) 0 (editor-cursor e))
   FONT-SIZE FONT-COLOR))


;; ===============
;; Editor -> Image
;; produce text to render after cursor depending on cursor position
(check-expect (text-after (make-editor "hello" 0)) (text "hello" FONT-SIZE FONT-COLOR))
(check-expect (text-after (make-editor "hello" 1)) (text "ello"  FONT-SIZE FONT-COLOR))
(check-expect (text-after (make-editor "hello" 2)) (text "llo"   FONT-SIZE FONT-COLOR))
(check-expect (text-after (make-editor "hello" 5)) (text ""      FONT-SIZE FONT-COLOR))

;(define (text-after e) (text "" FONT-SIZE FONT-COLOR)) ;stub
; took template from Editor
(define (text-after e)
  (text
   (substring (editor-text e) (editor-cursor e))
   FONT-SIZE
   FONT-COLOR))


;; ================
;; Editor KeyEvent -> Editor
;; Move cursor left, right or delete char
(check-expect (handle-key (make-editor "hello" 5) "left")  (make-editor "hello"  4))
(check-expect (handle-key (make-editor "hello" 5) "right") (make-editor "hello"  5))
(check-expect (handle-key (make-editor "hello" 0) "\b")    (make-editor "hello"  0))
(check-expect (handle-key (make-editor "hello" 0) "shift") (make-editor "hello"  0))
(check-expect (handle-key (make-editor "hello" 5) "!")     (make-editor "hello!" 6))

;;(define (handle-key e key) e) ;stub

; took template from Editor
(define (handle-key e key)
  (cond [(string=? "left" key)
         (handle-left e)]
        [(string=? "right" key)
          (handle-right e)]
        [(string=? "\b" key)
          (handle-delete e)]
        [(> (string-length key) 1)
         (make-editor (editor-text e) (editor-cursor e))]
        [(equal? (string-length key) 1)
         (handle-char e key)]
        ))

;; ====================
;; Editor -> Editor
;; Move cursor to left
(check-expect (handle-left (make-editor "hello" 0)) (make-editor "hello" 0)) ; cursor at beginning of the word
(check-expect (handle-left (make-editor "hello" 2)) (make-editor "hello" 1)) ; cursor at middle
(check-expect (handle-left (make-editor "hello" 5)) (make-editor "hello" 4)) ; cursor at the end

;;(define (handle-left e) e) ;stub

(define (handle-left e)
  (if (equal? 0 (editor-cursor e))
      (make-editor (editor-text e) (editor-cursor e))
      (make-editor (editor-text e) (- (editor-cursor e) 1))))

;; ====================
;; Editor -> Editor
;; Move cursor to right
(check-expect (handle-right (make-editor "hello" 0)) (make-editor "hello" 1)) ; cursor at beginning of the word
(check-expect (handle-right (make-editor "hello" 2)) (make-editor "hello" 3)) ; cursor at middle
(check-expect (handle-right (make-editor "hello" 5)) (make-editor "hello" 5)) ; cursor at the end

;; (define (handle-right e) e) ;stub

(define (handle-right e)
  (if (equal? (string-length (editor-text e)) (editor-cursor e))
      (make-editor (editor-text e) (editor-cursor e))
      (make-editor (editor-text e) (+ 1 (editor-cursor e)))))

;; ====================
;; Editor -> Editor
;; Delete characther and move cursor left one char
(check-expect (handle-delete (make-editor "hello" 0)) (make-editor "hello" 0)) ; cursor at beginning of the word
(check-expect (handle-delete (make-editor "hello" 3)) (make-editor "helo"  2)) ; cursor at middle
(check-expect (handle-delete (make-editor "hello" 5)) (make-editor "hell"  4)) ; cursor at the end

;; (define (handle-delete e) e) ;stub

(define (handle-delete e)
  (if (equal? 0 (editor-cursor e))
      (make-editor (editor-text e) (editor-cursor e))
      (make-editor (string-append
                    (substring (editor-text e) 0 (- (editor-cursor e) 1))
                    (substring (editor-text e) (editor-cursor e) (string-length (editor-text e)) ))
                   (- (editor-cursor e) 1))))

;; ====================
;; Key Editor -> Editor
;; Append char to text and move cursor to right one char
(check-expect (handle-char (make-editor "hello" 0) ".") (make-editor ".hello" 1)) ; cursor at beginning of the word
(check-expect (handle-char (make-editor "hello" 3) ".") (make-editor "hel.lo" 4)) ; cursor at middle
(check-expect (handle-char (make-editor "hello" 5) ".") (make-editor "hello." 6)) ; cursor at the end

;; (define (handle-char key e) e)

(define (handle-char e key)
  (if (equal? 0 (editor-cursor e))
      (make-editor (string-append key (editor-text e)) (+ 1 (editor-cursor e)))
      (make-editor (string-append
                    (substring (editor-text e) 0 (editor-cursor e))
                    key
                    (substring (editor-text e) (editor-cursor e) (string-length (editor-text e))))
                   (+ (editor-cursor e) 1))))
