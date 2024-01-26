;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |HW8 - FINAL COPY|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(require 2htdp/universe)

;; TASK 1 - HW8 

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define (pipe-temp pipe)
  (... (pipe-top pipe) ....
       (pipe-bot pipe) ....
       (pipe-left pipe) ....
       (pipe-right pipe) ....))   

(define PIPE-TL (make-pipe #true #false #true #false))

(define PIPE-TR (make-pipe #true #false #false #true))

(define PIPE-BR (make-pipe #false #true #false #true))

(define PIPE-BL (make-pipe #false #true #true #false))

(define PIPE-TB (make-pipe #true #true #false #false))

(define PIPE-LR (make-pipe #false #false #true #true))

(define PIPE-TBLR (make-pipe #true #true #true #true))

(define STARTING-PIPE-R (make-pipe #false #false #false #true))

(define STARTING-PIPE-L (make-pipe #false #false #true #false))

(define STARTING-PIPE-T (make-pipe #true #false #false #false))

(define STARTING-PIPE-B (make-pipe #false #true #false #false))

(define EMPTY (make-pipe #false #false #false #false))

(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR))

(define TILE-SIDE-LENGTH 60)
(define PIPE-WIDTH 20)

(define-struct pipe-cords [pipe x y filled?])

; A Pipe-Cords is a (make-pipe-cords Pipe PosReal PosReal Boolean) where:
; - pipe represents a pipe
; - x represents a Pipe's x-coordinate
; - y represents a Pipe's y-coordinate
; - filled? represents if the pipe is filled with goo or not

(define PC0 (make-pipe-cords STARTING-PIPE-R 10 20 #t))
(define PC1 (make-pipe-cords PIPE-TBLR 5 10 #f))
(define PC2 (make-pipe-cords PIPE-LR 7 20 #f))
(define PC3 (make-pipe-cords PIPE-BR 40 27 #f))
(define PC4 (make-pipe-cords PIPE-TB 30 20 #f))
(define PC5 (make-pipe-cords STARTING-PIPE-R 5 5 #f))

(define (pipe-c-temp cords)
  (... (pipe-cords-pipe cords) ...
       (pipe-cords-x cords) ...
       (pipe-cords-y cords) ...))



; A ListOfPipeCords (LOPC) is one of:
; - '()
; - (cons PipeCords LOPC)
; A list where each element in the list represents
; the type of pipe and its location (x,y) on the grid and if it is filled or not. 

(define (lopc-temp lopc)
  (...(cond...
    [(empty? lopc)...]
    [(cons? lopc)...
     (first lopc)
     (lopc-temp (rest lop))...])))

(define LOPC-E '())
(define LOPC (list PC5))
(define LOPC1 (list PC5 PC1 PC2 PC3))
(define LOPC2 (list PC0 PC2 PC3 PC4))

;; TASK 3 - HW8 

(define-struct GooFlow [direction lopc])
; A GooFlow is a (make-GooFLow String ListOfPipeCords) where:
; direction is a Direction, which is a string that indicates the direction the goo is currently flowing in
; lopc is a ListOfPipeCords, where each element in the list represents the type of pipe, its location, and if the pipe is filled or not. 

(define (GooFlow-temp gf)
  (... (direction-temp (GooFlow-direction gf)) ...
       (lopc-temp (GooFlow-lopc gf)) ...))

(define GOO-D (make-GooFlow "right" LOPC))
(define GOO2 (make-GooFlow "left" LOPC1))
(define GOO3 (make-GooFlow "up" (list PC5 PC2 PC1)))
(define GOO4 (make-GooFlow "down" (list PC5 PC1 PC3)))

; A Direction is a string, one of:
; - "left"
; - "right"
; - "up"
; - "down"

(define (direction-temp d)
  (...(cond
        [(string=? LEFT d)...]
        [(string=? RIGHT d)...]
        [(string=? TOP d)...]
        [(string=? BOTTOM d)...])...))

(define LEFT "left")
(define RIGHT "right")
(define TOP "top")
(define BOTTOM "bottom")


; direction-helper: GooFlow -> Posn
; Takes in a GooFlow and produces the coordinates at which the next pipe should be at

(check-expect (direction-helper GOO-D) (make-posn 5 6))
(check-expect (direction-helper GOO2) (make-posn 5 4))
(check-expect (direction-helper GOO3) (make-posn 4 5))
(check-expect (direction-helper GOO4) (make-posn 6 5))


(define (direction-helper x)
  (cond [(string=? (GooFlow-direction x) "up") (make-posn (- (pipe-cords-x (first (GooFlow-lopc x))) 1) (pipe-cords-y (first (GooFlow-lopc x))))]
        [(string=? (GooFlow-direction x) "down") (make-posn (+ 1 (pipe-cords-x (first (GooFlow-lopc x)))) (pipe-cords-y (first (GooFlow-lopc x))))]
        [(string=? (GooFlow-direction x) "right") (make-posn (pipe-cords-x (first (GooFlow-lopc x))) (+ 1(pipe-cords-y (first (GooFlow-lopc x)))))]
        [(string=? (GooFlow-direction x) "left") (make-posn (pipe-cords-x (first (GooFlow-lopc x))) (- (pipe-cords-y (first (GooFlow-lopc x))) 1))]))

; ;; TASK 4 - HW8 
 
; grid-goo-propagate : GooFlow Grid -> GooFlow
; moves goo forward by one tile. If goo is stuck, produce the same goo.

(check-expect (grid-goo-propagate GOO-D (make-grid (list (make-pipe-cords PIPE-LR 5 6 #f)) 7)) (make-GooFlow
 "right" (list (make-pipe-cords (make-pipe #false #false #true #true) 5 6 #f) (make-pipe-cords
   (make-pipe #false #false #false #true) 5 5 #f))))

(check-expect (grid-goo-propagate GOO-D (make-grid (list (make-pipe-cords PIPE-TL 5 6 #f)) 7))
              (make-GooFlow "up"
              (list
      (make-pipe-cords (make-pipe #true #false #true #false)
        5 6 #f) (make-pipe-cords (make-pipe
           #false
         #false
          #false #true)
          5 5 #f))))
(check-expect (grid-goo-propagate GOO-D (make-grid (list (make-pipe-cords PIPE-TL 5 6 #f)) 7))
              (make-GooFlow
 "up"
 (list
  (make-pipe-cords
   (make-pipe #true #false #true #false)
   5
   6
   #f)
  (make-pipe-cords
   (make-pipe
    #false
    #false
    #false
    #true)
   5
   5 #f))))

#;(check-expect (grid-goo-propagate GOO-D (make-grid '() 7)) (make-GooFlow "right" (list (make-pipe-cords (make-pipe #false #false #false #true) 5 5 #false))))

                         

#;(define (grid-goo-propagate gf g)
  (cond[(empty? (grid-tile g)) gf]
       [(cons? (grid-tile g))
  (if
   (pipe? (pipe-at g (posn-x (direction-helper gf)) (posn-y (direction-helper gf))))
   (cond
     [(and (pipe-top (pipe-cords-pipe (first (grid-tile g))))
           (pipe-bot (pipe-cords-pipe (first (grid-tile g))))
           (pipe-left (pipe-cords-pipe (first (grid-tile g)))) 
           (pipe-right (pipe-cords-pipe (first (grid-tile g)))))
      (make-GooFlow (GooFlow-direction gf) (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
     [(and (string=? (GooFlow-direction gf) "right")
           (boolean=? (pipe-left (pipe-cords-pipe (first (grid-tile g)))) #t))
      (cond
        [(pipe-top (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "up" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-right (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "right" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-bot (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "down" (cons (first (grid-tile g)) (GooFlow-lopc gf)))])]
     [(and (string=? (GooFlow-direction gf) "left")
           (boolean=? (pipe-right (pipe-cords-pipe (first (grid-tile g)))) #t))
      (cond
        [(pipe-top (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "up" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-left (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "left" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-bot (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "down" (cons (first (grid-tile g)) (GooFlow-lopc gf)))])]
     [(and (string=? (GooFlow-direction gf) "down")
           (boolean=? (pipe-top (pipe-cords-pipe (first (grid-tile g)))) #t))
      (cond
        [(pipe-left (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "left" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-right (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "right" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-bot (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "down" (cons (first (grid-tile g)) (GooFlow-lopc gf)))])]
     [(and (string=? (GooFlow-direction gf) "up")
           (boolean=? (pipe-bot (pipe-cords-pipe (first (grid-tile g)))) #t))
      (cond
        [(pipe-top (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "up" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-right (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "right" (cons (first (grid-tile g)) (GooFlow-lopc gf)))]
        [(pipe-left (pipe-cords-pipe (first (grid-tile g)))) (make-GooFlow "left" (cons (first (grid-tile g)) (GooFlow-lopc gf)))])]
     [else (grid-goo-propagate gf (make-grid (rest (grid-tile g) ) (grid-xy g)))])
   (grid-goo-propagate gf (make-grid (rest (grid-tile g)) (grid-xy g))))]))

;; TASK 2 - HW8 

;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.


(define (pipe->image pipe tile-side-length pipe-width filled? direction)
  (cond
    [(and filled? (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe) (or (string=? direction "right") (string=? direction "left")))
          (overlay/align "middle" "middle" (square tile-side-length "outline" "black") (rotate 90 (rectangle pipe-width tile-side-length "solid" "green")))]
    [(and filled? (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe) (or (string=? direction "top") (string=? direction "bot")))
          (overlay/align "middle" "middle" (square tile-side-length "outline" "black") (rectangle pipe-width tile-side-length "solid" "green"))]

    [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
     (overlay/align "left" "middle" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (overlay/align "middle" "top" (square pipe-width "solid" (if filled?
                                                                                 "green" "black"))
                                   (overlay/align "right" "middle" (square pipe-width "solid" (if filled?
                                                                                                  "green" "black"))
                                                  (overlay/align "middle" "bottom" (square pipe-width "solid" (if filled?
                                                                                                                  "green" "black"))
                                                                 (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                                                 "green" "black"))
                                                                                (square tile-side-length "solid" "gray"))))))]
     
    [(and (pipe-top pipe) (pipe-left pipe))
     (overlay/align "left" "middle" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (overlay/align "middle" "top" (square pipe-width "solid" (if filled?
                                                                                 "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "gray"))))]
     
    [(and (pipe-top pipe) (pipe-right pipe))
     (overlay/align "middle" "top" (square pipe-width "solid" (if filled?
                                                                  "green" "black"))
                    (overlay/align "right" "middle" (square pipe-width "solid" (if filled?
                                                                                   "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "gray"))))]
     
    [(and (pipe-bot pipe) (pipe-right pipe))
     (overlay/align "middle" "bottom" (square pipe-width "solid" (if filled?
                                                                     "green" "black"))
                    (overlay/align "right" "middle" (square pipe-width "solid" (if filled?
                                                                                   "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "gray"))))]
     
    [(and (pipe-bot pipe) (pipe-left pipe))
     (overlay/align "middle" "bottom" (square pipe-width "solid" (if filled?
                                                                     "green" "black"))
                    (overlay/align "left" "middle" (square pipe-width "solid" (if filled?
                                                                                  "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "gray"))))]
     
    [(and (pipe-top pipe) (pipe-bot pipe))
     (overlay/align "middle" "top" (square pipe-width "solid" (if filled?
                                                                  "green" "black"))
                    (overlay/align "middle" "bottom" (square pipe-width "solid" (if filled?
                                                                                    "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "grey"))))]
     
    [(and (pipe-left pipe) (pipe-right pipe))
     (overlay/align "right" "middle" (square pipe-width "solid" (if filled?
                                                                    "green" "black"))
                    (overlay/align "left" "middle" (square pipe-width "solid" (if filled?
                                                                                  "green" "black"))
                                   (overlay/align "middle" "middle" (square pipe-width "solid" (if filled?
                                                                                                   "green" "black"))
                                                  (square tile-side-length "solid" "grey"))))]
    [(pipe-left pipe)
     (overlay/align "left" "middle" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (square tile-side-length "solid" "grey"))]
    [(pipe-right pipe)
     (overlay/align "right" "middle" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (square tile-side-length "solid" "grey"))]
    [(pipe-top pipe)
     (overlay/align "middle" "top" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (square tile-side-length "solid" "grey"))]
    [(pipe-bot pipe)
     (overlay/align "middle" "bottom" (square pipe-width "solid" (if filled?
                                                                   "green" "black"))
                    (square tile-side-length "solid" "grey"))]))


(define PIPE-EX (list PIPE-TL PIPE-TR PIPE-BL))

(define PIPE-EX1 (list PIPE-TL PIPE-TR))

(define PIPE-EX2 (list PIPE-TL))

(define PIPE-EX3 (list PIPE-TL PIPE-TL PIPE-TL PIPE-TL ))

(define PIPE-EX4 (list PIPE-TL PIPE-TL PIPE-TL PIPE-TL PIPE-TL))

(define PIPE-EX5 (list PIPE-TL PIPE-TL PIPE-TL PIPE-TL PIPE-TL PIPE-TL ))

(define PIPE-EX0 (list))

;; TASK 7 - HW8 

;; incoming-pipe->image: ListOfPipes Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.

#; (check-expect (incoming-pipe->image PIPE-EX 60 20) (above (pipe->image (first PIPE-EX) 60 20 #f)
                                                          (pipe->image (first (rest PIPE-EX)) 60 20 #f)
                                                          (pipe->image (second (rest PIPE-EX)) 60 20 #f)
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")))
#;(check-expect (incoming-pipe->image PIPE-EX1 60 20) (above (pipe->image (first PIPE-EX1) 60 20 #f)
                                                          (pipe->image (first (rest PIPE-EX1)) 60 20 #f)
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")))
#;(check-expect (incoming-pipe->image PIPE-EX2 60 20) (above (pipe->image (first PIPE-EX2) 60 20 #f)
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")))
#;(check-expect (incoming-pipe->image PIPE-EX3 60 20) (above (pipe->image (first PIPE-EX3) 60 20 #f)
                                                          (pipe->image (first (rest PIPE-EX3)) 60 20 #f)
                                                          (pipe->image (second (rest PIPE-EX3)) 60 20 #f)
                                                          (pipe->image (third (rest PIPE-EX3)) 60 20 #f)
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")))
#;(check-expect (incoming-pipe->image PIPE-EX4 60 20) (above (pipe->image (first PIPE-EX4) 60 20 #f)
                                                          (pipe->image (first (rest PIPE-EX4)) 60 20 #f)
                                                          (pipe->image (second (rest PIPE-EX4)) 60 20 #f)
                                                          (pipe->image (third (rest PIPE-EX4)) 60 20 #f)
                                                          (pipe->image (fourth (rest PIPE-EX4)) 60 20 #f)
                                     
                                                          (square 60 "solid" "transparent")))
#;(check-expect (incoming-pipe->image PIPE-EX0 60 20) (above 
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")
                                                          (square 60 "solid" "transparent")))


                                                          
                                                         
#;(define (incoming-pipe->image lop tsl pw)
  (cond
    [(empty? lop)
     (above 
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent"))]
    [(>= (length lop) 6)
     (above 
      (pipe->image (first lop) tsl pw #f)
      (pipe->image (first (rest lop)) tsl pw #f)
      (pipe->image (second (rest lop)) tsl pw #f)
      (pipe->image (third (rest lop)) tsl pw #f)
      (pipe->image (fourth (rest lop)) tsl pw #f)
      (pipe->image (fifth (rest lop)) tsl pw #f))]
    [(= (length lop) 5)
     (above 
      (pipe->image (first lop) tsl pw #f)
      (pipe->image (first (rest lop)) tsl pw #f)
      (pipe->image (second (rest lop)) tsl pw #f)
      (pipe->image (fourth (rest lop)) tsl pw #f)
      (square tsl "solid" "transparent"))]
    [(= (length lop) 4)
     (above 
      (pipe->image (first lop) tsl pw #f)
      (pipe->image (first (rest lop)) tsl pw #f)
      (pipe->image (second (rest lop)) tsl pw #f)
     (pipe->image (third (rest lop)) tsl pw #f)
     (square tsl "solid" "transparent")
     (square tsl "solid" "transparent"))]
    [(= (length lop) 3)
     (above 
      (pipe->image (first lop) tsl pw #f)
      (pipe->image (first (rest lop)) tsl pw #f)
      (pipe->image (second (rest lop)) tsl pw #f)
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent"))]
    [(= (length lop) 2)
     (above 
      (pipe->image (first lop)tsl pw #f)
      (pipe->image (first (rest lop))tsl pw #f)
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent"))]
    [(= (length lop) 1)
     (above 
      (pipe->image (first lop) tsl pw #f)
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent")
      (square tsl "solid" "transparent"))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PART 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct grid [tile xy])

; A grid is a (make-grid ListOfPipeCords PosReal) where:
; - tile is a ListOfPipeCords within a grid
; - xy is the dimensons of the grid (n x n)

(define GRID1 (make-grid (list PC1 PC2 PC3 PC4) 5))
(define GRID2 (make-grid (list PC1 PC2 PC3) 8))
(define GRID3 (make-grid (list PC1 PC2) 4))
(define GRID4 (make-grid (list PC0 '()) 8))

(define (grid-temp p)
  (... (lopc-temp(grid-tile p)) ...
       (grid-xy p) ...))



; create-cell; PosReal -> Image
; draws a cell with a black outline where the number inputted are the dimensions of that cell

(check-expect (create-cell 10) (square 10 "outline" "black"))
(check-expect (create-cell 40) (square 40 "outline" "black"))
(check-expect (create-cell 50) (square 50 "outline" "black"))
(check-expect (create-cell 100) (square 100 "outline" "black"))

(define (create-cell num)
  (square num "outline" "black"))

; create-row-of-cells; PosReal PosReal -> Image
; draws a row of cells given a number of cells and constant dimension for those cells

(check-expect (create-row-of-cells 1 60) (square 60 "outline" "black"))
                                                 
(check-expect (create-row-of-cells 4 60) (beside (square 60 "outline" "black")
                                                 (square 60 "outline" "black")
                                                 (square 60 "outline" "black")
                                                 (square 60 "outline" "black")))
(check-expect (create-row-of-cells 5 60) (beside (square 60 "outline" "black")
                                                 (square 60 "outline" "black")
                                                 (square 60 "outline" "black")
                                                 (square 60 "outline" "black")
                                                 (square 60 "outline" "black")))

(define (create-row-of-cells num-of-cells tsl)
  (cond
    [(= num-of-cells 1) (create-cell tsl)]
    [else (beside (create-cell tsl) (create-row-of-cells (- num-of-cells 1) tsl))]))

; create-grid: PosReal PosReal PosReal -> Image
; draws a grid given a numbers of cells in a number of rows and the dimension of each cell in a row.

(check-expect (create-grid 1 1 60) (create-row-of-cells 1 60))
(check-expect (create-grid 5 2 60) (above (create-row-of-cells 5 60) (create-row-of-cells 5 60)))
(check-expect (create-grid 10 4 60) (above (create-row-of-cells 10 60) (create-row-of-cells 10 60) (create-row-of-cells 10 60) (create-row-of-cells 10 60)))

(define (create-grid num-of-cells num-of-rows tsl)
  (cond
    [(= num-of-rows 1) (create-row-of-cells num-of-cells tsl)]
    [else (above (create-row-of-cells num-of-cells tsl) (create-grid num-of-cells (- num-of-rows 1) tsl))]))


; pipe-on-cell: ListOfPipeCords PosReal PosReal GooFlow
; draws each cell on a grid and the pipes given a ListOfPipeCords

(check-expect (pipe-on-cell LOPC-E 60 20 GOO-D) '())
#;(check-expect (pipe-on-cell LOPC1 60 20 GOO-D) (list (pipe->image STARTING-PIPE-R 60 20 #t)
                                                     (pipe->image PIPE-TBLR 60 20 #f)
                                                     (pipe->image PIPE-LR 60 20 #f)
                                                     (pipe->image PIPE-BR 60 20 #f)))
#;(check-expect (pipe-on-cell LOPC2 60 20 GOO-D) (list (pipe->image STARTING-PIPE-R 60 20 #f)
                                                     (pipe->image PIPE-LR 60 20 #f)
                                                     (pipe->image PIPE-BR 60 20 #f)
                                                     (pipe->image PIPE-TB 60 20 #f)))
                                                     
; pipe-on-cell: ListOfPipeCords PosReal PosReal -> ListOfImage
; outputs a ListPipeCords on a cell as a list of images

(check-expect (pipe-on-cell LOPC-E 60 20 GOO-D) '())
#;(check-expect (pipe-on-cell LOPC1 60 20 GOO-D) (list (pipe->image STARTING-PIPE-R 60 20 #t)
                                                     (pipe->image PIPE-TBLR 60 20 #f)
                                                     (pipe->image PIPE-LR 60 20 #f)
                                                     (pipe->image PIPE-BR 60 20 #f)))

#;(check-expect (pipe-on-cell LOPC2 60 20 GOO-D) (list (pipe->image STARTING-PIPE-R 60 20 #f)
                                                     (pipe->image PIPE-LR 60 20 #f)
                                                     (pipe->image PIPE-BR 60 20 #f)
                                                     (pipe->image PIPE-TB 60 20 #f)))


#;(define (pipe-on-cell lopc tsl pw gf)
    (local [; lopc->image: Pipe-Cords -> Image
            ; draws a Pipe-Cords as an image
            (define (lopc->image pipe-cords)
    (local [(define (check-goo? pipe-x gf-x) (ormap (lambda (x)
     (and (= (pipe-cords-x pipe-cords) (pipe-cords-x x)) (= (pipe-cords-y pipe-cords) (pipe-cords-y x)))) (GooFlow-lopc gf)))]
       (pipe->image (pipe-cords-pipe pipe-cords) tsl pw (check-goo? pipe-cords gf))))] (map lopc->image lopc)))



; coordinates : [ListOfPipeCords] PosReal -> [ListPosns]
; outputs a list of posns for a given list of pipe cords. 

(define (coordinates lopc tsl)
  (local
    [(define (find-coordinates cell)
       (make-posn (+ (* (pipe-cords-y cell) tsl) (/ tsl 2)) (+ (* (pipe-cords-x cell) tsl) (/ tsl 2))))]
          (map find-coordinates lopc)))

; grid->image : Grid PosReal PosReal GooFlow -> Image
; Draws the grid of the pipes with the cells having the correct dimensions.

(define (grid->image grid tsl pw gf)
  (place-images
   (pipe-on-cell (grid-tile grid) tsl pw gf) (coordinates (grid-tile grid) tsl) (create-grid (grid-xy grid) (grid-xy grid) tsl)))


; place-pipe : Grid Pipe Number Number -> Grid
; Places the pipe on the grid on a column and row.

(check-expect (place-pipe GRID1 PIPE-TL 50 50) (make-grid (list (make-pipe-cords PIPE-TL 50 50 #f) PC1 PC2 PC3 PC4) 5))
(check-expect (place-pipe GRID2 PIPE-TBLR 50 50) (make-grid (list (make-pipe-cords PIPE-TBLR 50 50 #f) PC1 PC2 PC3) 8))
(check-expect (place-pipe GRID3 PIPE-BR 50 50) (make-grid (list (make-pipe-cords PIPE-BR 50 50 #f) PC1 PC2) 4)) 

(define (place-pipe grid pipe row column)
  (make-grid
   (cons (make-pipe-cords pipe row column #f) (grid-tile grid))
   (grid-xy grid)))

; pipe-at : Grid Integer Integer -> Boolean [Optional Pipe]
; Produces pipe at the given row and column or #false

(check-expect (pipe-at GRID1 50 50) #f)
(check-expect (pipe-at GRID2 20 20) #f)
(check-expect (pipe-at GRID1 5 10) PIPE-TBLR)

(define (pipe-at grid row column)
  (cond
    [(empty? (grid-tile grid)) #f]
    [(and (= (pipe-cords-x (first (grid-tile grid))) row)
          (= (pipe-cords-y (first (grid-tile grid))) column))
     (pipe-cords-pipe (first (grid-tile grid)))]
    [else #f]))
          
;; TASK 5 - HW8 

(define-struct GameState (grid next tsl pw sp gf))
; GameState is a (make-GameState Grid [List-of Pipes] Num Num Pipe-Cords GooFlow) where
; - grid is the given grid
; - next is a ListOfPipes
; - tsl is the tile-side-length 
; - pw is the pipe-width 
; - sp is the starting pipe, a Pipe and its (x,y) and filled?
; - gf is a GooFlow
        
(define STARTING-GRID (make-GameState (make-grid '() 7) ALL-PIPES TILE-SIDE-LENGTH PIPE-WIDTH PC5 GOO-D))
(define GAME1 (make-GameState GRID1 (list PIPE-BL PIPE-TR PIPE-TBLR) TILE-SIDE-LENGTH PIPE-WIDTH PC5 GOO-D))
(define GAME2 (make-GameState GRID2 (list PIPE-TL PIPE-TR PIPE-BL) TILE-SIDE-LENGTH PIPE-WIDTH PC5 GOO-D))
(define GAME3 (make-GameState GRID3 (list PIPE-LR PIPE-TL) TILE-SIDE-LENGTH PIPE-WIDTH PC5 GOO-D))

#; (define (game-temp g)
     (...
      (GameState-grid g)...
      (lopc-temp (GameState-next g)) ...
      (Gamestate-tsl g) ...
      (GameState-pw g) ...
      (pipe-temp (GameState-sp g)) ...
      (GooFlow-temp (GameState-gf g)) ...))


; draw-state: GameState -> Image
; draws the current GameState as a grid and incoming pipes

(define (draw-state state)
  (beside (grid->image (GameState-grid state) (GameState-tsl state) (GameState-pw state) (GameState-gf state))
        (incoming-pipe->image (GameState-next state) (GameState-tsl state) (GameState-pw state))))



; Tile-Num : GameState PosReal PosReal -> [List-of-Num]
; Gets the column number and the row number, and returns
; list of row number and column number

(check-expect (tile-num GAME1 100 100) (list 4 2))
(check-expect (tile-num GAME2 30 50) (list 8 1))
(check-expect (tile-num GAME3 30 30) (list 4 1))

(define (tile-num state xcor ycor)
  (list
   (- (grid-xy (GameState-grid state)) (floor (/ ycor TILE-SIDE-LENGTH)))
   (ceiling (/ xcor TILE-SIDE-LENGTH))))

; Next-pipe : GameState PosReal PosReal -> GameState
; Places the next pipe from the given list.

(check-expect (next-pipe GAME1 50 50) (make-GameState
 (make-grid
  (list
   (make-pipe-cords
    (make-pipe #false #true #true #false) 50 50 #false)
   (make-pipe-cords
    (make-pipe #true #true #true #true) 5 10 #false)
   (make-pipe-cords
    (make-pipe #false #false #true #true) 7 20 #false)
   (make-pipe-cords
    (make-pipe #false #true #false #true) 40 27 #false)
   (make-pipe-cords
    (make-pipe #true #true #false #false) 30 20 #false)) 5)
 (list
  (make-pipe #true #false #false #true)
  (make-pipe #true #true #true #true))  60 20
 (make-pipe-cords
  (make-pipe #false #false #false #true) 5 5 #false)
 (make-GooFlow
  "right"
  (list
   (make-pipe-cords
    (make-pipe #false #false #false #true) 5 5 #false)))))
(check-expect (next-pipe GAME2 30 30) (make-GameState
 (make-grid
  (list
   (make-pipe-cords
    (make-pipe
     #true
     #false
     #true
     #false)
    30
    30
    #false)
   (make-pipe-cords
    (make-pipe #true #true #true #true)
    5
    10
    #false)
   (make-pipe-cords
    (make-pipe
     #false
     #false
     #true
     #true)
    7
    20
    #false)
   (make-pipe-cords
    (make-pipe
     #false
     #true
     #false
     #true)
    40
    27
    #false))
  8)
 (list
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false))
 60
 20
 (make-pipe-cords
  (make-pipe #false #false #false #true)
  5
  5
  #false)
 (make-GooFlow
  "right"
  (list
   (make-pipe-cords
    (make-pipe
     #false
     #false
     #false
     #true)
    5
    5
    #false)))))
              
(check-expect (next-pipe GAME3 75 75) (make-GameState
 (make-grid
  (list
   (make-pipe-cords
    (make-pipe
     #false
     #false
     #true
     #true)
    75
    75
    #false)
   (make-pipe-cords
    (make-pipe #true #true #true #true)
    5
    10
    #false)
   (make-pipe-cords
    (make-pipe
     #false
     #false
     #true
     #true)
    7
    20
    #false))
  4)
 (list
  (make-pipe #true #false #true #false))
 60
 20
 (make-pipe-cords
  (make-pipe #false #false #false #true)
  5
  5
  #false)
 (make-GooFlow
  "right"
  (list
   (make-pipe-cords
    (make-pipe
     #false
     #false
     #false
     #true)
    5
    5
    #false)))))

(define (next-pipe g row column)
  (make-GameState
   (place-pipe (GameState-grid g) (first (GameState-next g)) row column)
   (rest-pipe (GameState-next g))
   TILE-SIDE-LENGTH
   PIPE-WIDTH
   (GameState-sp g)
   (GameState-gf g)))


; Place-next-pipe : [List-of-Pipes] -> Pipe
; Helper function that gets the first pipe from the list pipe. 

(define (place-next-pipe pipe)
  (if
   (empty? pipe) pipe
   (first pipe)))

; New-grid : Grid PosReal PosReal PosReal -> Image
; Creates a new grid when it is placed in a new row or column 

(define (new-grid grid row column)
  (place-pipe grid (first (grid-tile grid)) row column))

; Rest-pipe : [List-of Pipe] -> [List-of Pipe]
; Gets the next pipe from the list, recurrs if no more pipes left.  

(define (rest-pipe pipe)
  (if
   (> (length pipe) 1)
   (rest pipe) '()))

        
; Place-pipe-on-click : GameState PosReal PosReal MouseEvent -> GameState
; When the mouse is clicked down in an x or y coordinate, makes
; the pipe that is next in the list placed into the tile of the grid,
; where the mouse was clicked.

;; TASK 7 (check for where click occurs) - HW8 

(define (place-pipe-on-click state x-cor y-cor mouse-event)
  (if (not (> x-cor (* (GameState-tsl state) (grid-xy (GameState-grid state)))))
  (cond
    [(and (empty? (GameState-next state))
          (string=? mouse-event "button-down"))
     (make-GameState
            (GameState-grid state)
            (GameState-next state)
            (GameState-tsl state)
            (GameState-pw state)
            (GameState-sp state)
            (grid-goo-propagate (GameState-gf state) (GameState-grid state)))]
    [(and (cons? (GameState-next state))
          (string=? mouse-event "button-down"))
     (next-pipe state (floor (/ y-cor TILE-SIDE-LENGTH)) (floor (/ x-cor TILE-SIDE-LENGTH)))]
    [else state])
  state))


;; ;; TASK 6 - HW8 

; gamestate-init : GameState -> GameState 
; initializes the gamestate


(define CONSTANT (random 4))
(define sp (cond
             [(= CONSTANT 0) "right"] 
             [(= CONSTANT 1) "top"]
             [(= CONSTANT 2) "bottom"]
             [(= CONSTANT 3) "left"]))


(check-expect (gamestate-init 7 5 4 LEFT ALL-PIPES) (make-GameState
 (make-grid (list (make-pipe-cords (make-pipe #false #false #true #false) 5 4 #true)) 7)
 (list
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #true #true #false #false)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 60
 20
 (make-pipe-cords (make-pipe #false #false #true #false) 5 4 #true)
 (make-GooFlow "left" (list (make-pipe-cords (make-pipe #false #false #true #false) 5 4 #true)))))

(check-expect (gamestate-init 4 1 3 TOP ALL-PIPES)(make-GameState
 (make-grid (list (make-pipe-cords (make-pipe #true #false #false #false) 1 3 #true)) 4)
 (list
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #true #true #false #false)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 60
 20
 (make-pipe-cords (make-pipe #true #false #false #false) 1 3 #true)
 (make-GooFlow "top" (list (make-pipe-cords (make-pipe #true #false #false #false) 1 3 #true)))))

(check-expect (gamestate-init 6 2 3 RIGHT ALL-PIPES)(make-GameState
 (make-grid (list (make-pipe-cords (make-pipe #false #false #false #true) 2 3 #true)) 6)
 (list
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #true #true #false #false)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 60
 20
 (make-pipe-cords (make-pipe #false #false #false #true) 2 3 #true)
 (make-GooFlow "right" (list (make-pipe-cords (make-pipe #false #false #false #true) 2 3 #true)))))

(check-expect (gamestate-init 7 3 4 BOTTOM ALL-PIPES) (make-GameState
 (make-grid (list (make-pipe-cords (make-pipe #false #true #false #false) 3 4 #true)) 7)
 (list
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #true #true #false #false)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 60
 20
 (make-pipe-cords (make-pipe #false #true #false #false) 3 4 #true)
 (make-GooFlow "bottom" (list (make-pipe-cords (make-pipe #false #true #false #false) 3 4 #true)))))








(define (gamestate-init d x y str lop)
  (make-GameState
   (make-grid (list (cond
                                   [(string=? str "right") (make-pipe-cords STARTING-PIPE-R x y #t)]
                                   [(string=? str "left") (make-pipe-cords STARTING-PIPE-L x y #t)]
                                   [(string=? str "bottom")(make-pipe-cords STARTING-PIPE-B x y #t)]
                                   [(string=? str "top") (make-pipe-cords STARTING-PIPE-T x y #t)])) d)
    lop 60 20
                            
   (cond
                                   [(string=? str "right") (make-pipe-cords STARTING-PIPE-R x y #t)]
                                   [(string=? str "left") (make-pipe-cords STARTING-PIPE-L x y #t)]
                                   [(string=? str "bottom")(make-pipe-cords STARTING-PIPE-B x y #t)]
                                   [(string=? str "top") (make-pipe-cords STARTING-PIPE-T x y #t)])
   (make-GooFlow str (list (cond
                                   [(string=? str "right") (make-pipe-cords STARTING-PIPE-R x y #t)]
                                   [(string=? str "left") (make-pipe-cords STARTING-PIPE-L x y #t)]
                                   [(string=? str "top") (make-pipe-cords STARTING-PIPE-T x y #t)]
                                   [(string=? str "bottom") (make-pipe-cords STARTING-PIPE-B x y #t)])))))


;; TASK 9 - HW8 

(define GS-1 (gamestate-init 7 5 5 "left" ALL-PIPES))
(define GS-2 (gamestate-init 7 6 4 "right" ALL-PIPES))
(define GS-3 (gamestate-init 5 3 2 "top" ALL-PIPES))
(define GS-4 (gamestate-init 6 4 4 "bottom" ALL-PIPES))


; stop-game: GameState -> GameState
; Stops game if list of incoming pipes is exhausted 
#; (define (stop-game state)
  (and (empty? (GameState-next state))
       (not (pipe? (GameState-current state)))
       (= (length ((GooFlowlopc state) (GameState-gf state) (grid-goo-propagate (GameState-gf state) (GameState-grid state)))))))



; Pipe-fantasy ; GameState -> GameState 
(define (pipe-fantasy game-state)
  (big-bang game-state
    [to-draw draw-state]
    [on-mouse place-pipe-on-click])) 

 (pipe-fantasy GS-1) 


