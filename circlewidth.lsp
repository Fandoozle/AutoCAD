; Written By: Jason Piercey 07.31.01
; Revised: 01.16.02 To handle multiple selection
; This function converts circles to lightweight polylines
(defun C:Circle2Pline (/ CirEnt   CirElst  CirCen   CirRad   CirLay
                         CirLin   CirClr   CirLts   PlineEnt ss
                         i        );ss1)
  
  (setq ss (ssget '((0 . "CIRCLE")))) ; Select all circles in the drawing
  (if ss ; If the selection set is not empty
    (progn ; Begin a series of expressions
      (setq i 0 );ss1 (ssadd)) ; Initialize a counter variable
      (repeat (sslength ss) ; Loop through each circle in the selection set
        (setq CirEnt (ssname ss i) ; Get the entity name of the circle              
              CirElst (entget CirEnt) ; Get the entity data of the circle
              CirCen  (cdr (assoc 10 CirElst)) ; Get the center point of the circle
              CirRad  (cdr (assoc 40 CirElst)) ; Get the radius of the circle
              CirLay  (cdr (assoc  8 CirElst)) ; Get the layer of the circle
              CirLin  (cdr (assoc  6 CirElst)) ; Get the linetype of the circle
              CirClr  (cdr (assoc 62 CirElst)) ; Get the color of the circle
              CirLts  (cdr (assoc 48 CirElst)) ; Get the linetype scale of the circle
              )
        
        (setq PlineEnt (list '(0 . "LWPOLYLINE") ; Create a list of entity data for the polyline
                             '(100 . "AcDbEntity")
                             (cons 8 CirLay) ; Set the layer of the polyline to the same as the circle
                             '(100 . "AcDbPolyline")
                             '(90 . 2) ; Set the number of vertices to 2
                             '(70 . 1) ; Set the closed flag to 1
                             '(43 . 0.0) ; Set the constant width to 0.0
                             '(38 . 0.0) ; Set the elevation to 0.0
                             '(39 . 0.0) ; Set the thickness to 0.0
                             (cons 10 (polar CirCen (* pi) CirRad)) ; Set the first vertex to the top of the circle
                             '(40 . 0.0) ; Set the starting width to 0.0
                             '(41 . 0.0) ; Set the ending width to 0.0
                             '(42 . 1.0) ; Set the bulge to 1.0
                             (cons 10 (polar CirCen (* pi 2.0) CirRad)) ; Set the second vertex to the bottom of the circle
                             '(40 . 0.0) ; Set the starting width to 0.0
                             '(41 . 0.0) ; Set the ending width to 0.0
                             '(42 . 1.0) ; Set the bulge to 1.0
                             '(210 0.0 0.0 1.0) ; Set the normal vector to the Z-axis
                             )
              )
        
        (if CirLin (setq PlineEnt (append PlineEnt (list (cons 6  CirLin))))) ; If the circle has a linetype, set the polyline to the same
        (if CirClr (setq PlineEnt (append PlineEnt (list (cons 62 CirClr))))) ; If the circle has a color, set the polyline to the same
        (if CirLts (setq PlineEnt (append PlineEnt (list (cons 48 CirLts))))) ; If the circle has a linetype scale, set the polyline to the same
        (entmake PlineEnt) ; Create the polyline entity
        (entdel CirEnt) ; Delete the circle entity
        (setq i (1+ i)) ; Increment the counter variable
        )
      )
    )
  ;(ssget "p") ; This line is commented out
  (princ (strcat "\n"(itoa i) " Circles converted to LwPolylines")) ; Print the number of circles converted
  (princ) ; Exit the function quietly
  )
