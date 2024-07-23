;;Please feel free to rename these commands as you desire.
(defun c:xxx () (section t nil)); SECTION W/ BORDER

* * * * * ERROR ROUTINE * * * * *
(defun newerr (msg)
 (prompt (strcat "\nSection cancelled: " msg)); PRINT ERROR
 (setvar "cmdecho" cmd); RESET COMMAND ECHO
 (setvar "highlight" hlt); RESET HIGHLIGHT
)


* * * * * MAIN FUNCTION * * * * *
;If the first argument has any value other than nil then the border will be left.  If it is nil
;then the border is erased.
;If the second argument is has any value other than nil then entities inside the border will be erased.
;If it is nil then entities outside the border are erase.
;For very large area drawings (maps or something), the DST variable may need to be changed.  If you
;find that not all entities are being trimmed properly try increasing the number higher than 1000.

(defun section (bdr n / olderr newerr cmd hlt p1 p2 p1x p1y p2x p2y p3 p4 dst plus minus p1a p2a p3a p4a lst)
 (graphscr); CHANGE TO GRAPHICS SCREEN
 (setq olderr *error* ; SET UP NEW
       *error* newerr ; ERROR ROUTINE
       cmd (getvar "cmdecho"); SAVE COMMAND ECHO SETTING
       hlt (getvar "highlight"); SAVE HIGHLIGHT SETTING
       p1 (getpoint "\nSelect first corner of rectangle: "); GET LL CORNER OF RECTANGLE
       p2 (getcorner p1 "\nSelect other corner: "); GET UR CORNER
       p1x (car p1)
       p1y (cadr p1)
       p2x (car p2)
       p2y (cadr p2)
       p3 (list p2x p1y); BUILD LR CORNER
       p4 (list p1x p2y); BUILD UL CORNER
       dst (/ (distance p1 p2) 1000.0); OFFSET FACTOR FOR TRIMMING
       plus (if n - +)
       minus (if n + -)
 );END SETQ
 (cond
  ((and (< p1x p2x) (< p1y p2y)); P1 IS LL CORNER
   (setq p1a (list (minus p1x dst) (minus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (plus p2x dst) (plus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (> p1x p2x) (< p1y p2y)); P1 IS UL CORNER
   (setq p1a (list (plus p1x dst) (minus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (minus p2x dst) (plus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (> p1x p2x) (> p1y p2y)); P1 IS UR CORNER
   (setq p1a (list (plus p1x dst) (plus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (minus p2x dst) (minus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (< p1x p2x) (> p1y p2y)); P1 IS LR CORNER
   (setq p1a (list (minus p1x dst) (plus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (plus p2x dst) (minus p2y dst))); BUILD UR TRIM LINE POINT
  )
 ); END COND
 (setq p3a (list (car p2a) (cadr p1a)); BUILD LR TRIM LINE POINT
       p4a (list (car p1a) (cadr p2a)); BUILD UL TRIM LINE POINT
 ); END SETQ
 (setvar "cmdecho" 0); TURN OFF COMMAND ECHO
 (setvar "highlight" 0); TURN OFF HIGHLIGHT

 (if n                                          ;ERASE ENTITIES
  (command "_.erase" "_w" p1 p2 "_r" lst "")    ;INSIDE RECTANGLE
  (command "_.erase" "_all" "_r" "_c" p1 p2 "") ;OUTSIDE RECTANGLE
 ); END IF
 (command "_.trim" lst "" "_f" p1a p3a ""     ;TRIM ENTITIES AROUND BORDER
                          "_f" p3a p2a ""     ;DO TO THE FINICKY NATURE OF TRIMMING
                          "_f" p2a p4a ""     ;WITH THE FENCE OPTION, I HAVE USED FOUR
                          "_f" p4a p1a "" ""  ;FENCE LINES INSTEAD OF ONE LONG ONE
 ); END COMMAND
 (command "ZOOM" "E") ;; zoom to extents
 (command "QSAVE") ;; quicksave
 (if (not bdr) (entdel lst)); DELETE POLYLINE BORDER IF DESIRED
 (setq *error* olderr); RESTORE ORIGINAL ERROR ROUTINE
 (setvar "highlight" hlt); RESTORE HIGHLIGHT
 (setvar "cmdecho" cmd); RESTORE COMMAND ECHO
 (princ); EXIT CLEANLY
)