(DEFUN C:RCP ()
 (SETVAR "CMDECHO" 0);; turn echo off-turns off the read out of the following commands
 (command "AUDIT" "Y");; runs an audit to fix errors
 (command "imageframe" 1)
 (command "imageframe" 0)
 (command "_.layer" "_set" "0" "")
 (command "-layer" "Thaw" "*" "ON" "*" "UNLOCK" "*" "S" "0" "") ;; thaws, turns on and unlocks all layers then sets the current layer to 0
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
   (while (ssget "X" '((0 . "INSERT"))) ;; bursts all blocks in drawing. Checks to see if there are any in the drawing after
    (sssetfirst nil (ssget "X" (list )))
    (c:burst)
   ); end while
;;delete specific layers
 (command "-laydel" "name" "A-WALL" "" "Y")
 (command "-laydel" "name" "A-DOOR" "" "Y")
 (command "-laydel" "name" "I-WALL" "" "Y")
 (command "-laydel" "name" "Q-CASE" "" "Y")
 (command "-laydel" "name" "Q-SPCQ" "" "Y")
 (command "-laydel" "name" "Q-CASE-HDLN" "" "Y")
 (command "-laydel" "name" "A-GLAZ" "" "Y")
 (command "-laydel" "name" "A-GLAZ-CURT" "" "Y")
 (command "-laydel" "name" "A-GLAZ-CWMG" "" "Y")
 (command "-laydel" "name" "A-AREA-IDEN" "" "Y")
;; end delete specific layers
 ;;(repeat 2 ;; repeat following commands  2 times	
  (command "-PURGE" "A" "*" "N");; purge drawing
 );;
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (command "ZOOM" "E");; zoom to extents
 (command "AUDIT" "Y");; runs an audit to fix errors
 (command "QSAVE");; quicksave
 (SETVAR "CMDECHO" 1);; turn echo on-turns on the read out of commands
);;END DEFUN