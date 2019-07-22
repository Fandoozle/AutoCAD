(DEFUN C:VCD ()
 (SETVAR "CMDECHO" 0);; turn echo off-turns off the read out of the following commands
 (command "AUDIT" "Y");; runs an audit to fix errors
 (command "imageframe" 1)
 (command "imageframe" 0)
 (command "_.layer" "_set" "0" "")
 (command "-laydel" "name" "layerx" "" "y") ;;Delete revision layers
(command "-layer" "Thaw" "*" "ON" "*" "UNLOCK" "*" "S" "0" "") ;; thaws, turns on and unlocks all layers then sets the current layer to 0
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
   (while (ssget "X" '((0 . "INSERT"))) ;; bursts all blocks in drawing. Checks to see if there are any in the drawing after
    (sssetfirst nil (ssget "X" (list )))
    (c:burst)
   ); end while
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (repeat 2 ;; repeat following commands  2 times	
  (command "-PURGE" "A" "*" "N");; purge drawing
 );;
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (command "ZOOM" "E");; zoom to extents
 (command "QSAVE");; quicksave
 (SETVAR "CMDECHO" 1);; turn echo on-turns on the read out of commands
);;END DEFUN