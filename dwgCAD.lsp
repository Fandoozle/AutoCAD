;; Cleans up drawing to create a CAD file
(DEFUN C:CAD ()
 (SETVAR "CMDECHO" 0);; turn echo off-turns off the read out of the following commands
 (command "_.layer" "_set" "0" "")
(command "-layer" "Thaw" "*" "ON" "*" "UNLOCK" "*" "S" "0" "") ;; thaws, turns on and unlocks all layers then sets the current layer to 0
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
  (command "AUDIT" "Y");; runs an audit to fix errors
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
(repeat 4 ;; repeat following commands  4 times	
  (command "-overkill" all "" "");; overkill everything
 );;
(while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (repeat 4 ;; repeat following commands  2 times	
  (command "-PURGE" "A" "*" "N");; purge drawing
 );;
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active

(if (and ;if
(tblsearch "LAYER" "*REV*") ;exists and
(not (ssget "X" '((8 . "*REV*")))) ;is empty and
(/= (getvar "CLAYER") "*REV*") ;not current
) ;then
(command "_PURGE" "_LAY" "*REV*" "_NO");purge
)
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active

 (command "ZOOM" "E");; zoom to extents
 (command "QSAVE");; quicksave
 (SETVAR "CMDECHO" 1);; turn echo on-turns on the read out of commands
);;END DEFUN