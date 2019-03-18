;: DEFUN is the function
;; C: tells autocad what the command is 
;; FOO is the command to start the script
;; () indicates no local variables and no arguments
;; This cleans up the drawing by thawing and making all layers visible, then bursting all blocks in the drawing.
;; Only use the bursting section for smaller drawings. It will take a while on larger ones.
;; Overkills any overlap after the bursting
;; Resets the layer properties to "ByLayer"
;; Audits the drawing
;; Purgest the drawing twice
;; Zooms to extents then saves
;; Pauses during each command to allow for processing
(DEFUN C:FOO ()
 (SETVAR "CMDECHO" 0);; turn echo off-turns off the read out of the following commands
 (command "-layer" "Thaw" "*" "ON" "*" "UNLOCK" "*" "S" "0" "") ;; thaws, turns on and unlocks all layers then sets the current layer to 0
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
   (while (ssget "X" '((0 . "INSERT"))) ;; bursts all blocks in drawing. Checks to see if there are any in the drawing after
    (sssetfirst nil (ssget "X" (list )))
    (c:burst)
   ); end while
 (repeat 4 ;; repeat following commands  4 times	
  (command "-overkill" all "" "");; overkill everything
 );;
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (command "SETBYLAYER" "ALL" "" "Y" "Y");; set all objects to be "ByLayer"
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (command "AUDIT" "Y");; runs an audit to fix errors
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (repeat 2 ;; repeat following commands 2 times	
  (command "-PURGE" "A" "*" "N");; purge drawing
 );;
 (command "ZOOM" "E");; zoom to extents
 (command "QSAVE");; quicksave
 (SETVAR "CMDECHO" 1);; turn echo on-turns on the read out of commands
(princ)
);;END DEFUN