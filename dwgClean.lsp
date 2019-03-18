;: DEFUN is the function
;; C: tells autocad what the command is 
;; VCD is the command to start the script
;; () indicates no local variables and no arguments
;; This routine cleans up a drawing by running and audit and a purge VCD (very clean drawing) 
(DEFUN C:VCD ()
 (SETVAR "CMDECHO" 0);; turn echo off-turns off the read out of the following commands
 (command "AUDIT" "Y");; runs an audit to fix errors
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (repeat 2 ;; repeat following commands  2 times	
  (command "-PURGE" "A" "*" "N");; purge drawing
 );;
  (while (= 1 (getvar "cmdactive")) (command pause)) ;; pauses while command is active
 (command "ZOOM" "E");; zoom to extents
 (command "QSAVE");; quicksave
 (SETVAR "CMDECHO" 1);; turn echo on-turns on the read out of commands
);;END DEFUN