;; Run the routine by the command BE
;; This routine breaks a line at a specified gap distance. 
;; To use it enter the distance desired and select the point at intersection then select the line to use to break with.
(Defun C:BE (/ s1  obj ds pr PT1 PT2 )(vl-load-com)
  (setq s1 (getvar "osmode")) ;; find the current osmode of drawing
  (setq gap (cond
  	((getdist (strcat "\nEnter distance"
                 (if gap (strcat " <" (rtos gap) ">: ") ": ")
                            )))(gap))
                )
  (setvar osmode 2048)
  (setq PT1 (getpoint "\nPick central point to trim:")) ;; select a central point
  (setq obj (car (entsel "\nSelect object to trim:")))  ;; select the objects to trim
  (setq ds (vlax-curve-getDistAtPoint obj (vlax-curve-getclosestpointto obj PT1)))    
  (setq pr (vlax-curve-getParamAtPoint obj (vlax-curve-getclosestpointto obj PT1)))
	;; break the line at the specific point selected at a distance given by 0.5 units
	(command "_break" obj "_non" (vlax-curve-getPointAtDist obj (+ ds (* gap 0.5)))
        	"_non" (vlax-curve-getPointAtDist obj (- ds (* gap 0.5))))
  (setvar "osmode" s1) ;; set the osmode back to the original osmode of the drawing
    (princ)
)

;; Definitions below
;; Variables: s1, obj, ds, pr, PT1, PT2, gap
;; s1: finds the "osmode" current snap point
;; gap: specify the distance of the break
;; PT1: the intersection of where to trim
;; osmode: snap point
;; strcat Returns a string that is the concatenation of multiple strings
;; entsel: A prompt string to be displayed to users. If omitted, entsel prompts with the message, "Select object." 
;; setq: Sets the value of a symbol or symbols to associated expressions 
;; princ: Prints an expression to the command line, or writes an expression to an open file 
;; car: Returns the first element of a list unless empty then returns "nil"
;; vlax-curve-getDistAtPoint: Returns the length of the curve's segment between the curve's start point and the specified point 
;; vlax-curve-getParamAtPoint: Returns the parameter of the curve at the point