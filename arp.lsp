;; This routine creates an array along a path for a JFK barrier wall
;; It prompts the user to select objects to array, select the path along which to array,
;; and enter a distance between objects (defaulting to 6 feet if not entered).
;; It then calls the "._arraypath" command with the selected objects, path, and distance as arguments.
(defun c:ARP (/ sel path dist) ;Arraypath for JFK barrier wall

  (if (and (setq sel (ssget))
	   (setq path (car (entsel "\nSelect path:")))
	   (setq dist (cond ((getdist "\nEnter the Distance Between Objects <6 ft>: "))
			    (6.)))
	   (initcommandversion)
	   )
    (command "._arraypath" sel "" path "I" dist "F" "M" "D" "X" ))
  (princ)
  )
