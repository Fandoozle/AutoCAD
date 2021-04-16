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