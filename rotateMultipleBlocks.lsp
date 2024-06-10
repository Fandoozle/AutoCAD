;| RMB 
=========================================================================
   Rotate Block(s) at its insertion point(s) by specifying rotation angle
=========================================================================|;
(vl-load-com)
(defun C:RMB (/ CN ENT OS RA SS)
  (setq SS (ssget (list (cons 0 "INSERT"))))
  (if *RA (setq RA (getdist (strcat "Specify rotation angle <" (angtos *RA) ">: ")))
    (progn
      (initget 1)
      (setq RA (getdist "\nSpecify rotation angle: "))
    ); progn
  ); if
  (if (not RA)
    (setq RA *RA)
  ); if
  (setq *RA RA)
  (repeat (setq CN (sslength SS))
    (setq CN (1- CN)
	  ENT (ssname SS CN)
	  OS (vlax-ename->vla-object ENT)
    ); setq
    (vla-put-rotation OS (+ (* (/ RA 180) pi)
			    (vla-get-rotation OS)
			    )
    )
  ); repeat
  (princ)
); defun C:RMB