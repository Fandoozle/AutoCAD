;;-------------------------=={ Cursor Rotate }==------------------------;;
;;                                                                      ;;
;;  This program allows the user to rotate the AutoCAD crosshairs       ;;
;;  (that is, modifying the SNAPANG system variable) to align with an   ;;
;;  object residing at a selected point, to a fixed angle, or a fixed   ;;
;;  percentage representing a slope or incline grade.                   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-09-25                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:cr ( / a e f l o u v x z )
    (setq z (trans '(0 0 1) 1 0 t)
          x (angle '(0 0) (trans (getvar 'ucsxdir) 0 z t))
    )
    (while
        (cond
            (   (or (null u) (= "Object" u))
                (initget "Angle Grade Cancel")
                (setq u (cond ((getpoint "\nPick point on object [Angle/Grade/Cancel] <Angle>: ")) ("Angle")))
                (cond
                    (   (= 'str (type u)))
                    (   (null (setq l (nentselp u)))
                        (princ "\nPoint does not lie on an object.")
                    )
                    (   (or (and (setq e (car l)
                                       o (vlax-ename->vla-object e)
                                 )
                                 (vlax-property-available-p o 'rotation)
                                 (setq a (vla-get-rotation o))
                            )
                            (and (not (vl-catch-all-error-p (setq u (vl-catch-all-apply 'vlax-curve-getclosestpointto (list e (trans u 1 0))))))
                                 (setq a (angle '(0 0) (trans (vlax-curve-getfirstderiv e (vlax-curve-getparamatpoint e u)) 0 z)))
                            )
                        )
                        (if (caddr l)
                            (setq f (lambda ( x ) (reverse (cdr (reverse x))))
                                  v (list (cos a) (sin a) 0.0)
                                  v (mapcar '(lambda ( x ) (apply '+ (mapcar '* (f x) v))) (f (caddr l)))
                                  a (angle '(0 0) (trans v 0 z))
                            )
                        )
                        (not (setvar 'snapang (- a x)))
                    )
                    (   (princ "\nIncompatible object selected."))
                )
            )
            (   (= "Angle" u)
                (initget "Object Grade Cancel")
                (if (numberp (setq u (getangle "\nSpecify angle [Object/Grade/Cancel] <Object>: ")))
                    (not (setvar 'snapang u))
                    t
                )
            )
            (   (= "Grade" u)
                (initget "Object Angle Cancel")
                (if (numberp (setq u (getreal "\nSpecify grade (%) [Object/Angle/Cancel] <Object>: ")))
                    (not (setvar 'snapang (atan (/ u 100.0))))
                    t
                )
            )
            (   (= "Cancel" u) nil)
        )
    )
    (princ)
)
(vl-load-com) (princ)