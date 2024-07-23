;;-----------------------=={ Outline Objects  }==-----------------------;;
;;                                                                      ;;
;;  This program enables the user to generate one or more closed        ;;
;;  polylines or regions outlining all objects in a selection.          ;;
;;                                                                      ;;
;;  Following a valid selection, the program calculates the overall     ;;
;;  rectangular extents of all selected objects and constructs a        ;;
;;  temporary rectangular polyline offset outside of such extents.      ;;
;;                                                                      ;;
;;  Using a point located within the offset margin between the extents  ;;
;;  of the selection and temporary rectangular frame, the program then  ;;
;;  leverages the standard AutoCAD BOUNDARY command to construct        ;;
;;  polylines and/or regions surrounding all 'islands' within the       ;;
;;  temporary bounding frame.                                           ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-11-30                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-01-23                                      ;;
;;                                                                      ;;
;;  - Added option to erase original objects.                           ;;
;;----------------------------------------------------------------------;;

(defun c:outline ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if (setq sel (ssget))
        (progn
            (LM:startundo (LM:acdoc))
            (LM:outline sel)
            (initget "Yes No")
            (if (/= "No" (getkword "\nErase original objects? [Yes/No] <Yes>: "))
                (repeat  (setq idx (sslength sel))
                    (entdel (ssname sel (setq idx (1- idx))))
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Outline Objects  -  Lee Mac
;; Attempts to generate a polyline outlining the selected objects.
;; sel - [sel] Selection Set to outline
;; Returns: [sel] A selection set of all objects created

(defun LM:outline ( sel / app are box cmd dis enl ent lst obj rtn tmp )
    (if (setq box (LM:ssboundingbox sel))
        (progn
            (setq app (vlax-get-acad-object)
                  dis (/ (apply 'distance box) 20.0)
                  lst (mapcar '(lambda ( a o ) (mapcar o a (list dis dis))) box '(- +))
                  are (apply '* (apply 'mapcar (cons '- (reverse lst))))
                  dis (* dis 1.5)
                  ent
                (entmakex
                    (append
                       '(   (000 . "LWPOLYLINE")
                            (100 . "AcDbEntity")
                            (100 . "AcDbPolyline")
                            (090 . 4)
                            (070 . 1)
                        )
                        (mapcar '(lambda ( x ) (cons 10 (mapcar '(lambda ( y ) ((eval y) lst)) x)))
                           '(   (caar   cadar)
                                (caadr  cadar)
                                (caadr cadadr)
                                (caar  cadadr)
                            )
                        )
                    )
                )
            )
            (apply 'vlax-invoke
                (vl-list* app 'zoomwindow
                    (mapcar '(lambda ( a o ) (mapcar o a (list dis dis 0.0))) box '(- +))
                )
            )
            (setq cmd (getvar 'cmdecho)
                  enl (entlast)
                  rtn (ssadd)
            )
            (while (setq tmp (entnext enl)) (setq enl tmp))
            (setvar 'cmdecho 0)
            (command
                "_.-boundary" "_a" "_b" "_n" sel ent "" "_i" "_y" "_o" "_p" "" "_non"
                (trans (mapcar '- (car box) (list (/ dis 3.0) (/ dis 3.0))) 0 1) ""
            )
            (while (< 0 (getvar 'cmdactive)) (command ""))
            (entdel ent)
            (while (setq enl (entnext enl))
                (if (and (vlax-property-available-p (setq obj (vlax-ename->vla-object enl)) 'area)
                         (equal (vla-get-area obj) are 1e-4)
                    )
                    (entdel enl)
                    (ssadd  enl rtn)
                )
            )
            (vla-zoomprevious app)
            (setvar 'cmdecho cmd)
            rtn
        )
    )
)

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; s - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( s / a b i m n o )
    (repeat (setq i (sslength s))
        (if
            (and
                (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
                (vlax-method-applicable-p o 'getboundingbox)
                (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list o 'a 'b))))
            )
            (setq m (cons (vlax-safearray->list a) m)
                  n (cons (vlax-safearray->list b) n)
            )
        )
    )
    (if (and m n)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list m n))
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;