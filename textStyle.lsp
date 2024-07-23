
;;; Changes objects that are set to one text style to another text style. Both styles need to be defined in the drawing.
;;; Posted by Peter
;;; http://forums.augi.com/showthread.php?22959-Help-Changing-text-style-in-blocks
;;;
;;; Use the folowing format in the command line after loading the routine:
;;; (changestyle "oldtextstylename" "newtextstylename")
;;; 
(defun TextStyle (strStyle1 strStyle2 / entItem objBlock objDocument objItem )
 (vl-load-com)
 (setq objDocument (vla-get-activedocument (vlax-get-acad-object)))
 (if (and (tblobjname "style" strStyle1)
          (tblobjname "style" strStyle2)         
     )
  (vlax-for objBlock (vla-get-blocks objDocument)
   (if (> (vla-get-count objBlock) 0)
    (progn
     (setq objItem (vla-item objBlock 0)
           entItem (vlax-vla-object->ename objItem)
     )
     (while entItem
      (if (and (vlax-property-available-p (setq objItem (vlax-ename->vla-object entItem)) "StyleName")
               (= (strcase (vla-get-stylename objItem)) (strcase strStyle1))
          )
       (vla-put-stylename objItem strStyle2)
      )
      (setq entItem (entnext entItem))
     )
    )
   )
  )
  (princ "\nError check if styles exist: ")
 )
 (vla-regen objDocument 0)
)
