; Define the command CHGATTRIB
(defun c:CHGATTRIB ()
; Ask the user to enter the name of the block
(setq blkname (getstring "\nEnter the name of the block: "))
; Ask the user to enter the tag of the attribute
(setq atttag (getstring "\nEnter the tag of the attribute: "))
; Ask the user to enter the new content for the attribute
(setq newcontent (getstring "\nEnter the new content: "))
; Search for the block in the drawing using its name
(setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blkname))))
; Check if the block is found
(if (not ss)
(princ "\nBlock not found.")
(progn
; Get the entity name of the block
(setq ent (ssname ss 0))
; Find the attribute in the block with the given tag
(setq attvals (mapcar 'cdr (vl-remove-if-not '(lambda (x) (equal (cdr (assoc 2 x)) atttag)) (entget ent))))
; Check if the attribute is found
(if (not (null attvals))
(progn
; Get the attribute value
(setq attval (car attvals))
; Replace the attribute value with the new content
(setq attval (subst newcontent (cdr attval) attval))
; Add the attribute value back into the entity list with its code 1
(setq attval (cons 1 attval))
; Replace the old attribute with the new one in the entity list of the block
(setq ent (subst attval (assoc 1 (entget ent)) (entget ent)))
; Update the block with the modified entity list
(entmod ent)
; Inform the user that the attribute was updated successfully
(princ "\nAttribute updated successfully.")
)
; Inform the user that the attribute was not found
(princ "\nAttribute not found.")
)
)
)
; Return control to the AutoCAD command line
(princ)
)
