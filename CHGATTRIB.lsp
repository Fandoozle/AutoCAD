(defun c:CHGATTRIB2 (/ blkname atttag ss ent attvals newatts)
  ; Prompt user for block name, attribute tag, and new attribute values
  (setq blkname (getstring "\nEnter the name of the block: "))
  (setq atttag (getstring "\nEnter the tag of the attribute: "))
  (setq newatts (getstring "\nEnter new attribute values: "))

  ; Get the selection set of blocks with matching names
  (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blkname))))

  ; If no matching blocks found, display error message and exit
  (if (not ss)
    (princ "\nBlock not found.")
    ; Otherwise, loop through each block and update the attribute information
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq attvals (mapcar 'cdr (vl-remove-if-not '(lambda (x) (equal (cdr (assoc 2 x)) atttag)) (entget ent))))
        ; If the attribute is found, update its value
        (if (not (null attvals))
          (progn
            (setq attval (car attvals))
            (setq attval (subst newatts (cdr attval) attval))
            (setq attval (cons 1 attval))
            (setq ent (subst attval (assoc 1 (entget ent)) (entget ent)))
            (entmod ent)
            (princ "\nAttribute updated successfully.")
          )
          ; If the attribute is not found, display error message
          (princ "\nAttribute not found.")
        )
        (setq i (1+ i))
      )
    )
  )
  (princ)
)
