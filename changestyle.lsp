; This routine changes the style of all text and Mtext entities in the drawing to "iso"
; To run the routine, type "CHANGESTYLE" at the command line
defun C:CHANGESTYLE (/ entities len count ent ent_data ent_name new_style_name)
; Set the default text style to "iso" with "isocp.shx" font
(command "STYLE" "iso" "isocp.shx" "" "" "" "" "")
; Change the style of all TEXT entities to "iso"
(setq entities (ssget "X" '((0 . "TEXT")))
     len      (sslength entities)
     count 0
);setq 

(while (< count len)
      (setq ent      (ssname entities count) 
            ent_data (entget ent)
            ent_name (cdr (assoc 7 ent_data))
      );setq

(setq new_style_name (cons 7 "iso"))
(setq ent_data (subst new_style_name (assoc 7 ent_data) ent_data))
(entmod ent_data)

(setq count (+ count 1))
);while

;;;runs same routine again, picking up Mtext this time.
; Change the style of all MTEXT entities to "iso"
(setq entities (ssget "X" '((0 . "MTEXT")))
     len      (sslength entities)
     count 0
);setq 

(while (< count len)
      (setq ent      (ssname entities count) 
            ent_data (entget ent)
            ent_name (cdr (assoc 7 ent_data))
      );setq

(setq new_style_name (cons 7 "iso"))
(setq ent_data (subst new_style_name (assoc 7 ent_data) ent_data))
(entmod ent_data)

(setq count (+ count 1))
);while

(princ)

);defun
