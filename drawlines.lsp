;;;; This creates a group of lines with names of all the layers

(defun c:drawlines ( / doc lyrs lst pt)
; Get the active document and all its layers
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
    lyrs (vla-get-layers doc)
);end_setq
; Create a list of all layer names and sort it alphabetically
(vlax-for lyr lyrs (setq lst (cons (vlax-get lyr 'name) lst)))
(setq lst (vl-sort lst '< )
  pt (getpoint "\nSelect insertion point: ")
);end_setq
; Loop through each layer name and create a line and text entity for it
(foreach lyr lst
  ; Create a line entity for the layer name
  (entmakex (list '(0 . "line") (cons 10 pt) (cons 11 (mapcar '+ pt '(2.5 0 0))) (cons 8 lyr)))
  ; Create a text entity for the layer name
  (entmakex (list '(0 . "text") (cons 10 (mapcar '+ pt '(3.0 0 0))) '(40 . 0.1) (cons 1 lyr) (cons 8 lyr)))
  ; Move the insertion point down for the next layer name
  (setq pt (mapcar '- pt '(0 0.15 0)))
 );end_foreach
 (princ)
);end_defun
