(defun c:layfrz (/ LM:lst->str lst layers)
  ;; List to String - Lee Mac
  (defun LM:lst->str (lst del / str)
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
  )

  ;; Get a list of all layers in the active document
  (vlax-for obj (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (setq lst (cons (vla-get-name obj) lst))
  )

  ;; Convert the list of layer names to a comma-separated string
  (setq layers (LM:lst->str lst ","))

  ;; Freeze the layers using the VPLAYER command
  (command "_.vplayer" "_freeze" layers "_current" "")

  (princ) ; Print a newline character
)
