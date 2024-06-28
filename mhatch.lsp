```
This function, `c:mhatch`, creates a hatch pattern for selected entities in AutoCAD. It first checks if the selected entities are either a LWPOLYLINE, POLYLINE, CIRCLE, or ELLIPSE. If the entity is a closed polyline, a circle, or a closed curve, it creates a hatch using the current system variables for hatch pattern name, scale, and angle. The hatch is created in either the model space or paper space depending on the current viewport. The function then prints a newline character to the command line. This function is a great example of how to use AutoLISP to automate tasks in AutoCAD.

```

(defun c:mhatch (/ ang do-it doc hatch oname pname scl space ss)
  ; Check if the user has selected any entities of type LWPOLYLINE, POLYLINE, CIRCLE, or ELLIPSE
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
    (progn
      ; Get the current system variables for hatch pattern scale, angle, and name
      (setq scl (getvar "hpscale")  ; Hatch pattern scale
            ang (getvar "hpang")    ; Hatch pattern angle
            pname (getvar "hpname") ; Hatch pattern name
            ; Check if hatch patterns are associative
            hpassoc (if (= (getvar "hpassoc") 1)
                        :vlax-true
                        :vlax-false)
            ; Get the active document
            doc (vla-get-activedocument
                 (vlax-get-acad-object))
            ; Check if the current viewport is paper space or model space
            space (if (= (getvar "cvport") 1)
                      (vla-get-paperspace doc)
                      (vla-get-modelspace doc)
                      )
            )
      ; Loop through each entity in the active selection set
      (vlax-for ent (vla-get-activeselectionset doc)
        (setq do-it nil
              ; Get the name of the entity
              oname (strcase (vla-get-objectname ent)))
        ; Check the type and properties of the entity
        (cond ((vl-string-search "CIRCLE" oname) ; If the entity is a circle
               (setq do-it t)
               )
              ((and (vl-string-search "LINE" oname) ; If the entity is a closed polyline
                    (eq (vla-get-closed ent) :vlax-true)
                    )
               (setq do-it t)
               )
              ((equal (vlax-curve-getstartpoint ent) ; If the entity is a closed curve
                      (vlax-curve-getendpoint ent)
                      1e-6)
               (setq do-it t)
               )
              )
        ; If the entity meets the conditions, create a hatch
        (if do-it
            (progn
              ; Create a new hatch in the current space
              (setq hatch (vlax-invoke space 'addhatch acHatchObject pname hpassoc))
              ; Add the entity to the hatch
              (vlax-invoke hatch 'appendouterloop (list ent))
              ; Set the hatch pattern angle and scale
              (vlax-put hatch 'patternangle ang)
              (vlax-put hatch 'patternscale scl)
              ; Evaluate the hatch to display it
              (vla-evaluate hatch)
              )
            )
        )
      )
    )
  ; Print a newline character to the command line
  (princ)
  )