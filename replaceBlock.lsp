(vl-load-com)

(defun c:RB () (c:ReplaceBlock))
(defun c:ReplaceBlock (/ *error* blockName ok acDoc space ss oBlock)

  (defun *error* (msg)
    (if ss (vla-delete ss))
    (if acDoc
      (vla-endundomark acDoc)
    )
    (cond ((not msg))                                                   ; Normal exit
          ((member msg '("Function cancelled" "quit / exit abort")))    ; <esc> or (quit)
          ((princ (strcat "\n** Error: " msg " ** ")))                  ; Fatal error, display it
    )
    (princ)
  )

  (if
    (and
      (ssget "_:L" '((0 . "INSERT")))
      (or (/= ""
              (setq blockName
                     (strcase
                       (getstring
                         T
                         (strcat "\nEnter replacement block name"
                                 (if *ReplaceBlockName*
                                   (strcat " <" *ReplaceBlockName* ">: ")
                                   ": "
                                 )
                         )
                       )
                     )
              )
          )
          (setq blockName *ReplaceBlockName*)
      )
      (setq *ReplaceBlockName* blockName)
      (or (and (tblsearch "block" blockName) (setq ok T))
          (setq blockName (findfile (strcat blockName ".dwg")))
      )
    )
     (progn
       (vla-startundomark
         (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
       )
       (setq space (vlax-get acDoc
                             (if (= 1 (getvar 'cvport))
                               'paperspace
                               'modelspace
                             )
                   )
       )
       (vlax-for x (setq ss (vla-get-activeselectionset acDoc))
         (vla-put-layer
           (setq oBlock (vla-insertblock
                          space
                          (vla-get-insertionpoint x)
                          blockName
                          (vla-get-xscalefactor x)
                          (vla-get-yscalefactor x)
                          (vla-get-zscalefactor x)
                          (vla-get-rotation x)
                        )
           )
           (vla-get-layer x)
         )
         (vla-put-color oBlock (vla-get-color x))
         (vla-delete x)
         (if (not ok)
           (progn
             (setq blockName (vl-filename-base blockName))
             (setq ok T)
           )
         )
       )
     )
  )
  (*error* nil)
)