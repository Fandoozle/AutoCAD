; Load the Visual LISP COM support
(vl-load-com)

; Define a function RB that calls another function ReplaceBlock
(defun c:RB () (c:ReplaceBlock))

; Define a function ReplaceBlock that takes no arguments
(defun c:ReplaceBlock (/ *error* blockName ok acDoc space ss oBlock)

  ; Define an error handler function that takes a message argument
  (defun *error* (msg)
    (if ss (vla-delete ss)) ; If the selection set exists, delete it
    (if acDoc
      (vla-endundomark acDoc) ; If the active document exists, end the undo mark
    )
    (cond ((not msg))                                                   ; Normal exit
          ((member msg '("Function cancelled" "quit / exit abort")))    ; <esc> or (quit)
          ((princ (strcat "\n** Error: " msg " ** ")))                  ; Fatal error, display it
    )
    (princ) ; Exit the function quietly
  )

  ; If the following conditions are met
  (if
    (and
      (ssget "_:L" '((0 . "INSERT"))) ; Select all the block references in the drawing
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
          (setq blockName *ReplaceBlockName*) ; Use the previous block name if any
      )
      (setq *ReplaceBlockName* blockName) ; Store the block name in a global variable
      (or (and (tblsearch "block" blockName) (setq ok T)) ; Check if the block name exists in the block table
          (setq blockName (findfile (strcat blockName ".dwg"))) ; Or find the block name as a dwg file
      )
    )
     (progn ; Begin a series of expressions
       (vla-startundomark
         (setq acDoc (vla-get-activedocument (vlax-get-acad-object))) ; Get the active document object and start an undo mark
       )
       (setq space (vlax-get acDoc
                             (if (= 1 (getvar 'cvport)) ; Check if the current viewport is paper space
                               'paperspace ; Get the paper space object
                               'modelspace ; Get the model space object
                             )
                   )
       )
       (vlax-for x (setq ss (vla-get-activeselectionset acDoc)) ; Loop through each block reference in the selection set
         (vla-put-layer
           (setq oBlock (vla-insertblock ; Insert a new block reference
                          space ; In the current space
                          (vla-get-insertionpoint x) ; At the same insertion point as the original block
                          blockName ; With the replacement block name
                          (vla-get-xscalefactor x) ; With the same x scale factor as the original block
                          (vla-get-yscalefactor x) ; With the same y scale factor as the original block
                          (vla-get-zscalefactor x) ; With the same z scale factor as the original block
                          (vla-get-rotation x) ; With the same rotation as the original block
                        )
           )
           (vla-get-layer x) ; Set the layer of the new block to the same as the original block
         )
         (vla-put-color oBlock (vla-get-color x)) ; Set the color of the new block to the same as the original block
         (vla-delete x) ; Delete the original block
         (if (not ok) ; If the block name was not found in the block table
           (progn
             (setq blockName (vl-filename-base blockName)) ; Get the base name of the block file
             (setq ok T) ; Set the flag to true
           )
         )
       )
     )
  )
  (*error* nil) ; Call the error handler function with no message
)
