;***********************************************************
;**                                                       **
;** Purpose: This routine allows the user to explode all   **
;**          blocks in the current drawing, including      **
;**          those that are not set as explodable.         **
;**                                                       **
;** Usage: Type "expl-p" to run the routine.               **
;**                                                       **
;***********************************************************
;
; Load the ActiveX Automation Library to work with VLA objects
;
; Loop through all the blocks in the current drawing
; - Get the current drawing's active document using vlax-get-acad-object and vla-get-ActiveDocument
; - Get all the blocks in the current drawing using vla-get-Blocks and the previously obtained active document
; - Check if the block name matches the pattern for a space character or if it is not set as explodable
; - Set the block as explodable if it was not explodable before
;
; End the loop through all the blocks.
;
; Return control to the command line using the princ function.

; Allows the exploding of all blocks
; Type "expl-p" to run
(defun c:expl-p ()
  ; Load the ActiveX Automation Library to work with VLA objects
  (vl-load-com)
  
  ; Loop through all the blocks in the current drawing
  (vlax-for block (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    ; Check if the block name matches the pattern for a space character or if it is not set as explodable
    (or (wcmatch (vla-get-Name block) "`**_Space*")
        (vla-put-explodable block :vlax-true)
    )
  )
  
  ; Return control to the command line
  (princ)
)
