(vl-load-com) ; Load the Visual LISP extensions
(defun c:ClearWipeouts (/ b o) ; Define a function named ClearWipeouts with two local variables b and o
 ;;--- Tharwat 26.June.2013 ---;; ; A comment indicating the author and date of the function
 (or doc ; If doc is nil, then
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) ; Set doc to the active document object
 )
 (vlax-for b ; For each block b
             (vla-get-blocks ; Get the blocks collection
               doc ; From the document object
             )
   (if ; If
     (and ; Both
       (eq :vlax-false (vla-get-isLayout b)) ; The block is not a layout
       (eq :vlax-false (vla-get-isXref b)) ; And the block is not an external reference
     )
      (vlax-for o b ; For each object o in the block b
        (if (eq "AcDbWipeout" (vla-get-objectname o)) ; If the object is a wipeout
          (vl-catch-all-apply 'vla-delete (list o)) ; Delete the object using error handling
        )
      )
   )
 )

(if (setq ss (ssget "_X" '((0 . "WIPEOUT")(410 . "Model")))) ; If a selection set of wipeouts in the model space is obtained
 (command "_.erase" ss "") ; Erase the selection set
 )
 (vla-regen doc acAllViewports) ; Regenerate the document in all viewports
 (princ) ; Exit the function quietly
)
