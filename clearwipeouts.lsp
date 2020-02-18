(vl-load-com)
(defun c:ClearWipeouts (/ b o)
 ;;--- Tharwat 26.June.2013 ---;;  
 (or doc
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
 )
 (vlax-for b
             (vla-get-blocks
               doc
             )
   (if
     (and
       (eq :vlax-false (vla-get-isLayout b))
       (eq :vlax-false (vla-get-isXref b))
     )
      (vlax-for o b
        (if (eq "AcDbWipeout" (vla-get-objectname o))
          (vl-catch-all-apply 'vla-delete (list o))
        )
      )
   )
 )

(if (setq ss (ssget "_X" '((0 . "WIPEOUT")(410 . "Model"))))
 (command "_.erase" ss "")
 )
 (vla-regen doc acAllViewports)
 (princ)
)