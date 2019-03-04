; Allows the exploding of all blocks in a drawing
; type expl-p to run
(defun c:expl-p ()
(vl-load-com)
(vlax-for b (vla-get-Blocks
(vla-get-ActiveDocument (vlax-get-acad-object))
)
(or (wcmatch (vla-get-Name b) "`**_Space*")
(vla-put-explodable b :vlax-true)
)
)
(princ)
)