; Define a command named C:BLKTO0 with local variables idx, lst, and sel
(defun c:blkto0 ( / idx lst sel )
    ; If a selection set of blocks is obtained
    (if (setq sel (ssget '((0 . "INSERT"))))
        ; Repeat for each block in the selection set
        (repeat (setq idx (sslength sel))
            ; Call the BLOCK->0 command with the block name as argument
            (block->0 (cdr (assoc 2 (entget (ssname sel (setq idx (1- idx)))))))
        )
    )
    ; Regenerate the drawing
    (command "_.regen")
    ; Exit quietly
    (princ)
)

; Define a command named BLOCK->0 with local variables blk, ent, and enx
(defun block->0 ( blk / ent enx )
    ; Check two conditions
    (cond
        ; If the block name is not in the list
        (   (member blk lst))
        ; Then do the following
        (   ; Get the block definition entity from the block name
            (setq ent (tblobjname "block" blk))
            ; While there is another entity in the block definition
            (while (setq ent (entnext ent))
                ; Modify the entity to change its layer and color to 0
                (entmod (subst-append 8 "0" (subst-append 62 256 (setq enx (entget ent)))))
                ; If the entity is a nested block
                (if (= "INSERT" (cdr (assoc 0 enx)))
                    ; Then call the BLOCK->0 command with the nested block name as argument
                    (block->0 (cdr (assoc 2 enx)))
                )
            )
            ; Add the block name to the list
            (setq lst (cons blk lst))
        )
    )
)

; Define a helper function named SUBST-APPEND with local variables key, val, lst, and itm
(defun subst-append ( key val lst / itm )
    ; If the key is found in the list
    (if (setq itm (assoc key lst))
        ; Then replace the value with the new value
        (subst (cons key val) itm lst)
        ; Else append the key-value pair to the list
        (append lst (list (cons key val)))
    )
)