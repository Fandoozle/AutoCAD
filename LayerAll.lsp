;;; LayerAll.lsp
;;; This routine includes several commands to perform operations on all layers.

;;; The first command, "C:LAA", thaws, unlocks, and turns on all layers.
(defun C:LAA (); Layers All All [Thawed, Unlocked, On]
(command "_.layer" "_thaw" "" "_unlock" "" "on" "*" "" ".regen")
(princ)
)

;;; The second command, "C:LAON", thaws and turns on all layers that are not locked.
(defun C:LAON (); Layers All [Thawed and] On [not Unlocked]
(command "_.layer" "thaw" "" "_on" "" "" ".regen")
(princ)
)

;;; The third command, "C:LAOF", turns off all layers except the current layer.
(defun C:LAOF (); Layers All Off [except current]
(command "_.layer" "_off" "*" "_no" "")
(princ)
)

;;; The fourth command, "C:LAF", freezes all layers except the current layer.
(defun C:LAF (); Layers All Frozen [except current]
(command "_.layer" "_freeze" "*" "_no" "")
(princ)
)
