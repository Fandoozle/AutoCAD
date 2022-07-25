;;;  LayerAll.lsp
;;;  To perform operations On All Layers
(defun C:LAA (); Layers All All [Thawed, Unlocked, On]
  (command "_.layer" "_thaw" "*" "_unlock" "*" "_on" "*" "" "_.regen")
  (princ)
)
(defun C:LAON (); Layers All [Thawed and] On [not Unlocked]
  (command "_.layer" "_thaw" "*" "_on" "*" "" "_.regen")
  (princ)
)
(defun C:LAOF (); Layers All Off [except current]
  (command "_.layer" "_off" "*" "_no" "")
  (princ)
)
(defun C:LAF (); Layers All Frozen [except current]
  (command "_.layer" "_freeze" "*" "_no" "")
  (princ)
)