(DEFUN C:DelLayouts (/ layouts)
(vl-load-com)
(setq layouts
(vla-get-layouts
(vla-get-activedocument (vlax-get-acad-object))
)
)
(mapcar '(lambda (layout)
(vla-delete (vla-item layouts layout))
)
(layoutlist)
)
(princ)
)