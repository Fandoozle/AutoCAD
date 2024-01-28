; This function deletes all layouts except the Model and Layout1 tabs
(DEFUN C:DelLayouts (/ layouts) ; Define a function with the name DelLayouts and a local variable layouts
(vl-load-com) ; Load the Visual LISP COM support
(setq layouts ; Assign the layouts variable
(vla-get-layouts ; Get the layouts collection object
(vla-get-activedocument (vlax-get-acad-object)) ; Get the active document object
)
)
(mapcar '(lambda (layout) ; Apply a function to each element of a list
(vla-delete (vla-item layouts layout)) ; Delete the layout object by name
)
(layoutlist) ; Get the list of layout names
)
(princ) ; Exit the function quietly
)
