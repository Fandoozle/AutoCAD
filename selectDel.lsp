;; SelectDelete
;; pre select an element on a drawing
;; run the command to select similar elements (ie selectsimilar) and deletes the selections
;; helps with tedious tasks
(DEFUN C:SD ()
 (command "selectsimilar") ;; select similar elements
 (command "erase") ;; erase selection
) ;;END DEFUN
