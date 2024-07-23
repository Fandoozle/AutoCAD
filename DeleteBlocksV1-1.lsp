;;-------------------------=={ Delete Blocks }==------------------------;;
;;                                                                      ;;
;;  This program enables the user to quickly remove multiple blocks     ;;
;;  from a drawing.                                                     ;;
;;                                                                      ;;
;;  After invoking the program with the command 'delblocks' at the      ;;
;;  AutoCAD command-line, the user is prompted to select a block to     ;;
;;  delete. At this prompt, the user may either select a block          ;;
;;  reference from the drawing, type 'N' to choose a block by name, or  ;;
;;  exit the program by typing 'E' or by dismissing the prompt.         ;;
;;                                                                      ;;
;;  By opting to choose a block by name, the user is presented with a   ;;
;;  simple dialog interface consisting of a list box and filter. The    ;;
;;  dialog provides a list of all standard & dynamic blocks defined     ;;
;;  in the active drawing, excluding anonymous blocks and xrefs. From   ;;
;;  this list, the user may select multiple blocks to be deleted from   ;;
;;  the active drawing.                                                 ;;
;;                                                                      ;;
;;  Upon the user selecting a block from the drawing or selecting one   ;;
;;  or more block names from the dialog, the program will delete all    ;;
;;  primary & nested references of the blocks from all layouts of the   ;;
;;  active drawing, and will proceed to delete the respective block     ;;
;;  definitions, if possible.                                           ;;
;;                                                                      ;;
;;  The user will be informed at the command-line of those blocks       ;;
;;  successfully deleted by the program, and those blocks that the      ;;
;;  program was unable to remove.                                       ;;
;;                                                                      ;;
;;  The program also defines the function 'LM:deleteblocks' so that     ;;
;;  users who wish to remove multiple blocks from several drawings      ;;
;;  in a batch processing operation may bypass the dialog and call      ;;
;;  this function with a VLA Document Object and a list of block names  ;;
;;  (case-insensitive) from a Script or another LISP application.       ;;
;;  This function is also compatible with ObjectDBX.                    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-11-05                                      ;;
;;                                                                      ;;
;;  - Program modified to prompt user for selection of a block to       ;;
;;    delete, with the option of displaying a list of all blocks        ;;
;;    defined in the drawing.                                           ;;
;;----------------------------------------------------------------------;;

(defun c:delblocks ( / *error* def lst sel tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while (setq def (tblnext "block" (not def)))
        (if (zerop (logand 125 (cdr (assoc 70 def))))
            (setq lst (cons (cdr (assoc 2 def)) lst))
        )
    )
    (setq lst (vl-sort lst '<))
    
    (while
        (and (or lst (prompt "\nNo blocks defined in the current drawing."))
            (progn
                (setvar 'errno 0)
                (initget "Name Exit")
                (setq sel (entsel "\nSelect block to delete [Name/Exit] <Exit>: "))
                (cond
                    (   (= 7 (getvar 'errno))
                        (princ "\nMissed, try again.")
                    )
                    (   (or (null sel) (= "Exit" sel))
                        nil
                    )
                    (   (= "Name" sel)
                        (if (setq tmp (LM:filtlistbox "Select Blocks to Delete" lst t))
                            (setq tmp (LM:deleteblocks:acdoc tmp)
                                  lst (vl-remove-if '(lambda ( x ) (member (strcase x) tmp)) lst)
                            )
                        )
                        nil
                    )
                    (   (/= "INSERT" (cdr (assoc 0 (entget (car sel)))))
                        (princ "\nThe selected object is not a block.")
                    )
                    (   t
                        (setq tmp (LM:deleteblocks:acdoc (list (LM:blockname (vlax-ename->vla-object (car sel)))))
                              lst (vl-remove-if '(lambda ( x ) (member (strcase x) tmp)) lst)
                        )
                        t
                    )
                )
            )
        )
    )
    (princ)
)

(defun LM:deleteblocks:acdoc ( del / rtn )
    (LM:startundo (LM:acdoc))
    (setq rtn (LM:deleteblocks (LM:acdoc) del))
    (vla-regen (LM:acdoc) acallviewports)
    (foreach blk del
        (if (member (strcase blk) rtn)
            (princ (strcat "\nDeleted block " blk "."))
            (princ (strcat "\nUnable to delete block " blk "."))
        )
    )
    (LM:endundo (LM:acdoc))
    rtn
)
        
;; Delete Blocks  -  Lee Mac
;; Deletes all references of a list of blocks from a drawing (including nested references, nested to any level).
;; Proceeds to delete the associated block definitions from the drawing, if possible.
;; doc - [vla] Document object
;; lst - [lst] List of block names (case insensitive)
;; Returns: [lst] List of blocks that were successfully deleted.

(defun LM:deleteblocks ( doc lst / blc lck )
    (setq blc (vla-get-blocks doc))
    (if (setq lst (mapcar 'strcase (vl-remove-if-not '(lambda ( blk ) (LM:catchapply 'vla-item (list blc blk))) lst)))
        (progn
            (vlax-for lay (vla-get-layers doc)
                (if (= :vlax-true (vla-get-lock lay))
                    (progn
                        (setq lck (cons lay lck))
                        (vla-put-lock lay :vlax-false)
                    )
                )
            )
            (vlax-for def blc
                (vlax-for obj def
                    (if (and (= "AcDbBlockReference" (vla-get-objectname obj))
                             (member (strcase (LM:blockname obj)) lst)
                        )
                        (vl-catch-all-apply 'vla-delete (list obj))
                    )
                )
            )
            (setq lst (vl-remove-if-not '(lambda ( blk ) (LM:catchapply 'vla-delete (list (vla-item blc blk)))) lst))
            (foreach lay lck (vla-put-lock lay :vlax-true))
            lst
        )
    )
)

;; Catch Apply  -  Lee Mac
;; Applies a function to a list of parameters and catches any exceptions.
 
(defun LM:catchapply ( fnc prm / rtn )
    (if (not (vl-catch-all-error-p (setq rtn (vl-catch-all-apply fnc prm))))
        (cond ( rtn ) ( t ))
    )
)

;; Filtered List Box  -  Lee Mac
;; Displays a list box interface from which the user may select one or more items.
;; Includes an edit box filter to enable the user to filter the displayed list of items.
;; msg - [str] List box dialog title
;; lst - [lst] List of strings to display in the list box
;; mtp - [bol] T=Allow multiple items; nil=Single item selection
;; Returns: [lst] List of selected items, else nil

(defun LM:filtlistbox ( msg lst mtp / _addlist dch dcl des rtn sel tmp )

    (defun _addlist ( key lst )
        (start_list key)
        (foreach x lst (add_list x))
        (end_list)
        lst
    )

    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (write-line
                (strcat
                    "filtlistbox : dialog { label = \"" msg "\"; spacer;"
                    ": list_box { key = \"lst\"; width = 50; fixed_width = true; height = 15; fixed_height = true; allow_accept = true; "
                    "multiple_select = " (if mtp "true" "false") "; }"
                    ": edit_box { key = \"flt\"; width = 50; fixed_width = true; label = \"Filter:\"; }"
                    "spacer; ok_cancel; }"
                )
                des
            )
            (not (close des))
            (< 0 (setq dch (load_dialog dcl)))
            (new_dialog "filtlistbox" dch)
        )
        (progn
            (_addlist "lst" (setq tmp lst))
            (set_tile "lst" (setq rtn "0"))
            (set_tile "flt" "*")
            (action_tile "lst" "(setq rtn $value)")
            (action_tile "flt"
                (vl-prin1-to-string
                   '(progn
                        (setq flt (strcat "*" (strcase $value) "*")
                              sel (mapcar '(lambda ( n ) (nth n tmp)) (read (strcat "(" rtn ")")))
                        )
                        (_addlist "lst" (setq tmp (vl-remove-if-not '(lambda ( x ) (wcmatch (strcase x) flt)) lst)))
                        (set_tile "lst"
                            (setq rtn
                                (vl-string-trim "()"
                                    (vl-princ-to-string
                                        (cond
                                            (   (vl-sort (vl-remove nil (mapcar '(lambda ( x ) (vl-position x tmp)) sel)) '<))
                                            (  '(0)   )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (setq rtn
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x tmp)) (read (strcat "(" rtn ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (setq dch (unload_dialog dch))
    )
    (if (and (= 'str (type dcl)) (findfile dcl))
        (vl-file-delete dcl)
    )
    rtn
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns a global pointer to the VLA Active Document Object

(defun LM:acdoc nil
    (cond ( acdoc ) ((setq acdoc (vla-get-activedocument (vlax-get-acad-object)))))
)

;;----------------------------------------------------------------------;;

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;