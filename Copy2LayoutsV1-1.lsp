;;-----------------------=={ Copy to Layouts }==------------------------;;
;;                                                                      ;;
;;  This program enables a user to quickly copy a selection of objects  ;;
;;  to all or selected layouts in a drawing.                            ;;
;;                                                                      ;;
;;  Upon calling the program with 'C2L' at the command line, the user   ;;
;;  is prompted to make a selection of objects in the active layout to  ;;
;;  copy. Following selection, the user is prompted, via a dialog       ;;
;;  interface, to choose one or more layouts to which the objects will  ;;
;;  be copied.                                                          ;;
;;                                                                      ;;
;;  If the program is called with 'C2AL' at the command line, every     ;;
;;  object in the selection is automatically copied to all layouts      ;;
;;  in the drawing, without the use of the dialog interface.            ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    10-02-2011                                      ;;
;;                                                                      ;;
;;  First Release.                                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    08-07-2013                                      ;;
;;                                                                      ;;
;;  Entire program rewritten.                                           ;;
;;----------------------------------------------------------------------;;

(defun c:c2l  nil (copy2layouts nil))  ;; Copy to selected layouts
(defun c:c2al nil (copy2layouts   t))  ;; Copy to all layouts

(defun copy2layouts ( all / lst obl sel tab )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (= 1 (getvar 'cvport))
        (setq tab (strcase (getvar 'ctab)))
        (setq tab "MODEL")
    )
    (if (ssget (list (cons 410 tab)))
        (progn
            (vlax-for lay (vla-get-layouts (LM:acdoc))
                (if (/= tab (strcase (vla-get-name lay)))
                    (setq lst (cons (cons (vla-get-name lay) lay) lst))
                )
            )
            (if (or (and all (setq lst (mapcar 'cdr lst)))
                    (setq lst
                        (mapcar '(lambda ( x ) (cdr (assoc x lst)))
                            (LM:listbox "Choose Layouts to Copy to"
                                (mapcar 'car (vl-sort lst '(lambda ( a b ) (< (vla-get-taborder (cdr a)) (vla-get-taborder (cdr b))))))
                                t
                            )
                        )
                    )
                )
                (progn
                    (vlax-for obj (setq sel (vla-get-activeselectionset (LM:acdoc)))
                        (setq obl (cons obj obl))
                    )
                    (vla-delete sel)
                    (LM:startundo (LM:acdoc))
                    (foreach lay lst (vlax-invoke (LM:acdoc) 'copyobjects obl (vla-get-block lay)))
                    (LM:endundo   (LM:acdoc))
                )
            )
        )
    )
    (princ)
)

;;-----------------------=={ List Box }==---------------------;;
;;                                                            ;;
;;  Displays a List Box allowing the user to make a selection ;;
;;  from the supplied data.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  title    - List Box Dialog title                          ;;
;;  lst      - List of Strings to display in the List Box     ;;
;;  multiple - Boolean flag to determine whether the user     ;;
;;             may select multiple items (T=Allow Multiple)   ;;
;;------------------------------------------------------------;;
;;  Returns:  List of selected items, else nil.               ;;
;;------------------------------------------------------------;;

(defun LM:ListBox ( title lst multiple / dch des tmp res )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat
                            "listbox : dialog { label = \""
                            title
                            "\"; spacer; : list_box { key = \"list\"; multiple_select = "
                            (if multiple "true" "false")
                            "; } spacer; ok_cancel; }"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError Loading List Box Dialog.")
        )
        (   t     
            (start_list "list")
            (foreach item lst (add_list item))
            (end_list)
            (setq res (set_tile "list" "0"))
            (action_tile "list" "(setq res $value)")
            (setq res
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" res ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    (if (and tmp (setq tmp (findfile tmp)))
        (vl-file-delete tmp)
    )
    res
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
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Copy2Layouts.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Available Commands:"
        "\n::    \"C2L\"   -  Copy to selected layouts."
        "\n::    \"C2AL\"  -  Copy to all layouts."
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;