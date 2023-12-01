; This routine changes the text color of multiple leaders
; Filename: MLeaderTextColor.lsp
; Author: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/multi-leader-object-text-color/m-p/6707419#M347290

(vl-load-com) ; Load the Visual LISP extensions

(defun c:MLeaderTextColor ( / col ss obj txt x y)
  
  (if (and (setq col (getint "\nText color number: ")) ; Prompt for the color number
           (setq ss (ssget '((0 . "MULTILEADER")))) ; Select the multileaders
           )
    (repeat (setq i (sslength ss)) ; Loop through the selection set
      (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))) ; Get the multileader object
            txt (vla-get-TextString obj) ; Get the text string
            txt (vl-string-trim "{}" txt)) ; Remove the curly braces
      (foreach e '("\\C" "\\c") ; For each color escape sequence
        (while (and (setq x (vl-string-search e txt)) ; Find the start of the sequence
                    (setq y (vl-string-search ";" txt x))) ; Find the end of the sequence
          (setq txt (strcat (substr txt 1 x) ; Concatenate the text without the sequence
                            (substr txt (+ 2 y))))))
      (vla-put-TextString obj (strcat "{\\C" (itoa col) ";" txt "}")) ; Set the text string with the new color
      ))
  (princ) ; Exit quietly
)
