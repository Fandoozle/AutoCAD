;; This program creates multileaders with reversed text and deletes selected text
;; It uses the vlax and vla objects to manipulate the text and multileader entities
(defun c:am (/ newleader pt1 pt2 ss txt x w rjp-getbbwdth) ; Define a function with the name am and some local variables
(vl-load-com) ; Load the Visual LISP COM support
 ;; define function to get bounding box width
(defun rjp-getbbwdth (obj / out ll ur) ; Define a function with the name rjp-getbbwdth and some local variables
(vla-getboundingbox obj 'll 'ur) ; Get the bounding box of the given object and store the lower-left and upper-right corners
(setq out (mapcar 'vlax-safearray->list (list ll ur))) ; Convert the corners to lists and store them in out
(distance (car out) (list (caadr out) (cadar out))) ; Calculate and return the distance between the x-coordinates of the corners
)
(if (setq ss (ssget '((0 . "*TEXT")))) ; If there are any text entities in the drawing, select them and store them in ss
;; get text content and width
(progn ; Begin a series of expressions
(setq txt (apply ; Apply a function to a list of arguments
'strcat ; The function is strcat, which concatenates strings
(mapcar ; Apply a function to each element of a list
'cdr ; The function is cdr, which returns the rest of a list
(vl-sort ; Sort a list according to a comparison function
(mapcar ; Apply a function to each element of a list
'(lambda (x) ; The function is a lambda expression that takes one argument x
(cons ; Create a cons cell
(vlax-get x 'insertionpoint) ; The car of the cons cell is the insertion point of the text entity
(strcat (vlax-get x 'textstring) " ") ; The cdr of the cons cell is the text string of the text entity with a space added
)
)
(setq ; Assign a value to a variable
ss (mapcar ; Apply a function to each element of a list
'vlax-ename->vla-object ; The function is vlax-ename->vla-object, which converts an entity name to a VLA object
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))) ; The list is the result of removing any sublist from the list of entity names in the selection set
)
)
)
)
(function (lambda (y1 y2) (< (cadr (car y2)) (cadr (car y1)))) ; The comparison function is a lambda expression that compares the y-coordinates of the insertion points
)
)
)
)
w (car (vl-sort (mapcar 'rjp-getbbwdth ss) '>)) ; Get the maximum bounding box width of the text entities
txt (apply 'strcat ; Apply a function to a list of arguments
(mapcar 'chr (reverse (cdr (reverse (vl-string->list txt))))) ; The list is the result of reversing the characters of the concatenated text string
)
)
;; delete selected text
(mapcar 'vla-delete ss) ; Delete all the text entities in the selection set
)
)
(if (and (setq pt1 (getpoint "\nSpecify leader arrowhead location: ")) ; If the user specifies a point for the leader arrowhead
(setq pt2 (getpoint pt1 "\nSpecify landing location: ")) ; And the user specifies a point for the landing location
)
;; create multileader entity    
(progn ; Begin a series of expressions
(command "._MLEADER" pt1 pt2 "") ; Create a multileader entity with the given points
;; reverse text and set width
(setq newleader (vlax-ename->vla-object (entlast))) ; Get the VLA object of the multileader entity
(vla-put-textstring newleader txt) ; Set the text string of the multileader entity to the reversed text
(vla-put-textwidth newleader w) ; Set the text width of the multileader entity to the maximum width
)
)
(princ) ; Exit the function quietly
)
