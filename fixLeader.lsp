;; This program creates multileaders with reversed text and deletes selected text
;; It uses the vlax and vla objects to manipulate the text and multileader entities
(defun c:am (/ newleader pt1 pt2 ss txt x w rjp-getbbwdth)
(vl-load-com)
 ;; define function to get bounding box width
(defun rjp-getbbwdth (obj / out ll ur)
(vla-getboundingbox obj 'll 'ur)
(setq out (mapcar 'vlax-safearray->list (list ll ur)))
(distance (car out) (list (caadr out) (cadar out)))
)
(if (setq ss (ssget '((0 . "*TEXT"))))
;; get text content and width
(progn (setq txt (apply
'strcat
(mapcar
'cdr
(vl-sort
(mapcar '(lambda (x)
(cons (vlax-get x 'insertionpoint)
(strcat (vlax-get x 'textstring) " ")
)
)
(setq
ss (mapcar
'vlax-ename->vla-object
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
)
)
)
(function (lambda (y1 y2) (< (cadr (car y2)) (cadr (car y1))))
)
)
)
)
w (car (vl-sort (mapcar 'rjp-getbbwdth ss) '>))
txt (apply 'strcat
(mapcar 'chr (reverse (cdr (reverse (vl-string->list txt)))))
)
)
;; delete selected text
(mapcar 'vla-delete ss)
)
)
(if (and (setq pt1 (getpoint "\nSpecify leader arrowhead location: "))
(setq pt2 (getpoint pt1 "\nSpecify landing location: "))
)
;; create multileader entity    
(progn (command "._MLEADER" pt1 pt2 "")
;; reverse text and set width
(setq newleader (vlax-ename->vla-object (entlast)))
(vla-put-textstring newleader txt)
(vla-put-textwidth newleader w)
)
)
(princ)
)
