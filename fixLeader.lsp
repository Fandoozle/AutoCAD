(defun c:am (/ newleader pt1 pt2 ss txt x w rjp-getbbwdth)
(vl-load-com)
(defun rjp-getbbwdth (obj / out ll ur)
(vla-getboundingbox obj 'll 'ur)
(setq out (mapcar 'vlax-safearray->list (list ll ur)))
(distance (car out) (list (caadr out) (cadar out)))
)
(if (setq ss (ssget '((0 . "*TEXT"))))
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
(mapcar 'vla-delete ss)
)
)
(if (and (setq pt1 (getpoint "\nSpecify leader arrowhead location: "))
(setq pt2 (getpoint pt1 "\nSpecify landing location: "))
)
(progn (command "._MLEADER" pt1 pt2 "")
(setq newleader (vlax-ename->vla-object (entlast)))
(vla-put-textstring newleader txt)
(vla-put-textwidth newleader w)
)
)
(princ)
)