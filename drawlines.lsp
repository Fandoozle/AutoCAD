;;;; This creates a group of lines with names of all the layers

(defun c:drawlines (/ pt dat ln)
(setq pt (getpoint "\nSelect insertion point: "))
(while (setq dat (tblnext "layer" (null dat)))
(entmake (list '(0 . "line") (cons 10 pt) (cons 11 (mapcar '+ pt '(2.5 0 0))) (cons 8 (setq ln (cdr (assoc 2 dat))))))
(entmake (list '(0 . "text") (cons 10 (mapcar '+ pt '(3.0 0 0))) '(40 . 0.1) (cons 1 ln) (cons 8 ln)))
(setq pt (mapcar '- pt '(0 0.15 0))))
(princ))
