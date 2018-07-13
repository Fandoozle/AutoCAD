(defun C:WA (/ pt1 pt2); = Wiring Arcs
  (setvar 'osmode 64); Insertion
  (setq pt1 (getpoint "\nStart point for Arc(s): "))
  (while (setq pt2 (getpoint "\nEnd of Arc: "))
    (command
      "_.arc" pt1 "_e" pt2 "_direction" ; [spelling out "_end" is taken as Osnap call]
      (angtos (apply (if (> (car pt2) (car pt1)) '+ '-) (list (angle pt1 pt2) (/ pi 5))))
        ; change 5 above to lower number for more bulge, higher for less
    ); command
    (setq pt1 pt2); for start of next Arc
  ); while
); defun