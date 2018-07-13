(defun C:WL (/ pt1 pt2); = Wiring Line
  (setvar 'osmode 64); Center
  (setq pt1 (getpoint "\nStart point for Line(s): "))
  (while (setq pt2 (getpoint "\nEnd of Line: "))
    (command
      "_.line" pt1 "_e" pt2 "_direction" ; [spelling out "_end" is taken as Osnap call]
      (angtos (apply (if (> (car pt2) (car pt1)) '+ '-) (list (angle pt1 pt2) (/ pi 5))))
        ; change 5 above to lower number for more bulge, higher for less
    ); command
    (setq pt1 pt2); for start of next Line
  ); while
); defun