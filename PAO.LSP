;; To draw a Polyline of line segments with "A" option making pre-
;; defined arc segment of 180 degrees and 1-unit diameter, with
;; diameter as continuation of direction of previous line segment.
;; Kent Cooper, June 2011

(defun C:PAO (/ pt1 pt2 pt3); = Polyline with Arc-Overs
  (setq
    pt1 (getpoint "\nStarting point: ")
    pt2 (getpoint pt1 "\nNext point: ")
  ); end setq
  (command "_.pline" pt1 pt2)
  (initget "Arc")
  (while
    (setq pt3 (getpoint pt2 "\nNext point or [Arc]: "))
    (if (= pt3 "Arc")
      (command ; else - predefined arc segment
        "_arc"
        "_direction"
        (/ (* (+ (angle pt1 pt2) (/ pi 2)) 180) pi)
        (polar pt2 (angle pt1 pt2) 1)
        "_line"
        (setq
          pt1 (getvar 'lastpoint)
          pt2 (getpoint pt1 "\nNext point: ")
        )
      ); end command
      (progn ; else - line segment
        (command pt3)
        (setq pt1 pt2 pt2 pt3); advance points
      ); end progn
    ); end if
    (initget "Arc")
  ); end while
  (command); end Polyline
  (princ)
); end defun