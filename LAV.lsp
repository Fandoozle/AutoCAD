(defun C:LAV (/ ss n vp); = Lock All Viewports
  (repeat (setq n (sslength (setq ss (ssget "_X" '((0 . "VIEWPORT"))))))
    (if (> (cdr (assoc 69 (entget (setq vp (ssname ss (setq n (1- n))))))) 1); not the Paper Space Viewport of its Layout
      (vla-put-DisplayLocked (vlax-ename->vla-object vp) -1); lock it

    ); if
  ); repeat
); defun

(C:LAV)