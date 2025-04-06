;;  ScaleAboutCenters.lsp [command name: SAC]
;;  To Scale multiple objects, each About its own Center, by the same User-specified
;;    scale factor.
;;  Uses the middle of each object's bounding box as the base point for scaling, to
;;    keep objects centered at approximately the same position in the drawing.
;;    [For Mtext, that will be based on the defined Mtext box width, not the extents
;;    of the content; for a Block or Text, the center of its extents in the drawing, not
;;    its insertion point; for an Arc, the center of its extents, not its geometric center;
;;    some entity types' (e.g. Spline's) bounding box can sometimes reach beyond
;;    its extents and affect results slightly.]
;;  Rejects selection of objects on locked Layers, or without a "center" [Rays, Xlines].
;;  Stores scale factor; offers as default on subsequent use in same editing session.
;;  Kent Cooper, 6 May 2014

(defun C:SAC (/ *error* cmde ss inc ent)
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); end if
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
    (princ)
  ); end defun - *error*
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "_.undo" "_begin")
  (setq *SACscl
    (cond
      ( (getreal
          (strcat
            "\nEnter Scale Factor <"
            (if *SACscl (rtos *SACscl 2 4) "1"); offer default: prior value / 1 on first use
            ">: "
          ); strcat
        ); getreal
      ); User-input condition
      (*SACscl); Enter on subsequent use [prior value]
      (1); Enter on first use
    ); cond & *SACscl
    ss (ssget ":L" '((-4 . "<NOT") (0 . "RAY,XLINE") (-4 . "NOT>")))
      ;; not objects on Locked Layers or without finite extents
  ); setq
  (repeat (setq inc (sslength ss))
    (setq ent (ssname ss (setq inc (1- inc))))
    (vla-getboundingbox (vlax-ename->vla-object ent) 'minpt 'maxpt)
    (command
      ".scale" ent "" "_none"
      (mapcar '/ ; midpoint of bounding box
        (mapcar '+ (vlax-safearray->list minpt) (vlax-safearray->list maxpt))
        '(2 2 2)
      ); mapcar
      *SACscl
    ); command
  ); repeat
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); defun
(vl-load-com)
(prompt "\nType SAC to Scale objects About each one's Center.")