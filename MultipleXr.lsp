;;; Attach Xref to all drawings in Folder , RLX 6-Feb-2019
(defun c:RlxFaXref (/ _getfolder app adoc odbs odbx v xref folder dwg xr)
  (vl-load-com)
  (defun _getfolder ( m / sh f r )
    (setq sh (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application") f (vlax-invoke-method sh 'browseforfolder 0 m 0))
    (vlax-release-object sh)(if f (progn (setq r (vlax-get-property (vlax-get-property f 'self) 'path))(if (wcmatch r "*\\") r (strcat r "\\")))))
  ; i is 0 (absolute), 1 (relative) of 2 (no) -xref path
  (defun RLXref_SetPathType (i)
    (vl-registry-write (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\Dialogs\\XattachDialog") "PathType" i))
  (setq odbs "ObjectDBX.AxDbDocument" v (substr (getvar 'acadver) 1 2) adoc (vla-get-activedocument (setq app (vlax-get-acad-object))))
  (RLXref_SetPathType 1)
  (cond
    ((vl-catch-all-error-p (setq odbx (vl-catch-all-apply 'vla-getinterfaceobject (list app (if (< (atoi v) 16) odbs (strcat odbs "." v))))))
     (princ "\nObject DBX interface not created!"))
    ((not (setq xref (getfiled "Select Xref to attach" "" "dwg" 0))) (alert "No Xref was selected"))
    ((setq folder (_getfolder "Select folder with drawings to attach xref to"))
     (foreach dwg (vl-directory-files folder "*.dwg" 0)
       (setq dwg (strcat folder dwg))
       (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list odbx dwg)))
  (princ (strcase (strcat "\nError opening: " dwg)))
  (progn
    (princ (strcat "\nOpening: " dwg))
    ; attach xref
    (if (vl-catch-all-error-p (setq xr (vl-catch-all-apply 'vla-AttachExternalReference
        (list (vla-get-ModelSpace odbx) xref (vl-filename-base xref) (vlax-3d-point 0 0 0) 1 1 1 0 :vlax-false))))
      (princ (vl-catch-all-error-message xr)))
    ; save drawing
    (vla-saveas odbx (vla-get-name odbx))
  )
       )
     )
    )
  )
  (princ)
)