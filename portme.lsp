(defun c:PORTME ( / )
 (vlax-for OpenDwgs (vla-get-documents (vlax-get-acad-object))
   ;;(vla-setvariable OpenDwgs "TILEMODE" 0)
	(command "-exporttoautocad" "F" "2013" "" "")
 );; vlax-for
 (princ)
)
