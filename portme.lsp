(defun c:PORTME ( / )
 (vlax-for OpenDwgs (vla-get-documents (vlax-get-acad-object))
   (command "-exporttoautocad" "F" "2013" "" "")(command)(command)
 ) vlax-for
 (princ)
);; end PORTME
