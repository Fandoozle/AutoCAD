(defun c:PORTME ( / )
 (command "ZOOM" "E");; zoom to extents
 (vlax-for OpenDwgs (vla-get-documents (vlax-get-acad-object))
   (command "-exporttoautocad" "F" "2013" "" "")(command)(command)
 ) vlax-for
(vl-load-com)
(vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "_.CLOSE ")
 (princ)
);; end PORTME