;; this exports the current drawing to a later version (2013) then closes the drawing without saving
(defun c:PORTME ( / )
 (command "ZOOM" "E");; zoom to extents
 (vlax-for OpenDwgs (vla-get-documents (vlax-get-acad-object))
   (command "-exporttoautocad" "F" "2013" "" "")(command)(command)
 ) vlax-for
(command "_CLOSE" "_Y") ;; closes the drawing without saving
 (princ)
);; end PORTME
