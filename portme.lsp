;; A routine that uses the autocad comand "exporttoautocad"
;; It exports the autocad file to a 2013 version
(defun c:PORTME ( / )
 (vlax-for OpenDwgs (vla-get-documents (vlax-get-acad-object))
   (command "-exporttoautocad" "F" "2013" "" "")(command)(command)
 ) vlax-for
 (princ)
);; end PORTME