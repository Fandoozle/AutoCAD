; Jason Piercey . May 16th, 2003 
; assign a pagesetup to a layout 
; [document] - vla-object, layout object 
; [layout] - string, layout name 
; [setup] - string, pagesetup to assign 
; return: T or nil 
; example: (putPagesetup [document] [layout] [setup]) 
; revised: July 27th, 2003 
; trimmed off the fat and used (vl-catch-all.....) 
(defun putPagesetup (document layout setup) 
  (if (vl-catch-all-error-p (vl-catch-all-apply (function (lambda () (vla-copyfrom (vla-item (vla-get-layouts document) layout) (vla-item (vla-get-plotconfigurations document) setup) ) ) ) ) ) 
    nil t 
  ) 
) 

;To assign a pagesetup named "test" to all layouts defined in the current document. 
(setq *doc* (vla-get-activedocument (vlax-get-acad-object))) 
(foreach x (layoutlist) (putPagesetup *doc* x "test"))