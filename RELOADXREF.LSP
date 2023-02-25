;; This AutoLISP routine reloads all external references (xrefs) in the current drawing while turning off the VISRETAIN system variable to avoid any unexpected changes.
;; To use it, type XRE on the command line.

(defun c:XRE ( / )
(setvar 'VISRETAIN 0) ;; Set VISRETAIN to 0 to avoid any unexpected changes
(command "-XREF" "r" "*") ;; Reload all external references
(setvar 'VISRETAIN 1) ;; Set VISRETAIN back to 1
(command "QSAVE") ;; Quick save the drawing
) ;; end of defun block
