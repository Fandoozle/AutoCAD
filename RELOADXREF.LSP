// turn off visretain, reload all xrefs and turn visretain back on
(defun c:XRE ( / )
  (setvar 'VISRETAIN 0)
  (command "-XREF" "r" "*")
  (setvar 'VISRETAIN 1)
 (command "QSAVE");; quicksave
);defun