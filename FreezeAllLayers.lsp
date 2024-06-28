(defun c:freezeall ()
  (setvar 'cmdecho 0) ; Disable command echoing
  (command "_.VPLAYER" "Freeze" "*" "CURRENT" "") ; Freeze all layers in the current viewport
  (setvar 'cmdecho 1) ; Re-enable command echoing
  (princ) ; Print a newline character
)
