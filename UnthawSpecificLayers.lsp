(defun c:unthawall ()
  (setvar 'cmdecho 0)
  (command "_.VPLAYER" "THAW" "A-Building-Exst" "CURRENT" "")
  (command "_.VPLAYER" "THAW" "A-Building-Hatch" "CURRENT" "")
  (command "_.VPLAYER" "THAW" "A-Cable Tray Exst" "CURRENT" "")
  (command "_.VPLAYER" "THAW" "A-Equip-Exst" "CURRENT" "")
  (command "_.VPLAYER" "THAW" "G-Ground BAR-Exst" "CURRENT" "")
  (setvar 'cmdecho 1)
  (princ)
)
