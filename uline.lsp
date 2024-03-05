(defun c:uline (/ sset num ent sub)
(setq sset (ssget '((0 . "*text"))))
(setq num 0)
(repeat (sslength sset)
(setq
ent (entget (ssname sset num))
sub (cdr (assoc 1 ent))
)
(if (= (cdr (assoc 0 (entget (ssname sset num)))) "TEXT")
(setq ; TEXT
sub (vl-string-subst "" "%%U" sub)
sub (vl-string-subst "" "%%u" sub)
)
(setq ; MTEXT
sub (vl-string-subst "" "{\\L" sub)
sub (vl-string-subst "" "}" sub)
)
)
(setq
sub (subst (cons 1 sub)(assoc 1 ent) ent)
num (1+ num)
)
(entmod sub)
); end repeat
(princ)
)