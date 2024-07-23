(defun C:vpfreeze (/)
  (vl-load-com)
  (setq myvp (car (entsel "\nSelect viewport: ")))
  (setq myvp (vlax-ename->vla-object myvp))
  (setq mylay (getstring "\nEnter layer name: "))
  (vpfreeze myvp mylay)
)

(defun vpfreeze	(vp lay / typ val)
  (vla-getXdata vp "ACAD" 'typ 'val)
  (setq	typ (reverse
	      (cons
		1002
		(cons 1002
		      (cons 1003 (cddr (reverse (vlax-safearray->list typ))))
		)
	      )
	    )
	val (reverse
	      (cons (vlax-make-variant "}")
		    (cons (vlax-make-variant "}")
			  (cons	(vlax-make-variant lay)
				(cddr (reverse (vlax-safearray->list val)))
			  )
		    )
	      )
	    )
  )
  (vla-setXData
    vp
    (vlax-safearray-fill
      (vlax-make-safearray
	vlax-vbInteger
	(cons 0 (1- (length typ)))
      )
      typ
    )
    (vlax-safearray-fill
      (vlax-make-safearray
	vlax-vbVariant
	(cons 0 (1- (length val)))
      )
      val
    )
  )
  ;; this is needed to display the change
  (vla-display vp :vlax-false)
  (vla-display vp :vlax-true)
)