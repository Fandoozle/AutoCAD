
;;;; This replaces txt with a block with an attribute tag
(defun c:Txt2Blk (/ T2B_Selection T2B_Baseblock T2B_AttributeTag T2B_ActiveLayout T2B_ActiveDoc T2B_Text T2B_Block)
   (setq T2B_Baseblock    "ANNO-13A")
   (setq T2B_AttributeTag "CH1")   
   (if
      (and
         (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list (vla-get-Blocks (setq T2B_ActiveDoc (vla-get-ActiveDocument (vlax-get-acad-object)))) T2B_Baseblock))))
         (setq T2B_Selection (ssget '((0 . "*TEXT"))))
      )
      (progn
         (setq T2B_ActiveLayout (vla-get-Block (vla-get-ActiveLayout T2B_ActiveDoc)))
         (vla-StartUndoMark T2B_ActiveDoc)
         (foreach T2B_Text (mapcar 'cadr (ssnamex T2B_Selection))
            (if
               (= (type T2B_Text) 'ENAME)
               (progn
                  (setq T2B_Text (vlax-ename->vla-object T2B_Text))
                  (setq T2B_Block (vla-InsertBlock T2B_ActiveLayout (vla-get-InsertionPoint T2B_Text) T2B_Baseblock 1 1 1 0))
                  (PushAttValue T2B_Block (list (list T2B_AttributeTag (vla-get-TextString T2B_Text))))
                  (vla-Delete T2B_Text)
               )
            )
         )
         (vla-EndUndoMark T2B_ActiveDoc)
         (vlax-release-object T2B_ActiveLayout)
      )
   )
   (vlax-release-object T2B_ActiveDoc)
   (princ)
)

(defun PushAttValue (PAV_BlkObject PAV_TagValList / PAV_AttList)
   (if
      (and
         (= (type PAV_BlkObject)     'VLA-OBJECT)
         (= (vla-get-ObjectName    PAV_BlkObject) "AcDbBlockReference")
         (= (vla-get-HasAttributes PAV_BlkObject) :vlax-true)
      )     
      (progn
         (setq PAV_AttList (vlax-safearray->list (vlax-variant-value (vla-GetAttributes PAV_BlkObject))))
         (foreach PAV_Item PAV_AttList
            (vl-catch-all-apply 'vla-put-TextString (list PAV_Item (cadr (assoc (strcase (vla-get-TagString PAV_Item)) PAV_TagValList))))
         )
     )           
   )
)    
