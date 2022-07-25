;;  ZoomExtentsAllLayouts.lsp [command name: ZEAL]
;;  To get into all Layouts, go to Paper Space in and Zoom to the Extents for each,
;;  ending in Model Space and Zooming to the Extents there also.
;;  Kent Cooper, November 2011
(defun C:ZEAL ()
  (foreach lay (layoutlist)
    (setvar 'ctab lay)
    (command "_.pspace" "_.zoom" "_extents")
  ); end foreach
  (setvar 'ctab "Model")
  (command "_.zoom" "_extents")
); end defun