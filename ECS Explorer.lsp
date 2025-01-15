(if (= VAdd nil) (load (findfile "Architectural Windows Support Functions")))

(defun c:ECSExplorer ( / dcl_id)

	(setq Ent (car (entsel "\nPick a 2D entity:")))
	(setq EntDXF (entget Ent))

	; Extract information affected by or affecting the ECS:
	(setq ExtrusionVect (cdr (assoc 210 EntDXF)))
	(setq ExtrusionVectLine (strcat "Extrusion Vector (210):\t" (vl-prin1-to-string ExtrusionVect)))
	(setq Elevation (cdr (assoc 38 EntDXF)))
	(setq ElevationLine (strcat "Elevation (38):\t" (vl-prin1-to-string Elevation)))
	; This code gets the list of DXF code 10 points;
	(setq EntDXFCopy EntDXF)
	(setq 10Points nil)
	(while EntDXFCopy
		(setq CurrDXF (car EntDXFCopy))
		(setq EntDXFCopy (cdr EntDXFCopy))
		(if (= (car CurrDXF) 10)
			(setq 10Points (cons (cdr CurrDXF) 10Points))
		) ; if
	) ; while
	(setq PointsLine (strcat "Points (10):\t" (vl-prin1-to-string 10Points)))

	; Print out that information to the command line
	(princ "\nEntity DXF:")(princ EntDXF)
	(princ "\nExtrusion Vector:")(princ ExtrusionVect)
	(princ "\nElevation:")(princ Elevation)
	(princ "\nPoints:")(princ 10Points)
	
	; This puts the entity DXF into a list for display in a listbox in the dialog
	(setq EntDXFCopy EntDXF)
	(setq DXFStrList nil)
	(while EntDXFCopy
		(setq CurrDXF (car EntDXFCopy))
		(setq EntDXFCopy (cdr EntDXFCopy))
		(setq DXFStrList (cons (vl-prin1-to-string CurrDXF) DXFStrList))
	) ; while
	(setq DXFStrList (reverse DXFStrList))

	
	(setq ECSXFormMatrix (GetECSTransformMatrix Ent))
	(defun ECSXFormRowListboxFormat (ECSXFormRow)
		(strcat
			"(\t"
			(rtos (nth 0 ECSXFormRow) 2 5)
			"\t"
			(rtos (nth 1 ECSXFormRow) 2 5)
			"\t"
			(rtos (nth 2 ECSXFormRow) 2 5)
			"\t"
			(rtos (nth 3 ECSXFormRow) 2 5)
			"\t)"
		) ; strcat
	) ; defun
	(setq strXFormList (mapcar 'ECSXFormRowListboxFormat ECSXFormMatrix))

	; Inserts a ucs icon at the ECS
	(setq ECSVects (4sp->3sp ECSXFormMatrix))
	(setq XHat (nth 0 ECSVects))
	(setq YHat (nth 1 ECSVects))
	(setq ZHat (nth 2 ECSVects))
	(setq Origin (nth 3 ECSVects))
	(ToggleSnapsOff)
	(U3 Origin (VAdd Origin XHat) (VAdd Origin YHat))
	(command "insert" "UCSIcon" (list 0 0 0) 1 1 1 0)
	(UP)
	(ToggleSnapsOn)


	; Allright now show that info in a dialog!
  (setq dcl_id (load_dialog "ECS Explorer.dcl"))

	(new_dialog "ECSExplorerDialog" dcl_id)
	
	(start_list "lstECSData")
	(add_list ExtrusionVectLine)
	(add_list ElevationLine)
	(add_list PointsLine)
	(end_list)

	(start_list "lstECSXForm")
	;(mapcar '(lambda (x) (add_list (vl-prin1-to-string x))) ECSXFormMatrix)
	(mapcar 'add_list strXFormList)
	(end_list)

	(start_list "lstDXF")
	(mapcar 'add_list DXFStrList)
	(end_list)

	(start_dialog)
	
	(unload_dialog dcl_id)

	(entdel (entlast))
	
	(princ)
	
) ; defun