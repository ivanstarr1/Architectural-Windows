(if (= VAdd nil) (load "Architectural Windows Support Functions"))


(defun c:UCSFromUCSBlockInsert (/ UCSBlock)

	(setq UCSBlock (car (entsel "\nSelect UCS block insert:")))

	(ToggleSnapsOff)
	(ClearUserInput)

	(setq TransformMatrix (UCSTransformMatrixFromUCSBlockInsert UCSBlock))

	(U0)
	(UCSFromTransformMatrix TransformMatrix)

 	(SetUserInput)
	(ToggleSnapsOn)

	(setq pt (getpoint "\nPoint for 4x4 matrix to be printed in AutoCAD:"))
	(Print4xNMatrix TransformMatrix pt)
	
) ; defun


(defun UCSTransformMatrixFromUCSBlockInsert (UCSBlock / debug UCSBlockDXF UCSBlockECS UCSBlockDXFAng
																						           UCSBlockCSWithoutOffset UCSBlockOffsetInECS
																						           UCSBlockOffsetInECS4Sp UCSBlockOffset4Sp
																						           UCSBlockOffset UCSBlockCS)
	(setq UCSBlockDXF (entget UCSBlock))

	(setq UCSBlockECS (GetECSTransformMatrix UCSBlock))

	(setq debug nil)
	
	(setq UCSBlockDXFAng (cdr (assoc 50 UCSBlockDXF)))
	(setq UCSBlockCSWithoutOffset (MMult UCSBlockECS (ZRotate4Sp UCSBlockDXFAng)))

	(setq UCSBlockOffsetInECS (cdr (assoc 10 UCSBlockDXF)))
	(setq UCSBlockOffsetInECS4Sp (3Sp->4Sp (list UCSBlockOffsetInECS)))
	(setq UCSBlockOffset4Sp (MMult UCSBlockECS UCSBlockOffsetInECS4Sp))
	(setq UCSBlockOffset (nth 0 (4Sp->3Sp UCSBlockOffset4Sp)))
	(setq UCSBlockCS (MMult (Offset4Sp UCSBlockOffset) UCSBlockCSWithoutOffset))

	UCSBlockCS

) ; defun


(defun c:TransformPlinePoints ()

	(ToggleSnapsOff)
	(ClearUserInput)

	(setq pline nil)
	(setq TransformMatrix nil)
	(setq plinepoints nil)
	(setq TransformedPlinePoints nil)
	(setq TransformedPlinePoints3Sp nil)

	(if (= MatrixPrintPt nil) (setq MatrixPrintPt (list 0 -2 0)))

	(setq MatrixHorizOffset (list (* (getvar "textsize") 5) 0 0))
	
	(setq TransformMatrix
		(UCSTransformMatrixFromUCSBlockInsert (car (entsel "\nPick UCS icon block:")))
	) ; setq 

	; Draw (write out) the TransformMatrix
	(setq MatrixPrintPt (Print4xNMatrix TransformMatrix MatrixPrintPt))

	(setq pline (car (entsel "\nSelect a polyline: ")))
	(setq plinepoints (Get3DWCSPointsFromLWPline pline))
	(setq plinepoints4sp (3Sp->4Sp plinepoints))

	; Draw (write out) the pline points matrix
	(setq MatrixPrintPt (VAdd MatrixPrintPt MatrixHorizOffset))
	(setq MatrixPrintPt (Print4xNMatrix plinepoints4sp MatrixPrintPt))
	
	(setq TransformedPlinePoints (MMult TransformMatrix plinepoints4sp))
	(setq TransformedPlinePoints3Sp (4Sp->3Sp TransformedPlinePoints))

 ; Draw the =
	(setq MatrixPrintPt (VAdd MatrixPrintPt MatrixHorizOffset))
	(setq TextSize (getvar "TextSize"))
	(command "text" "s" "Monotxt" "s" "Monotxt" "j" "Left" MatrixPrintPt (* TextSize 6) 0 "=")
	(setvar "textsize" TextSize)

	(setq MatrixPrintPt (VAdd MatrixPrintPt MatrixHorizOffset))

	; Draw (write out) the pline points matrix
	(setq MatrixPrintPt (VAdd MatrixPrintPt MatrixHorizOffset))
	(setq MatrixPrintPt (Print4xNMatrix TransformedPlinePoints MatrixPrintPt))
	
	; Finally Draw the pline points themselves
	(Draw3spPoints TransformedPlinePoints3Sp t)

	; Adjust MatrixPrintPt in case program is run again
	(setq MatrixPrintPt (list 0 (- (YCoord MatrixPrintPt) (* TextSize 8)) 0))
	
 	(SetUserInput)
	(ToggleSnapsOn)

) ; defun
