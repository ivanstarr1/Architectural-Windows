; Architectural Windows.lsp by Ivan Starr 5/2022

; All 3DSolid wall objects must be on a layer "Walls"

(if (= VAdd nil) (load "C:\\Users\\ivans\\OneDrive\\Documents\\GitHub\\Architectural Windows\\Architectural Windows Support Functions"))

(setq WINDOWFRAMETHICKNESS 7)
(setq WINDOWFRAMEHEIGHT 9)
(setq GLASSTHICKNESS 0.5)
(setq WINDOWFRAMELAYER "A-Glazing-Frames")
(setq WINDOWGLASSLAYER "A-Glazing-Glass")
(setq WALLLAYER "Walls")
(setq MAXWALLTHICKNESS 40)

(defun AddWindow ()

	(ToggleSnapsOn)
	(initget 8 "Rectangle")
	(setq Ans3 (getpoint "\nFirst corner of window or pick Rectangle [Rectangle] <Rectangle>: "))
	(ToggleSnapsOff)
	(if (or (= Ans3 "Rectangle") (= Ans3 nil))
		(progn
			(setq PlRect (car (entsel "\mPick a polyline rectangle: ")))
			(if PlRect
				(if (= (cdr (assoc 0 (entget PlRect))) "LWPOLYLINE")
					(progn
						(CreateNewWindowFromRectInWall PLRect)
						; Put the ID in the WindowDictionary
						(initget 8 "Yes No")
						(setq YesNoAns (getkword "\nDelete rectangle? [Yes/No] <Yes>: "))
						(if (or (= YesNoAns "Yes") (= YesNoAns nil)) 
							(entdel PlRect)
						) ; if
					) ; progn
					(alert "Entity is not a pline")
				) ; if
				(alert "No entity selected")
			) ; if

		) ; progn
	) ; if
	(if (= (type Ans3) 'LIST) ; Then it's the first point of a new rectangle for a new window
		(progn
			(command ".rectang" Ans3)
			(ToggleSnapsOn)
			(command Pause)
			(ToggleSnapsOff)
			(setq DefiningRectangle (entlast))
			(CreateNewWindowFromRectInWall (entlast))
			(command ".erase" DefiningRectangle "")
		) ; progn
	) ; if
	
) ; defun

; This here is the top-level (main) function that defines the Window command
(defun c:Window ()

	(ToggleSnapsOff)
	(if (not (tblsearch "layer" WINDOWFRAMELAYER)) (command ".Layer" "m" WINDOWFRAMELAYER "c" 3 "" ""))
	(if (not (tblsearch "layer" WINDOWGLASSLAYER)) (command ".Layer" "m" WINDOWGLASSLAYER "c" 2 "" ""))	
	(LoadStandardErrorRoutine)
	(ClearUserInput)
	
	(setq Continue t)
	(while Continue

		(initget 8 "New Adjust Delete eXit")
		(setq Ans (getkword (strcat
						"\n  (N)ew Window from rectangle , (A)djust existing window, (D)elete window,"
						" or eXit [New/Adjust/Delete/eXit]<New>: "
				) ; strcat``
			) ; entsel
		) ; setq

		(if (or (= Ans "New") (= Ans nil))  ; OK a new window is about to be created
			(AddWindow)
		) ; if

		(if (= Ans "Adjust") ; If it's a window to adjust or delete
			(ModifyWindow)
		) ; if it's a window to adjust

		(if (= Ans "Delete")
			(DeleteWindow)

		) ; if
		
		(if (= Ans "eXit")
			(progn
				(setq Continue nil)
			) ; progn
		) ; if
		
	) ; while

 	(SetUserInput)
	(ToggleSnapsOn)
	(UnloadStandardErrorRoutine)
	
	(princ)
	
) ; defun

(defun ModifyWindow ()

	(setq PossWindowEnt (car (entsel "\nSelect window:")))
	(if PossWindowEnt
		(progn
			(setq PossWDDKey (IsEntityPartOfAWindow PossWindowEnt)) ; check to see if it really is a window ;XXX2
			(if PossWDDKey 
				(progn ; It is a window entity, so the PossWDDKey IS a WDDKEY
					(setq WDDKey PossWDDKey)
					(setq WindowDict (cdr (assoc -1 (dictsearch WINDOWDICTIONARYDICTIONARY WDDKey))))
					(SetUCSToWindowCS WindowDict)
					(setq Continue2 t)
					(while Continue2
						(ToggleSnapsOn)
						(initget 8 "eXit")
						(setq PossInputPoint (getpoint "n\Enter another point to adjust the corners of this window or eXit [eXit] <eXit>:"))
						(ToggleSnapsOff)
						(if (or (= PossInputPoint "eXit" ) (= PossInputPoint nil))
							(setq Continue2 nil)
						) ; if
						(if (= (type PossInputPoint) 'LIST)
							(progn
								(setq OldWindowDict (cdr (assoc -1 (dictsearch WINDOWDICTIONARYDICTIONARY WDDKey))))
								(setq NewWindowDict (AdjustWindow OldWindowDict (trans PossInputPoint 1 0 nil)))
								(vlax-ldata-put NewWindowDict "Id" WDDKey)
								(DeleteWindowAndWindowDictionary OldWindowDict t)
								(MakeWindowHole NewWindowDict)
								(dictadd WINDOWDICTIONARYDICTIONARY WDDKey NewWindowDict)
							) ; progn
						) ; if
					) ; while
					(UP)
				) ; progn
				(alert "Not part of any window in the window dictionary.")
			) ; if
		) ; progn
		(alert "Selection Error")
	) ; if
	
) ; defun

(defun DeleteWindow ()
	
	(setq PossWindowEnt (car (entsel "\nPick window to delete: ")))
	(if PossWindowEnt
		(progn
			(setq PossWDDKey (IsEntityPartOfAWindow PossWindowEnt)) ; check to see if it really is a window ;XXX2
			(if PossWDDKey 
				(progn ; It is a window entity, so the PossWDDKey IS a WDDKEY
					(setq WindowDict (ChildDictionary WINDOWDICTIONARYDICTIONARY PossWDDKey))
					(DeleteWindowAndWindowDictionary WindowDict t)
					;(dictremove WINDOWDICTIONARYDICTIONARY PossWDDKey)
					(alert "Window deleted.")
				) ; progn
				(alert "Not an entity of a known window")
			) ; if
		) ; progn
		(alert "Entity selection error")
	) ; if

) ; defun

(defun GetAllWalls (ssget "x" (list (cons 8 WALLLAYER))))

; This function figures out which 3DSolid wall entity the 2D Polyline Rectangle (parameter PLRect) is on, how thick the wall is where
; the window is, calls function DrawWindowFromRect with that information and an assumed side of 2D polyline that the
; 3DSolid wall is on, and then tests to see if the resulting window is inside the wall or not.  If it isn't, this
; function then deletes that window (and it'w window dictionary), then it calls DrawWindowFromRect to draw a new
; window on the OTHER side of the 2DPolyline (through the Handedness parameter)
(defun CreateNewWindowFromRectInWall (PLRect / WindowHole NewWindowDict NewUniqueID Ctr Continue Handedness CurrWallConflictPos CurrWallConflictNeg); WindowGlassEnt)

	
	(setq AllWalls (GetAllWalls))
	(setq NumWallEntities (sslength AllWalls))
	(setq Continue t)
	(setq Ctr 0)
	(while (and (= Continue t) (< Ctr NumWallEntities))
		(setq CurrWall (ssname AllWalls Ctr))
		(setq PlRectCopy1 (CopyInPlace PLRect))
		(setq PlRectCopy2 (CopyInPlace PLRect))
		(command "Extrude" PlRectCopy1 "" MAXWALLTHICKNESS)
		(setq WindowBoxPos (entlast))
		(command "Extrude" PlRectCopy2 "" (- MAXWALLTHICKNESS))
		(setq WindowBoxNeg (entlast))
		(setq CurrWallConflictPos (SpatialConflictSolid CurrWall WindowBoxPos))
		(setq CurrWallConflictNeg (SpatialConflictSolid CurrWall WindowBoxNeg))
		(entdel WindowBoxPos)
		(entdel WindowBoxNeg)
		(if CurrWallConflictPos (setq Continue nil WindowHole CurrWallConflictPos))
		(if CurrWallConflictNeg (setq Continue nil WindowHole CurrWallConflictNeg))
		(setq Ctr (1+ Ctr))
	) ; while
	; Now if Continue is nil, then the wall the window is going to be in has been found and it's CurrWall

	(if (= Continue nil)
		(progn

			; Generate Unique identifier FOR THE WINDOWDICTIONARYDICTIONARY,
			(setq NewUniqueID (UniqueID))

			; Figure out the wall thickness
			(setq RectArea (GetPLineArea PlRect))
			(setq WindowHoleVolume (Get3DSolidVolume WindowHole))
			(entdel WindowHole)
			(setq WallThickness (/ WindowHoleVolume RectArea))
			
			(setq NewWindowDict (DrawWindowFromRect PlRect CurrWall WallThickness 1))
			(vlax-ldata-put NewWindowDict "Id" NewUniqueID)
					
			; Get the glass ent to test to see if it's in the wall
			(setq WindowGlassEnt (handent (vlax-ldata-get NewWindowDict "GlassEntHandle")))

			 ; Flip the window to the other side of the rectangle if it is not inside the wall entity
			(if (not (IsWindowEntInsideWall WindowGlassEnt CurrWall))
				(progn
					(DeleteWindowAndWindowDictionary NewWindowDict nil)
					(setq NewWindowDict (DrawWindowFromRect PlRect CurrWall WallThickness -1))
					(vlax-ldata-put NewWindowDict "Id" NewUniqueID)
				) ; progn
			) ; if
			(MakeWindowHole NewWindowDict)
			(dictadd WINDOWDICTIONARYDICTIONARY NewUniqueID NewWindowDict)
			; Now the new window has been made, a hole in the wall has been made for it, it's Window dictionary has been added
			; to the window dictionary dictionary, and it's ID in the window dictionary dictionary has been included under
			; key ID
			NewUniqueID
		) ; progn
		(alert "No Window Created")
	)
	
) ; defun

; Figures out the window geometry from the rectangle using the 2DPolyline rectangle's Entity Coordinate system
; and it's 2D ECS points using some matrix math function calls, and then passes that info to the DrawWindow
; function to do the actual window drawing and creation of it's window dictionary
(defun DrawWindowFromRect (Rect WallEntity WallThickness Handedness / WindowDict WindowCS XLength YLength FrameEntHandle GlassEntHandle WallEntHandle WallThickness XHat YHat ZHat
													 2DECSPts 4sp2DECSPts EntECS 4sp3DWCSPts RectPts WindowOriginPt XVectPt YVectPt XVect YVect)

	; Figure out what the rectangle's corner points are in world coordinates
	(setq 2DECSPts (Get2DECSPointsFromWLPline Rect))
	(setq 4sp2DECSPts (3sp->4sp 2DECSPts))
	(setq EntECS (GetECSTransformMatrix Rect))
	(setq 4sp3DWCSPts (MMult EntECS 4sp2DECSPts))
	(setq RectPts (4sp->3sp 4sp3DWCSPts))

	; From the world coordinates, figure out what the window origin point, the window coordinate system unit vectors are, and the
	; XLength and YLength is
	(setq WindowOriginPt (nth 0 RectPts))
	(setq XVectPt (nth 1 RectPts)) ; X is corner 1
	(setq YVectPt (nth 3 RectPts)) ; Y is corner 3
	(setq XVect (VSubtr XVectPt WindowOriginPt))
	(setq YVect (VSubtr YVectPt WindowOriginPt))
	(setq XLength (VLength XVect))
	(setq YLength (VLength YVect))
	(setq XHat (UnitVect XVect))
	(setq YHat (UnitVect YVect))
	(setq ZHat (VCross XHat YHat))

	
	; Now the following is known:
	; WindowOriginPt XHat YHat ZHat XLength YLength Handedness
	(DrawWindow  WindowOriginPt XHat YHat ZHat XLength YLength WallThickness WallEntity Handedness)
	
) ; defun


; This is the function that actually draws the window and creates it's Window Dictionary given the required geometry.
; It's return value is the window dictionary entity name.
; The function's parameters are:
; WindowOriginPt - the origin point (the (0,0,0) point) of the window's coordinate system
; XHat, YHat, and ZHat - The X, Y, and Z unit vectors of the window's coordinate system
; XLength and YLength - the lengths of the window along it's coordinate system's X and Y axes
; WallThickness - the thickness of the wall that the window is in
; WallEntity - the handle of the 3DSolid wall that the window is in
; Handedness - the "Handedness" of the window's coordinate system.  As it turns out, this is not needed in this
; function (I just realized this, and besides, you can figure that out from the unit vectors)
(defun DrawWindow (WindowOriginPt XHat YHat ZHat XLength YLength WallThickness WallEntity Handedness / NewWindowDict OrgZOffset1 Org1 ZLength1 FrameEnt
									 Org2 XLength2 YLength2 XLength2 FrameEnt2 Org3 Org2 XLength3 YLength3 XLength3 GlassEnt) ; Returns a WindowDictionary

	; Back to the WCS
	(U0)
	; Move to the new window's coordinate system
	(U3 WindowOriginPt (VAdd WindowOriginPt XHat) (VAdd WindowOriginPt YHat))
	; Now create the frame by subtracting one box from another
	(setq OrgZOffset1 (/ (- WallThickness WINDOWFRAMETHICKNESS) 2 Handedness))
	(setq Org1 (list 0 0 OrgZOffset1))
	(setq XLength XLength)
	(setq YLength1 YLength)
	(setq ZLength1 (* WINDOWFRAMETHICKNESS Handedness))
	(UM Org1)

	(command ".layer" "s" WINDOWFRAMELAYER "")
	(command ".box"
					 (list 0 0 0) "Length" XLength YLength1 ZLength1
	) ; command
	(setq FrameEnt (entlast))
	(setq Org2 (list  WINDOWFRAMETHICKNESS WINDOWFRAMETHICKNESS 0))
	(setq XLength2 (- XLength (* WINDOWFRAMETHICKNESS 2)))
	(setq YLength2 (- YLength (* WINDOWFRAMETHICKNESS 2)))
	(setq ZLength2 ZLength1)
	(UM Org2)
	(command ".box"
					 (list 0 0 0) "Length" XLength2 YLength2 ZLength2
	) ; command
	(setq FrameEnt2 (entlast))
	(command ".subtract" FrameEnt "" FrameEnt2 "") ; Now its in FrameEnt1
	; The frame has now been made

	; Create the glass pane
	(setq Org3 (list  0 0 (/ (- WINDOWFRAMETHICKNESS GLASSTHICKNESS) 2 Handedness)))
	(setq XLength3 XLength2)
	(setq YLength3 YLength2)
	(setq ZLength3 GLASSTHICKNESS)
	(UM Org3)
	(command ".layer" "s" WINDOWGLASSLAYER "")
	(command ".box"
					 (list 0 0 0) "Length" XLength3 YLength3 ZLength3
	) ; command
	(setq GlassEnt (entlast))
	; now the glass has been made

	; now back up through the coordinate systems that were moved through in this function
	(UP)(UP)(UP)(UP)(UP)
	
	; create the new windowdictionary
	(setq NewWindowDict (CreateDictionary))
	
	; Load the dictionary with the info
	;		WindowCS XLength YLength Handedness FrameEntHandle GlassEntHandle
	(setq WindowCS
		(list
			(list (nth 0 XHat) (nth 0 YHat) (nth 0 ZHat) (nth 0 WindowOriginPt))
			(list (nth 1 XHat) (nth 1 YHat) (nth 1 ZHat) (nth 1 WindowOriginPt))
			(list (nth 2 XHat) (nth 2 YHat) (nth 2 ZHat) (nth 2 WindowOriginPt))
			(list 0.0 0.0 0.0 1.0)
		) ; list
	) ; setq

	(vlax-ldata-put NewWindowDict "WindowCS" WindowCS)
	(vlax-ldata-put NewWindowDict "XLength" XLength)
	(vlax-ldata-put NewWindowDict "YLength" YLength)
	(vlax-ldata-put NewWindowDict "Handedness" Handedness)
	(vlax-ldata-put NewWindowDict "FrameEntHandle" (Ent->Hand FrameEnt))
	(vlax-ldata-put NewWindowDict "GlassEntHandle" (Ent->Hand GlassEnt))
	(vlax-ldata-put NewWindowDict "WallEntHandle" (Ent->Hand WallEntity))
	(vlax-ldata-put NewWindowDict "WallThickness" WallThickness)

	; Return the WindowDictionaryHandle
	NewWindowDict
	
) ; defun


; Gets the drawing's WINDOWDICTIONARYDICTIONARY located in the (namedobjdict).  If one doesn't exist, it
; creates one, attaches it to the (namedobjdict), and returns it's entity name in either case
(defun GetWindowDictionaryDictionary ( / WindowDictHandle NewWindowDict)
	
	(setq WindowDictHandle (dictsearch (namedobjdict) "WindowDictionaryDictionary"))
	(if WindowDictHandle
		(cdr (assoc -1 WindowDictHandle))
		(progn
				(setq NewWindowDict (CreateDictionary))
				(dictadd (namedobjdict) "WindowDictionaryDictionary" NewWindowDict)
				NewWindowDict
			) ; progn
	) ; if
	
) ; defun


; This next line calls the above function when this program file is is first loaded in order to set the global
; WINDOWDICTIONARYDICTIONARY for use with this program
(if (not WINDOWDICTIONARYDICTIONARY) (setq WINDOWDICTIONARYDICTIONARY (GetWindowDictionaryDictionary)))


; Adjusts an existing window using it's WindowDict and a point Pt.  Adjusts the corner closest to the point Pt.
; Tha corner's new point is made to be Pt.  It actually draws a new window, and it's dictionary, and it's return
; value is the new dictionary's entity name
(defun AdjustWindow (WindowDict Pt / WindowDict WindowCS XLength YLength Handedness FrameEntHandle GlassEntHandle WallEntHandle WallThickness XHat YHat ZHat WindowOrigin DictList WindowCS
										 WindowCornerPoints Dists CurrValueIndex LowestValue LowestValueIndex CornerPt Delta)
	
	(setq DictList (vlax-ldata-list WindowDict))
	(setq WindowCS (vlax-ldata-get WindowDict "WindowCS"))
	(setq XLength (cdr (assoc "XLength" DictList)))
	(setq XDashLength (cdr (assoc "XLength" DictList)))
	(setq YLength (vlax-ldata-get WindowDict "YLength"))
	(setq Handedness (vlax-ldata-get WindowDict "Handedness"))
	(setq FrameEntHandle (vlax-ldata-get WindowDict "FrameEntHandle"))
	(setq GlassEntHandle (vlax-ldata-get WindowDict "GlassEntHandle"))
	(setq WallEntHandle (vlax-ldata-get WindowDict "WallEntHandle"))
	(setq WallThickness (vlax-ldata-get WindowDict "WallThickness"))

	(setq WindowCornerPoints (GetWindowCornerPoints WindowCS XLength YLength))

	(setq XHat (reverse (cdr (reverse (Col 0 WindowCS)))))
	(setq YHat (reverse (cdr (reverse (Col 1 WindowCS)))))
	(setq ZHat (reverse (cdr (reverse (Col 2 WindowCS)))))
	(setq WindowOrigin (reverse (cdr (reverse (Col 3 WindowCS)))))


	; Find the window corner closest to the Pt
	(setq Dists (list))
	(foreach Point WindowCornerPoints
		(setq Dist (VLength (VSubtr Point Pt)))
		(setq Dists (cons Dist Dists))
	) ; foreach
	(setq Dists (reverse Dists))

	(setq CurrValueIndex 1)
	(setq LowestValue (nth 0 Dists))
	(setq LowestValueIndex 0)
	(while (< CurrValueIndex 4)
		(setq CurrValue (nth CurrValueIndex Dists))
		(if (< CurrValue LowestValue)
			(progn
				(setq LowestValue CurrValue)
				(setq LowestValueIndex CurrValueIndex)
			) ; progn
		) ; if
		(setq CurrValueIndex (1+ CurrValueIndex))
	) ; while
	; Now LowestValueIndex is known

	(setq CornerPt (nth LowestValueIndex WindowCornerPoints))
	(setq Delta (VSubtr Pt CornerPt))
	
	(if (= LowestValueIndex 0) ; Maybe the Origin moved?
		(progn ; Yes it did!
			; Add Delta to Origin, subtract DeltaX from XLength, subtract DeltaY from YLength
			(setq WindowOrigin (VAdd WindowOrigin Delta))
			(setq XLength (- XLength (VDot Delta XHat)))
			(setq YLength (- YLength (VDot Delta YHat)))
		) ; progn
	) ; if
	
	(if (= LowestValueIndex 1); Maybe the Origin + DeltaX point moved?
		(progn
			; Add DeltaY to Origin,  subtract DeltaY from YLength, Add DeltaX toXLength
			(setq WindowOrigin (VAdd WindowOrigin (VMult Yhat (VDot YHat Delta))))
			(setq YLength (- YLength (VDot Delta YHat)))
			(setq XLength (+ XLength (VDot Delta XHat)))
		) ; progn
	) ; if
	
	(if (= LowestValueIndex 2)
		(progn
			(setq YLength (+ YLength (VDot Delta YHat)))
			(setq XLength (+ XLength (VDot Delta XHat)))
		) ; progn
	) ; if

	(if (= LowestValueIndex 3)
		(progn
			; Add DeltaX to Origin,  subtract DeltaX from XLength, Add DeltaY toYLength
			(setq WindowOrigin (VAdd WindowOrigin (VMult Xhat (VDot XHat Delta))))
			(setq YLength (+ YLength (VDot Delta YHat)))
			(setq XLength (- XLength (VDot Delta XHat)))
		) ; progn
	) ; if

	; OK, so now we have WDDKey and the new WindowOrigin/XLength/YLength combo.
	; the unit vectors and everything else doesn't change

	; - AdjustWindow should only produce the window and return the windowdict
	(DrawWindow WindowOrigin XHat YHat ZHat XLength YLength WallThickness (handent WallEntHandle) Handedness)

) ; defun


; Figures out the 4 window corner points of a window from the geometry stored in it's window dictionary
(defun GetWindowCornerPoints (WindowCS XLength YLength / 3spLocalPts 4spLocalPts 4spWCSPts 3spWCSPts)

	(setq 3spLocalPts
		(list
			(list 0 0 0)
			(list XLength 0 0)
			(list XLength YLength 0)
			(list 0 YLength 0)
		) ; list
	) ; setq
	(setq 4spLocalPts (3sp->4sp 3spLocalPts))
	(setq 4spWCSPts (MMult WindowCS 4spLocalPts))
	(setq 3spWCSPts (4sp->3sp 4spWCSPts))

) ; defun


; Seals up a rectangular hole for a window according to the geometry of the window stored in it's window dictionary
(defun SealWindowHole (WindowDict / WindowDict WindowCS XLength YLength Handedness FrameEntHandle GlassEntHandle WallEntHandle WallThickness)

	(setq WindowCS (vlax-ldata-get WindowDict "WindowCS"))
	(setq XLength (vlax-ldata-get WindowDict "XLength"))
	(setq YLength (vlax-ldata-get WindowDict "YLength"))
	(setq Handedness (vlax-ldata-get WindowDict "Handedness"))
	(setq WallEntHandle (vlax-ldata-get WindowDict "WallEntHandle"))
	(setq WallThickness (vlax-ldata-get WindowDict "WallThickness"))
	
	(SetUCSToWindowCS WindowDict)
	(command ".box"
					 (list 0 0 0)
					 (list XLength YLength 0)
					 (list XLength YLength (* WallThickness Handedness))
	) ; command
	(command ".union" (handent WallEntHandle) (entlast) "")
	(UP)
	
) ; defun


; Creates a rectangular hole for a window according to the geometry of the window stored in it's window dictionary
(defun MakeWindowHole (WindowDict / WindowDict WindowCS XLength YLength Handedness FrameEntHandle GlassEntHandle WallEntHandle WallThickness)

	(setq WindowCS (vlax-ldata-get WindowDict "WindowCS"))
	(setq XLength (vlax-ldata-get WindowDict "XLength"))
	(setq YLength (vlax-ldata-get WindowDict "YLength"))
	(setq Handedness (vlax-ldata-get WindowDict "Handedness"))
	(setq WallEntHandle (vlax-ldata-get WindowDict "WallEntHandle"))
	(setq WallThickness (vlax-ldata-get WindowDict "WallThickness"))
	
	(SetUCSToWindowCS WindowDict)
	(command ".box"
					 (list 0 0 0)
					 (list XLength YLength 0)
					 (list XLength YLength (* WallThickness Handedness))
	) ; command
	(command ".subtract" (handent WallEntHandle) "" (entlast) "")
	(UP)
	
) ; defun


; Deletes window and it's window dictionary from the WINDOWDICTIONARYDICTIONARY, optionally sealing the window's
; wall hole
(defun DeleteWindowAndWindowDictionary (WindowDict SealHole / FrameEntHandle GlassEntHandle WallEntHandle WallThickness)
	
	(setq FrameEntHandle (vlax-ldata-get WindowDict "FrameEntHandle"))
	(setq GlassEntHandle (vlax-ldata-get WindowDict "GlassEntHandle"))
	(command ".erase" (handent FrameEntHandle) (handent GlassEntHandle) "")
	
	(if SealHole (SealWindowHole WindowDict))

	(setq DictionaryId (vlax-ldata-get WindowDict "Id"))
	(dictremove WINDOWDICTIONARYDICTIONARY DictionaryId)

) ; defun


; This goes through the window dictionaries in the WINDOWDICTIONARYDICTIONARY to see if Ent is part of a window.
; If it is, it returns the Window Id.  If not, it returns nil
(defun IsEntityPartOfAWindow (Ent / RetVal Continue) ; Returns WDDKey

	(setq RetVal nil)
	(setq Rewind t)
	(setq Continue t)
	(while (and (setq CurrWindowDictionaryDictionaryEntry (dictnext WINDOWDICTIONARYDICTIONARY Rewind)) Continue)
		(setq CurrWindowDict (cdr (assoc -1 CurrWindowDictionaryDictionaryEntry)))
		(setq CurrWindowFrameEnt (handent (vlax-ldata-get CurrWindowDict "FrameEntHandle")))
		(setq CurrWindowGlassEnt (handent (vlax-ldata-get CurrWindowDict "GlassEntHandle")))
		(if (and CurrWindowGlassEnt CurrWindowFrameEnt) ; in case a window entity was erased manually
			(if (or (= (Ent->Hand CurrWindowFrameEnt) (Ent->Hand Ent)) (= (Ent->Hand CurrWindowGlassEnt) (Ent->Hand Ent)))
				(progn
					(setq RetVal (vlax-ldata-get CurrWindowDict "Id"))
					(setq Continue nil)
				) ; progn
			) ; if
		) ; if
		(setq Rewind nil)
	) ; while
	RetVal
	
) ; defun
	

(defun SetUCSToWindowCS (WindowDict / WindowCS XHat YHat WindowOrigin) ; can be used when not in U0 because of trans 0 1

	(setq WindowCS (vlax-ldata-get WindowDict "WindowCS"))
	(setq XHat (reverse (cdr (reverse (Col 0 WindowCS)))))
	(setq YHat (reverse (cdr (reverse (Col 1 WindowCS)))))
	(setq WindowOrigin (reverse (cdr (reverse (Col 3 WindowCS)))))

	(U3
		(trans WindowOrigin 0 1 nil)
		(trans (VAdd WindowOrigin XHat) 0 1 nil)
		(trans (VAdd WindowOrigin YHat) 0 1 nil)
	) ; U3

) ; defun


(defun IsWindowEntInsideWall (WindowEnt WallEnt / InitialEntlastHand WindowEntCopy WallEntCopy PossIntersectionEntHand)
	
	(setq InitialEntlastHand (Ent->Hand (entlast)))
	(command ".copy" WindowEnt "" (list 0 0 0) (list 0 0 0))
	(setq WindowEntCopy (entlast))
	(command ".copy" WallEnt "" (list 0 0 0) (list 0 0 0))
	(setq WallEntCopy (entlast))
	(command ".intersect" WindowEntCopy WallEntCopy "")
	(setq PossIntersectionEntHand (Ent->Hand (entlast)))
	(if (= InitialEntlastHand PossIntersectionEntHand)
		(progn ; no new entity so no intersection
			nil
		) ; progn
		(progn ; yes new entity so yes intersection
			(command ".erase" (entlast) "")
			t
		) ; progn
	) ; if
	
) ; defun




; Diagnostic function for debugging
(defun PrintWindowDictionaryDictionary ( / Rewind )
	
	(setq Rewind t)
	(while (setq CurrWindowDictionaryDictionaryEntry (dictnext WINDOWDICTIONARYDICTIONARY Rewind))
		(setq CurrWindowDict (cdr assoc -1 CurrWindowDictionaryDictionaryEntry))
		(progn)
		(setq Rewind nil)
	) ; while
	(vlax-ldata-list WINDOWDICTIONARYDICTIONARY)
	
) ; defun


; Diagnostic function for debugging
(defun NumberOfWindowDictionaries ( / Rewind Ctr)
	
	(setq Rewind t)
	(setq Ctr 0)
	(while (setq CurrWindowDictionaryDictionaryEntry (dictnext WINDOWDICTIONARYDICTIONARY Rewind))
		(setq Ctr (1+ Ctr))
		(setq Rewind nil)
	) ; while
	Ctr
	
) ; defun


; Diagnostic function for debugging
(defun DeleteAllWindowDictionaries ( / Rewind )
	
	(setq Rewind t)
	(while (setq CurrWindowDictionaryDictionaryEntry (dictnext WINDOWDICTIONARYDICTIONARY t))
		(setq CurrWindowDict (cdr (assoc -1 CurrWindowDictionaryDictionaryEntry)))
		(dictremove WINDOWDICTIONARYDICTIONARY (vlax-ldata-get CurrWindowDict "Id"))
		(entdel CurrWindowDict)
		(setq Rewind nil)
	) ; while
	
) ; defun


; Some legacy code from earlier versions that I might re-do for convenience when iterating one subcommand
;;;(defun c:DrawWindow ( / RecEnt)
;;;	
;;;	(command ".rectang" PAUSE PAUSE)
;;;	(setq RecEnt (entlast))
;;;	(CreateNewWindowFromRectInWall RecEnt CURRENTWALLENTITY)
;;;	(command ".erase" RecEnt "")
;;;	
;;;) ; defun
;;;
;;;
;;;(defun c:DeleteWindow ()
;;;	(setq WindowEnt (car (entsel "\nselect a window to delete: ")))
;;;	(setq PossWDDKey (IsEntityPartOfAWindow WindowEnt))
;;;	(setq WindowDictEntry (dictsearch WINDOWDICTIONARYDICTIONARY PossWDDKey))
;;;	(setq WindowDict (cdr (assoc -1 WindowDictEntry)))
;;;				
;;;	(DeleteWindowAndWindowDictionary WindowDict t)
;;;	(dictremove WINDOWDICTIONARYDICTIONARY PossWDDKey)
;;;
;;;) ; defun
;;;
;;;
;;;(defun c:RectToWindow ( / RecEnt)
;;;	
;;;	(setq RectEnt (car (entsel "\nselect a rectangle: ")))
;;;	(CreateNewWindowFromRectInWall RectEnt CURRENTWALLENTITY)
;;;	(command ".erase" RecEnt "")
;;;	
;;;) ; defun