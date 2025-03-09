
(defun ToggleSnapsOff ()

	(setq TEMPSNAPMODESAVE (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	
) ; defun


(defun ToggleSnapsOn ()
	
	(setvar "OSMODE" TEMPSNAPMODESAVE)
	
) ; defun


(defun LoadStandardErrorRoutine ()
	
	(defun *error* (msg)
		(princ (strcat "\nError: " msg))
		(SetUserInput)
		(ToggleSnapsOn)
		
	) ; defun
	
) ; defun


(defun UnloadStandardErrorRoutine ()
	
	(setq *error* nil)
	 
) ; defun


(defun Get3DWCSPointsFromLWPline (Pline)
	
	(setq 2DECSPts (Get2DECSPointsFromWLPline Pline))
	(setq EntECS (GetECSTransformMatrix Pline))
	(setq 4sp2DECSPts (3sp->4sp 2DECSPts))
	(setq 4sp3DWCSPts (MMult EntECS 4sp2DECSPts))
	(setq 3sp3DWCSPts (4sp->3sp 4sp3DWCSPts))
	
) ; defun


(defun Draw4spPoints (4spPoints / 3spPoints)
	(setq 3spPoints (4sp->3sp 4spPoints))
	(Draw3spPoints 3spPoints)
) ; defun


(defun Ent->Hand (ent)
	(cdr (assoc 5 (entget ent)))
) ; defun


(defun ClearUserInput ()
	
	(setq USERINPUTSETTINGS
		(list
			(getvar "osmode")
			(getvar "3dosmode")
			(getvar "ucsdetect")
			(getvar "clayer")
			(getvar "attreq")
			(getvar "attdia")
			(getvar "cmdecho")
			(getvar "regenmode")
			(getvar "gridmode")
		) ; list
	) ; setq
	
	(setvar "osmode" 0)
	(setvar "3dosmode" 0)
	;(setvar "ucsdetect" 1)
	(setvar "attreq" 0)
	(setvar "attdia" 0)
	(setvar "cmdecho" 0)
	(setvar "regenmode" 0)
	(setvar "gridmode" 0)
			
) ; defun


(defun SetUserInput ()
	
	(setvar "osmode" (nth 0 USERINPUTSETTINGS))
	(setvar "3dosmode" (nth 1 USERINPUTSETTINGS))
	(setvar "ucsdetect" (nth 2 USERINPUTSETTINGS))
	(setvar "clayer" (nth 3 USERINPUTSETTINGS))
	(setvar "attreq" (nth 4 USERINPUTSETTINGS))
	(setvar "attdia" (nth 5 USERINPUTSETTINGS))
	(setvar "cmdecho" (nth 6 USERINPUTSETTINGS))
	(setvar "regenmode" (nth 7 USERINPUTSETTINGS))
	(setvar "gridmode" (nth 8 USERINPUTSETTINGS))
	
) ; defun


(defun UniqueID ( / RetVal)
	
	(command ".point" "0,0,0")
	(setq RetVal (cdr (assoc 5 (entget (entlast)))))
	(command ".erase" (entlast) "")
	RetVal
	
) ; defun


(defun CopyInPlace (SSetOrEntity / Marker1 RegionEnts) ; copy stuff in-place

	(Copy SSetOrEntity (list 0 0 0) (list 0 0 0))
	
) ; defun


(defun Copy (SSetOrEntity P1 P2 / Marker1 RegionEnts)

	(command ".point" (list 0 0 0))
	(setq Marker1 (entlast))
	(command ".copy" SSetOrEntity "" P1 P2)
	(setq RegionEnts (GetSSFromMarker Marker1))

	RegionEnts
	
) ; defun


(defun GetSSFromMarker (markedEntity / ssRet)
	
	(GetEntitiesFromMarker markedEntity nil)

) ; defun


(defun GetEntitiesFromMarker (Marker EntityType / RetValYY CurrEnt EntList NewRetVal CurrEntDXF)
	
  (setq RetValYY (ssadd))
	(setq CurrEnt Marker)
	
  (while (setq CurrEnt (entnext CurrEnt))
		(setq CurrEntDXF (entget CurrEnt))

	  ;;If CurrEnt is a block reference with attributes
	  ;;move forward in database through the attributes first
	  (if
			(and
			    (= (cdr (assoc 0 CurrEntDXF)) "INSERT")   
			    (= (cdr (assoc 66 CurrEntDXF)) 1)
			) ; and
			(progn
				(setq RetValYY (ssadd CurrEnt RetValYY))
				(while
					(/=
						(cdr (assoc 0 (setq CurrEntDXF (entget (setq CurrEnt (entnext CurrEnt))))))
						"SEQEND"
					) ; /=
					(progn)
				) ; while
			) ; progn
		(setq RetValYY (ssadd CurrEnt RetValYY))
	  ) ; if
	) ; while

	; Filter Time?
	(if EntityType
		(progn
			(setq NewRetVal (ssadd))
			(setq EntList (SSet->List RetValYY))
			(foreach Ent Entlist
				(if (= (cdr (assoc 0 (entget Ent))) EntityType)
					(setq NewRetVal (ssadd Ent NewRetVal))
				) ; if
			) ; foreach
			(setq RetValYY NewRetVal)
		) ; progn
	) ; if

 	(command ".erase" Marker "")
	
  (if (> (sslength RetValYY) 0)
    RetValYY
    nil
  )
	
) ; defun


(defun SpatialConflictSolid (SolidEnameA SolidEnameB / InitialEntlastHand SolidEnameACopy SolidEnameBCopy PossIntersectionEntHand)
	
	(setq InitialEntlastHand (Ent->Hand (entlast)))
	(command ".copy" SolidEnameA "" (list 0 0 0) (list 0 0 0))
	(setq SolidEnameACopy (entlast))
	(command ".copy" SolidEnameB "" (list 0 0 0) (list 0 0 0))
	(setq SolidEnameBCopy (entlast))
	(command ".intersect" SolidEnameACopy SolidEnameBCopy "")
	(setq PossIntersectionEntHand (Ent->Hand (entlast)))
	(if (= InitialEntlastHand PossIntersectionEntHand)
		(progn ; no new entity so no intersection
			nil
		) ; progn
		(progn ; yes new entity so yes intersection
			(entlast)
		) ; progn
	) ; if
	
) ; defun


(defun Get3DSolidVolume (3DSolid)

	(vlax-get (vlax-ename->vla-object 3DSolid) 'volume)

) ; defun

(defun GetPLineArea (PLine)

	(vlax-get (vlax-ename->vla-object PLine) 'area)

) ; defun


(defun Get2DECSPointsFromWLPline (Pline)
	
	(setq RetVal (list))
	(setq PlineDXF (entget Pline))
	(while PlineDXF
		(setq CurrDotPair (car PlineDXF))
		(setq PlineDXF (cdr PlineDXF))
		(if (= (car CurrDotPair) 10)
			(progn
				(setq 2DPt (cdr CurrDotPair))
				(setq 3DPt (list (nth 0 2DPt) (nth 1 2DPt) 0))
				(setq RetVal (cons 3DPt RetVal))
			) ; progn
		) ; if
	) ; while
	(reverse RetVal)
	
) ; defun


(defun 3sp->4sp (Vects / Dim0 Dim1 Dim2 Dim3 CurrVect)

	(setq Dim0 (list))
	(setq Dim1 (list))
	(setq Dim2 (list))
	(setq Dim3 (list))
	(While Vects
		(setq CurrVect (car Vects))
		(setq Vects (cdr Vects))
		(setq Dim0 (cons (nth 0 CurrVect) Dim0))
		(setq Dim1 (cons (nth 1 CurrVect) Dim1))
		(setq Dim2 (cons (nth 2 CurrVect) Dim2))
		(setq Dim3 (cons 1 Dim3))
	) ; while
	(list
		(reverse Dim0)
		(reverse Dim1)
		(reverse Dim2)
		Dim3
	) ; list

) ; defun


(defun 4sp->3sp (Vects)
	(Transpose (reverse (cdr (reverse Vects))))
) ; defun


(defun MMult (A B / RetVal RowMax ColMax RowCtr ColCtr) ; Main matrix multiplication routine
	
	(if (/= (length (nth 0 A)) (length B))
		(progn
			(princ "\nCannot Multiply these 2 matrices.  Exiting")
			(exit)
		) ; progn
	) ; if
	(setq RowMax (1- (length B)))
	(setq ColMax (1- (length (nth 0 B))))
	(setq RowCtr RowMax)
	(while (>= RowCtr 0)
		(setq ColCtr ColMax)
		(setq CurrRow (list))
		(setq ColCtr ColMax)
		(while (>= ColCtr 0)
			(Setq DotProd (MVDot (Row RowCtr A) (Col ColCtr B)))
			(setq CurrRow (cons DotProd CurrRow))
			(setq ColCtr (1- ColCtr))
		) ; while
		(setq RetVal (cons CurrRow RetVal))
		(setq RowCtr (1- RowCtr))
	) ; while
	RetVal
		
) ; defun


(defun GetECSTransformMatrix (Ent / EntDXF ZAxis YAxis XAxis Elevation)
	
	(setq EntDXF (entget Ent))
	(setq ZAxis (cdr (assoc 210 EntDXF)))
	(if (= (nth 2 ZAxis) 1)
		(progn
			(setq YAxis (list 0 1 0))
			(setq XAxis (list 1 0 0))
		) ; progn
		(progn
			(setq XAxis (UnitVect (VCross (list 0 0 1) ZAxis)))
			(setq YAxis (UnitVect (VCross ZAxis XAxis)))
		) ; progn
	) ; if
	(setq Elevation (cdr (assoc 38 EntDXF)))
	(if (= Elevation nil) (setq Elevation 0))

	(list
		(list (nth 0 XAxis) (nth 0 YAxis) (nth 0 ZAxis) (* (nth 0 ZAxis) Elevation))
		(list (nth 1 XAxis) (nth 1 YAxis) (nth 1 ZAxis) (* (nth 1 ZAxis) Elevation)) 
		(list (nth 2 XAxis) (nth 2 YAxis) (nth 2 ZAxis) (* (nth 2 ZAxis) Elevation)) 
		(list 0 0 0 1)
	) ; list
	
) ; defun


(defun VCross (A B)
	
  (list
    (- (* (nth 1 A) (nth 2 B)) (* (nth 2 A) (nth 1 B)))
    (- (* (nth 2 A) (nth 0 B)) (* (nth 0 A) (nth 2 B)))
    (- (* (nth 0 A) (nth 1 B)) (* (nth 1 A) (nth 0 B)))
  ) ; list
	
) ; defun


(defun VLength (Vect)
	
	(sqrt
		(+
			(Sqr (nth 0 Vect))
			(Sqr (nth 1 Vect))
			(Sqr (nth 2 Vect))
		) ; +
	) ; sqrt
	
) ; defun


(defun UnitVect (Vect / VLen)
	
	(setq VLen (VLength Vect))
	(list
		(/ (nth 0 Vect) Vlen)
		(/ (nth 1 Vect) Vlen)
		(/ (nth 2 Vect) Vlen)
	); list
	
) ; defun


(defun Sqr (x) (* x x))


(defun Row (n A)
	(nth n A)
) ; defun


(defun Col (n A / Ctr RetVal)
	
	(setq RetVal (list))
	(setq Ctr (1- (length A)))
	(while (<= 0 Ctr)
		(setq RetVal (cons (nth n (nth Ctr A)) RetVal))
		(setq Ctr (1- Ctr))
	) ; while
	RetVal
	
) ; defun


(defun MVDot (A B / RetVal ALen BLen Ctr RetVal)
	
	(setq RetVal 0)
	(setq ALen (Length A) BLen (Length B))
	(if (/= ALen BLen) (exit))
	
	(setq Ctr 0)
	(while (< Ctr BLen)
		(setq RetVal (+ RetVal (* (nth Ctr A) (nth Ctr B))))
		(setq Ctr (1+ Ctr))
	) ; while
	RetVal
	
) ; defun


(defun Transpose (M)
	
	(setq RetVal (list))
	(setq RowMax (1- (length (nth 0 M))))
	(setq ColMax (1- (length M)))
	(setq RowCtr RowMax)
	(setq ColCtr ColMax)
	(while (>= RowCtr 0)
		(setq RetVal (cons (Col RowCtr M) RetVal))
		(setq RowCtr (1- RowCtr))
	) ; while
	RetVal
	
) ; defun


(defun VAdd (A B)
	
  (list
    (+ (nth 0 A) (nth 0 B))
    (+ (nth 1 A) (nth 1 B))
    (+ (nth 2 A) (nth 2 B))
  ) ; list
	
) ; defun


(defun VSubtr (A B)
	
  (list
    (- (nth 0 A) (nth 0 B))
    (- (nth 1 A) (nth 1 B))
    (- (nth 2 A) (nth 2 B))
  ) ; list
	
) ; defun




(defun CreateDictionary (/ Dict)
	
	(entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary")))
	
) ; defun


(defun VDot (A B)
	
  (+
    (* (nth 0 A) (nth 0 B))
    (* (nth 1 A) (nth 1 B))
    (* (nth 2 A) (nth 2 B))
  ) ; list
	
) ; defun


(defun ChildDictionary (Dict Key)

	(cdr (assoc -1 (dictsearch Dict Key)))

) ; defun


(defun PrintSymbolList (SymbolList / Item) ; Note: I tink this can only handle atom dita types

	(foreach Item SymbolList
		(prin1 Item)
		(princ "\n")
	)


) ; defun


(defun ZRotate4Sp (Ang)

	(list
		(list (cos Ang) (- (sin Ang)) 0 0)
		(list (sin Ang) (cos Ang)     0 0)
		(list 0         0             1 0)
		(list 0         0             0 1)
	) ; list

) ; defun



(defun XCoord (Vect)
	(nth 0 Vect)
) ; defun

(defun YCoord (Vect)
	(nth 1 Vect)
) ; defun

(defun ZCoord (Vect)
	(nth 2 Vect)
) ; defun

(defun Offset4Sp (3SpVect)

	(list
		(list 1 0 0 (XCoord 3SpVect))
		(list 0 1 0 (YCoord 3SpVect))
		(list 0 0 1 (ZCoord 3SpVect))
		(list 0 0 0 1)
	) ; list

) ; defun


(defun UCSFromTransformMatrix (XFormMatrix)

	(setq 3DVects (4Sp->3Sp XFormMatrix))
	(setq XHat (nth 0 3DVects))
	(setq YHat (nth 1 3DVects))
	(setq ZHat (nth 2 3DVects))
	(setq Offset (nth 3 3DVects))

	(U3 Offset (VAdd Offset XHat) (VAdd Offset YHat))

) ; defun


(defun c:UM ()
	(UM (getpoint "\nNew UCS origin:"))
) ; defun

(defun c:U0 ()
	(U0)
) ; defun

(defun c:UE ( / Ent)
	(setq Ent (entsel))
	(UE Ent)
) ; defun

(defun c:UP ()
	(UP)
) ; defun

(defun c:UX ()
	(UX)
) ; defun

(defun c:UY ()
	(UY)
) ; defun

(defun c:UZ ()
	(UZ)
) ; defun


(defun c:U3 ()
	(command ".ucs" "3")
) ; defun


(defun U3 (P1 P2 P3)
	(command ".ucs" "3" P1 P2 P3)
) ; defun

(defun UX ()
	(command ".ucs" "x" "90")
) ; defun

(defun UY ()
	(command ".ucs" "y" "90")
) ; defun

(defun UZ ()
	(command ".ucs" "z" "90")
) ; defun


(defun U0 ()
	(command "ucs" "")
) ; defun


(defun UM (Pt)
	(command ".ucs" "m" Pt)
) ; defun


(defun U3 (P1 P2 P3)
	(command ".ucs" "3" P1 P2 P3)
) ; defun


(defun UP ()
	(command "ucs" "p")
) ; defun


(defun ZRotate4Sp (Ang)

	(list
		(list (cos Ang) (- (sin Ang)) 0 0)
		(list (sin Ang) (cos Ang)     0 0)
		(list 0         0             1 0)
		(list 0         0             0 1)
	) ; list

) ; defun


(defun Draw3spPoints (3spPoints bClose / Ctr NumPts)
	(command ".line" (car 3spPoints))
	(setq 3spPoints (cdr 3spPoints))
	(foreach pt 3spPoints (command pt))
	(command (if bClose "c" ""))
) ; defun


(defun Print4xNMatrix (4x4Matrix LowerLeftPoint / CurrX CurrY TextSize HorizSpacing VertSpacing
											 Ctr Line NumCols Ctr2 CloseParenPoint OpenParenPoint OpenParenXCoord)

	(setq 4x4Matrix (reverse 4x4Matrix))
	(setq CurrX (XCoord LowerLeftPoint))
	(setq CurrY (YCoord LowerLeftPoint))
	(setq TextSize (getvar "TextSize"))
	(setq HorizSpacing (* 8 TextSize))
	(setq VertSpacing (* 1.5 TextSize))
	(setq Ctr 0)
	(while (< Ctr 4)
		(setq Line (nth Ctr 4x4Matrix))
		(setq NumCols (length Line))
		(setq Ctr2 0)
		(while (< Ctr2 NumCols)
			
			(command "text" "s" "Monotxt" "s" "Monotxt" "j" "Left" (list CurrX CurrY 0) TextSize 0 (rtos (nth Ctr2 Line) 2 3))
			(setq CurrX (+ CurrX HorizSpacing))
			(setq LastCurrX CurrX)
			(setq Ctr2 (1+ Ctr2))
		) ; while
		(setq CurrY (+ CurrY VertSpacing))
		(setq CurrX (XCoord LowerLeftPoint))
		(setq Ctr (1+ Ctr))
	) ; while
	(setq CloseParenPoint (list LastCurrX (YCoord LowerLeftPoint) 0))
	(command "text" "s" "Monotxt" "s" "Monotxt" "j" "Left" CloseParenPoint (* TextSize 6)  0 ")")
	(setq OpenParenXCoord (- (XCoord LowerLeftPoint) (* HorizSpacing 0.3)))
	(setq OpenParenPoint (list OpenParenXCoord (YCoord LowerLeftPoint) 0))
	(command "text" "s" "Monotxt" "s" "Monotxt" "j" "Left" OpenParenPoint (* TextSize 6) 0 "(")
	(setvar "textsize" TextSize)

	CloseParenPoint

) ; defun
