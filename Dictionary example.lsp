
; There are several basic autolisp dictionary functions:

; For the data section of the dictionary:
; vlax-ldata-put, vlax-ldata-get, vlax-ldata-list
; For the nested dictionaries section of the dictionary:
; dictadd, dictnext, dictsearch, dictremove


; and this is how you create a dictionary:
; (setq SomeNewDictionary (entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary"))))


; You should know about the (namedobjdict) too


; Example of creating and using a dictionary:

; Here's a circle entity to demonstrate how to keep track of individual entities with dictionaries:
(command "circle" (list 0 0 0) 1)
(setq CircleEnt (entlast))
(setq CircleEntDXF (entget CircleEnt))
(setq CircleHandle (cdr (assoc 5 CircleEntDXF)))

; Now we will create a new dictionary to play with:
(setq OurDict (entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary"))))
(setq DictData (entget OurDict))

; Now put in some data (any data) using vlax-ldata-put:
(vlax-ldata-put OurDict "Karla" "212 582 6218")
(vlax-ldata-put OurDict "Vec1" (list 2.5 3 8.25))
(vlax-ldata-put OurDict "Ent1" CircleEnt)
(vlax-ldata-put OurDict "Ent1Handle" CircleHandle)
(vlax-ldata-put OurDict "SomeUnnecessaryDatum" (* 5 3.25))

(vlax-ldata-get OurDict "Karla")
(vlax-ldata-get OurDict "Ent1")
(vlax-ldata-get OurDict "Ent1Handle")



(vlax-ldata-list OurDict)
(vlax-ldata-delete OurDict "SomeUnnecessaryDatum")
(vlax-ldata-list OurDict)



(vlax-ldata-list OurDict)

(setq OurDictEntDXF (entget OurDict))
(setq DictEntHandle (cdr (assoc 5 OurDictEntDXF)))
(PrintSymbolList x)

; Ok now copy the DictEntHandle, close the drawing, reopen it, and paste it into the following line:

(setq NewEntNameForDict (handent "2D6"))

; See?  you can't find it!
; It has to be "attached to something, and that something can be 1 of 2 things, I believe,
; and that is either a NORMAL (GRAPHICAL) entity, or.....  drumroll please...
; the NAMEDOBJDICT!
; which is an acnhor point in all dwgs to attach any dictionaries!
; WARNING: AUTOCAD USES THE NAMEDOBJDICT AS PART OF ITS INTERNAL INFRASTRUCTURE to store information!
; such as the layer information, and style information!  There are lots of dictionaries for internal use,
; so DONT MESS WITH THEM!  mORE ON THAT LATER

; For now, attach our dictionary to the NAMEDOBJDICT of this dwg thusly:
	(dictadd (namedobjdict) "OurDict" OurDict)
; "ourDict is the "key" of our dictionary in the (NAMEDOBJDICT),
; which is a dictionary itself, like the ROOT DICTIONARY. Why?
; Because Dictionaries are HIERARCHICAL, that is, you can ADD DICTIONARIES INSIDE DICTIONARIES
; with the dictadd command, as we did with the last executed command!  AMAZING!
; There are 2 KINDS OF INFORMATION you can have in a dictionary - the LDATA, which is accessed
; with vlax-ldata-XXX commands, and OTHER DICTIONARIES, which are accessed by the DICTXXX commands:
; DICTSEARCH, DICTADD, DICTREMOVE, and DICTNEXT!

; Lets attach an empty dictionary to "OurDict"

(setq SomeOtherDict (entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary"))))
(vlax-ldata-put SomeOtherDict "SomeOtherData" "You talkin' to me???")
(dictadd OurDict "SomeOtherDict" SomeOtherDict)

; If you try to store a dictionary in a dictionary with the vlax-ldata-put command, it will NOT bE
; PERSISTENT, ie next time youopen up that drawing after it was closed, that entry will not be there
; ir be valid

; It's a best practice to create a BASE DICTIONARY for a program that uses a dictionary and then
; attach it to the (namedobjdict) as I do in my porgrams, and then add whatever other dictionaries
; you need to that dictionary!

; allright, so you can find your dictionary (in this drawing's NEXT AutoCAD session) like this:

(setq OurDictInfo (dictsearch (namedobjdict) "OurDict"))
(setq OurDict (cdr (assoc -1 OurDictInfo)))

; And look all the data's still there!
(vlax-ldata-list OurDict)

; And look, that other dictionary in "OurDict" is still there -
(setq SomeOtherDictInfo (dictsearch OurDict "SomeOtherDict"))
(setq SomeOtherDict (cdr (assoc -1 SomeOtherDictInfo)))

(vlax-ldata-list SomeOtherDict)

; Now, just so you kow, and to be complete, you can iterate through the (sub-)dictionaries;
; with the DICTNEXT command, which works EXACTLY like the entnext command does with modelspace:


; Here are some functions I created to make handling dictionaries easier:

(defun CreateDictionary (/ Dict)
	
	(entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary")))
	
) ; defun

(defun ChildDictionary (Dict Key)

	(cdr (assoc -1 (dictsearch Dict Key)))

) ; defun


(defun CreateChildDictionary (ParentDict Key DictionaryDefinition / NewChildDict DictDefinitionItem CurrDictDefinitionKey CurrVarSpaceDefinitionKeyType)

	(setq NewChildDict (CreateDictionary))
	(dictadd
		ParentDict
		Key
		NewChildDict
	) ; dictadd
	(if DictionaryDefinition
		(progn
			(foreach DictDefinitionItem DictionaryDefinition
				(setq CurrDictDefinitionKey (car DictDefinitionItem))
				(setq CurrDictDefinitionKeyType (cdr DictDefinitionItem))
				(vlax-ldata-put NewChildDict CurrDictDefinitionKey (DefaultTypeValue CurrDictDefinitionKeyType))
			) ; foreach
		); progn
	) ; if

	NewChildDict

) ; defun


(defun DefaultTypeValue (VarType)

	(cond
		(
		 (= VarType "REAL")
		 0.0
		)
		(
		 (= VarType "INT")
		 0
		)
		(
		 (= VarType "STR")
		 ""
		)
		(
		 (= VarType "LIST")
		 nil
		)
		(
		 (= VarType "BOOL")
		 nil
		)
	) ; cond
	
) ; defun