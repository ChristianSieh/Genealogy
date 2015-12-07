#| 
	***** genealogy.lsp *****

Illustrates file I/O in Lisp.

Author: John M. Weiss, Ph.D.
Class:  CSC461 Programming Languages
Date:   Fall 2015

|#

(setf database nil)

(defstruct person name gender parents children)

(defun setstruct ( line )
	
	(let ((struct (make-person :name (car line) :gender (nth 1 line) 
		:parents (nth 2 line) :children (nth 3 line))
	     ))	
	
		(push struct database)
	)
)

; fileio function
; Original Auther: Dr. John Weiss
; Modified by: Christian Sieh
(defun fileio ( filename )
    ; check for correct usage
    (when (null filename) (return-from fileio "Usage: fileio.lsp filename"))

    ; read through file using with-open-file
    (with-open-file (fin filename :if-does-not-exist nil)
        (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
	(do 
		((line (read fin nil) (read fin nil)))
		((null line))
		(setstruct line)
	)
    )
)

#|
	*****main.lsp *****

Emulate a main function in Lisp, with command-line arguments.

Author: John M. Weiss, Ph.D.
Class:  CSC461 Programming Languages
Date:   Fall 2015

|#

; main function
(defun main ( args )	
	; call the fileio function, passing the first command-line argument as an input filename
	(fileio (car *args*))
)

; call the main function, passing command-line arguments
(main *ARGS*)

(defun myfind ( name )
	(dolist
		(item database struct)
		(when (equalp name (person-name item))
			(let ((struct item))
				(return struct)
			)
		)	
	)
)

(defun parents ( name )
	(person-parents (myfind name))
)

(defun children ( name )
	(person-children (myfind name))
)

(defun siblings ( name )
	(let ((tempParents (parents name)))

		(let ((tempSiblings))
			(dolist (parent tempParents)
				(setf tempSiblings (nconc (children parent) tempSiblings))
			)

			(setf tempSiblings (delete-duplicates tempSiblings))
			(delete name tempSiblings)
		)
	)
)

(defun grandchildren ( name )
	(let ((tempChildren (children name)))
		(let ((tempGrand))
			(dolist (child tempChildren)
				(setf tempGrand (nconc (children child) tempGrand))
			)

			(setf tempGrand (remove-duplicates tempGrand))
		)
	)
)

(defun grandparents ( name )
	(let ((tempParents (parents name)))

		(let ((tempGrand))
			(dolist (parent tempParents)
				(setf tempGrand (nconc (parents parent) tempGrand))
			)

			(setf tempGrand (remove-duplicates tempGrand))
		)
	)
)

(defun auntsUncles ( name )
	(let ((tempParents (parents name)))

		(let ((tempAuntUncle))
			(dolist (parent tempParents)
				(setf tempAuntUncle (nconc (siblings parent) tempAuntUncle))
			)

			(setf tempAuntUncle (remove-duplicates tempAuntUncle))
		)
	)
)

(defun niecesNephews ( name )
	(let ((tempSiblings (siblings name)))

		(let ((tempNiecesNephews))
			(dolist (sibling tempSiblings)
				(setf tempNiecesNephews (nconc (children sibling) tempNiecesNephews))
			)

			(setf tempNiecesNephews (remove-duplicates tempNiecesNephews))
		)
	)
)

(defun cousins ( name )
	(let ((tempAuntsUncles (auntsUncles name)))

		(let ((tempCousins))
			(dolist (auntUncle tempAuntsUncles)
				(setf tempCousins (nconc (children auntUncle) tempCousins))
			)

			(setf tempCousins (remove-duplicates tempCousins))
		)
	)
)

(defun ancestors ( name )

	(if (null (parents name))
		()
		(let ((tempParents (parents name)))
			(let ((tempAncestors tempParents))
				(dolist (parent tempParents) 
					(setf tempAncestors (nconc (ancestors parent) tempAncestors))
				)

				(setf tempAncestors (remove-duplicates tempAncestors))
			)
		)
	)
)

(defun descendants ( name )

	(if (null (children name))
		()
		(let ((tempChildren (children name)))
			(let ((tempDescendants tempChildren))
				(dolist (child tempChildren)
					(setf tempDescendants (nconc (descendants child) tempDescendants))
				)

				(setf tempDescendants (remove-duplicates tempDescendants))
			)
		)
	)
)

(defun malefilter ( lst )

	(dolist (name lst)
		(if (equalp (person-gender (myfind name)) 'female)
			(setf lst (delete name lst))
		)
	)

	(values lst)
)

(defun femalefilter ( lst )
	(dolist (name lst)
		(if (equalp (person-gender (myfind name)) 'male)
			(setf lst (delete name lst))
		)
	)

	(values lst)
)	

(defun mothers ( name )
	(let ((mother (parents name)))
		(setf mother (femalefilter mother))
	)
)

(defun fathers ( name )
	(let ((father (parents name)))
		(setf father (malefilter father))
	)
)

(defun sons ( name )
	(let ((son (children name)))
		(setf son (malefilter son))
	)
)

(defun daughters ( name )
	(let ((daughter (children name)))
		(setf daughter (femalefilter daughter))
	)
)

(defun sisters ( name )
	(let ((sister (siblings name)))
		(setf sister (femalefilter sister))
	)
)

(defun brothers ( name )
	(let ((brother (siblings name)))
		(setf brother (malefilter brother))
	)
)

(defun grandfathers ( name )
	(let ((grandfather (grandparents name)))
		(setf grandfather (malefilter grandfather))
	)
)

(defun grandmothers ( name )
	(let ((grandmother (grandparents name)))
		(setf grandmother (femalefilter grandmother))
	)
)

(defun grandsons ( name )
	(let ((grandson (grandchildren name)))
		(setf grandson (malefilter grandson))
	)
)

(defun granddaughters ( name )
	(let ((granddaughter (grandchildren name)))
		(setf granddaughter (femalefilter granddaughter))
	)
)

(defun uncles ( name )
	(let ((uncle (auntsUncles name)))
		(setf uncle (malefilter uncle))
	)
)

(defun aunts ( name )
	(let ((aunt (auntsUncles name)))
		(setf aunt (femalefilter aunt))
	)
)

(defun nieces ( name )
	(let ((niece (niecesNephews name)))
		(setf niece (femalefilter niece))
	)
)

(defun nephews ( name )
	(let ((nephew (niecesNephews name)))
		(setf nephew (malefilter nephew))
	)
)

(defun male-cousins ( name )
	(let ((maleCousin (cousins name)))
		(setf maleCousin (malefilter maleCousin))
	)
)

(defun female-cousins ( name )
	(let ((femaleCousin (cousins name)))
		(setf femaleCousin (femalefilter femaleCousin))
	)
)

(defun male-ancestors ( name )
	(let ((maleAncestor (ancestors name)))
		(setf maleAncestor (malefilter maleAncestor))
	)
)

(defun female-ancestors ( name )
	(let ((femaleAncestor (ancestors name)))
		(setf femaleAncestor (femalefilter femaleAncestor))
	)
)

(defun male-descendants ( name )
	(let ((maleDescendant (descendants name)))
		(setf maleDescendant (malefilter maleDescendant))
	)
)

(defun female-descendants ( name )
	(let ((femaleDescendant (descendants name)))
		(setf femaleDescendant (femalefilter femaleDescendant))
	)
)
