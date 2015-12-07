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

	(print struct)
	;(print (person-name struct))
	;(print (person-gender struct))
	;(print (person-parents struct))
	;(print (person-children struct))
	)
)

; fileio function
; Original Auther: Dr. John Weiss
; Modified by: Christian Sieh
(defun fileio ( filename )
    "(fileio filename): open an input file and read the data"

    ; check for correct usage
    (when (null filename) (return-from fileio "Usage: fileio.lsp filename"))

    ; read through file using with-open-file
    (format t "~%Opening file ~a using with-open-file~%" filename)
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
	"(main args): emulate a main function, called with command-line args"
	(format t "~D command line arguments: ~A" (length args) args)


	
	; call the fileio function, passing the first command-line argument as an input filename
	(format t "~a~%" (fileio (car *args*)))
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

(defun maleFilter ( lst )

)

(defun femaleFilter ( lst )

)	




