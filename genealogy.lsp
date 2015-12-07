#| 
	***** genealogy.lsp *****

Author: Christian Sieh
Class:  CSC461 Programming Languages
Date:   Fall 12/6/2015
Usage:  clisp -repl genealogy.lsp file.dat

This program will open and read each line. The read in line will then be
mapped onto a person struct and pushed on to the global variable 
*database*. After the database is built you will then be able to run
queries against the database by using the repl. An example query is:
(parents 'mike). This function will return a list with Mike's parents.
If Mike had no parents :'( then nil is returned instead. The queries
are not case sensitive and if a name that is not in the database
is entered into the query nil will be returned.
|#

;Global variable that will hold all of the person structs
(setf database nil)

;The struct we will use to hold each person's infromation
(defstruct person name gender parents children)

#|
 | Author: Christian Sieh
 | This function takes a list (in our case a line from a file) and
 | converts the list into a person struct. Then the struct is pushed
 | into the database list
 |#
(defun setstruct ( line )
	
	(let ((struct (make-person :name (car line) :gender (nth 1 line) 
		:parents (nth 2 line) :children (nth 3 line))
	     ))	
	
		(push struct database)
	)
)

#|
 | Original Author: Dr. John Weiss
 | Modified by: Christian Sieh
 | This function opens and reads lists from a file as well as handling error
 | checking. The read in line from the file is then passed to the 
 | setstruct function
 |#
(defun fileio ( filename )
    ; check for correct usage
    (when (null filename) (return-from fileio "Usage: fileio.lsp filename"))

    ; read through file using with-open-file
    (with-open-file (fin filename :if-does-not-exist nil)
        (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
	(do 
		;read in a line
		((line (read fin nil) (read fin nil)))
		;if line is null then exit do loop
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

#|
 | Author: Christian Sieh
 | This function given a name will search the database for all
 | structures matching that name and then return the struct
 |#
(defun myfind ( name )
	(dolist
		(item database)
		;When the name is found in a sturct in the database
		;return the found struct
		(when (equalp name (person-name item))
			(let ((struct item))
				(return struct)
			)
		)	
	)
)

#|
 | Author: Christian Sieh
 | This function will find the structure for the given name and then
 | return that structures parents field.
 |#
(defun parents ( name )
	(let ((foundStruct (myfind name)))
		(if (null foundStruct)
			;if parents are nil return nil
			()
			;return parents
			(person-parents foundStruct)
		)
	)
)

#| Author: Christian Sieh
 | This function will find the structure for the given name and then
 | return that structures children field.
 |#
(defun children ( name )
	(let ((foundStruct (myfind name)))
		(if (null foundStruct)
			;if children are nil return nil
			()
			;return children
			(person-children (myfind name))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will go through the database and return the siblings
 | for the name give
 |#
(defun siblings ( name )
	(let ((tempParents (parents name)))

		(let ((tempSiblings))
			;for each parent get their children and add them to
			;the tempSiblings list
			(dolist (parent tempParents)
				(setf tempSiblings (nconc (children parent) tempSiblings))
			)

			;remove duplicate sibling names form list
			(setf tempSiblings (delete-duplicates tempSiblings))
			;remove name from list since name isn't one of
			;name's siblings
			(delete name tempSiblings)
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get the grandchildren for the name supplied
 |#
(defun grandchildren ( name )
	;Get name's children
	(let ((tempChildren (children name)))
		(let ((tempGrand))
			;For each child, get their children and add to list
			(dolist (child tempChildren)
				(setf tempGrand (nconc (children child) tempGrand))
			)

			(setf tempGrand (remove-duplicates tempGrand))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get the grandparents for the name supplied
 |#
(defun grandparents ( name )
	;Get name's parents
	(let ((tempParents (parents name)))
		(let ((tempGrand))
			;For each parent, get their parents and add to list
			(dolist (parent tempParents)
				(setf tempGrand (nconc (parents parent) tempGrand))
			)

			(setf tempGrand (remove-duplicates tempGrand))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get the aunts and uncles for the name supplied
 |#
(defun auntsUncles ( name )
	;Get name's parents
	(let ((tempParents (parents name)))
		(let ((tempAuntUncle))
			;For each parent get their siblings
			(dolist (parent tempParents)
				(setf tempAuntUncle (nconc (siblings parent) tempAuntUncle))
			)

			(setf tempAuntUncle (remove-duplicates tempAuntUncle))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get the nieces and nephews for the name supplied
 |#
(defun niecesNephews ( name )
	;Get name's siblings
	(let ((tempSiblings (siblings name)))
		(let ((tempNiecesNephews))
			;For each sibling get their children
			(dolist (sibling tempSiblings)
				(setf tempNiecesNephews (nconc (children sibling) tempNiecesNephews))
			)

			(setf tempNiecesNephews (remove-duplicates tempNiecesNephews))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get the cousins for the name supplied
 |#
(defun cousins ( name )
	;Get name's aunts and uncles
	(let ((tempAuntsUncles (auntsUncles name)))
		(let ((tempCousins))
			;For each aunt/uncle get their children
			(dolist (auntUncle tempAuntsUncles)
				(setf tempCousins (nconc (children auntUncle) tempCousins))
			)

			(setf tempCousins (remove-duplicates tempCousins))
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get all the ancestors (name's parents and name's 
 | parent's parents and etc.) for the name supplied
 |#
(defun ancestors ( name )
	(if (null (parents name))
		;If name doesn't have parents return nil
		()
		;Get name's parents
		(let ((tempParents (parents name)))
			;Add name's parents to their ancestors
			(let ((tempAncestors tempParents))
				;For each parent look up their ancestors and add to list
				(dolist (parent tempParents) 
					(setf tempAncestors (nconc (ancestors parent) tempAncestors))
				)

				(setf tempAncestors (remove-duplicates tempAncestors))
			)
		)
	)
)

#|
 | Author: Christian Sieh
 | This function will get all the descendants (name's children and name's
 | children's children and etc.) for the name supplied
 |#
(defun descendants ( name )

	(if (null (children name))
		;If name doesn't have children return nil
		()
		;Get name's children
		(let ((tempChildren (children name)))
			;Add name's children to their descendants
			(let ((tempDescendants tempChildren))
				;For each child look up their descendants and add to list
				(dolist (child tempChildren)
					(setf tempDescendants (nconc (descendants child) tempDescendants))
				)

				(setf tempDescendants (remove-duplicates tempDescendants))
			)
		)
	)
)

#|
 | Author: Christian Sieh
 | This function when given a list of persons (the struct defined at the top
 | of this program) will remove everyone that is female from the list
 |#
(defun malefilter ( lst )
	;For each name in the list check if the person struct's gender is
	;female. If the gender is female then remove it from the list
	(dolist (name lst)
		(if (equalp (person-gender (myfind name)) 'female)
			(setf lst (delete name lst))
		)
	)

	;This is needed since we want to return lst but it
	;needs to be the last thing evaluated
	(values lst)
)

#|
 | Author: Christian Sieh
 | This function when given a list of persons (the struct defined at the top
 | of this program) will remove everyone that is male from the list
 |#
(defun femalefilter ( lst )
	;For each name in the list check if the person struct's gender is
	;male. If the gender is male then remove it from the list
	(dolist (name lst)
		(if (equalp (person-gender (myfind name)) 'male)
			(setf lst (delete name lst))
		)
	)

	;This is needed since we want to return lst but it
	;need sto be the last thing evaluated
	(values lst)
)	

#|
 | Author: Christian Sieh
 | This function uses the parents and femalefilter functions to return
 | name's mother
 |#
(defun mothers ( name )
	(let ((mother (parents name)))
		(setf mother (femalefilter mother))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the parents and malefilter functions to return
 | name's father
 |#
(defun fathers ( name )
	(let ((father (parents name)))
		(setf father (malefilter father))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the children and malefilter functions to return
 | name's sons
 |#
(defun sons ( name )
	(let ((son (children name)))
		(setf son (malefilter son))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the children and femalefilter functions to return
 | name's daughters
 |#
(defun daughters ( name )
	(let ((daughter (children name)))
		(setf daughter (femalefilter daughter))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the siblings and femalefilter functions to return
 | name's sisters
 |#
(defun sisters ( name )
	(let ((sister (siblings name)))
		(setf sister (femalefilter sister))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the siblings and malefilter functions to return
 | name's brothers
 |#
(defun brothers ( name )
	(let ((brother (siblings name)))
		(setf brother (malefilter brother))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the grandparents and malefilter functions to return
 | name's grandfathers
 |#
(defun grandfathers ( name )
	(let ((grandfather (grandparents name)))
		(setf grandfather (malefilter grandfather))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the grandparent and femalefilter functions to return
 | name's grandmothers
 |#
(defun grandmothers ( name )
	(let ((grandmother (grandparents name)))
		(setf grandmother (femalefilter grandmother))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the grandchildren and malefilter functions to return
 | name's grandsons
 |#
(defun grandsons ( name )
	(let ((grandson (grandchildren name)))
		(setf grandson (malefilter grandson))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the grandchildren and femalefilter functions to return
 | name's granddaughters
 |#
(defun granddaughters ( name )
	(let ((granddaughter (grandchildren name)))
		(setf granddaughter (femalefilter granddaughter))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the auntsUncles and malefilter functions to return
 | name's uncles
 |#
(defun uncles ( name )
	(let ((uncle (auntsUncles name)))
		(setf uncle (malefilter uncle))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the auntsUncles and femalefilter functions to return
 | name's aunts
 |#
(defun aunts ( name )
	(let ((aunt (auntsUncles name)))
		(setf aunt (femalefilter aunt))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the niecesNephews and femalefilter functions to return
 | name's nieces
 |#
(defun nieces ( name )
	(let ((niece (niecesNephews name)))
		(setf niece (femalefilter niece))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the niecesNephews and malefilter functions to return
 | name's nephews
 |#
(defun nephews ( name )
	(let ((nephew (niecesNephews name)))
		(setf nephew (malefilter nephew))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the cousins and malefilter functions to return
 | name's male-cousins
 |# 
(defun male-cousins ( name )
	(let ((maleCousin (cousins name)))
		(setf maleCousin (malefilter maleCousin))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the cousins and femalefilter functions to return
 | name's female-cousins
 |#
(defun female-cousins ( name )
	(let ((femaleCousin (cousins name)))
		(setf femaleCousin (femalefilter femaleCousin))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the ancestors and malefilter functions to return
 | name's male-ancestors
 |#
(defun male-ancestors ( name )
	(let ((maleAncestor (ancestors name)))
		(setf maleAncestor (malefilter maleAncestor))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the ancestors and femalefilter functions to return
 | name's female-ancestors
 |#
(defun female-ancestors ( name )
	(let ((femaleAncestor (ancestors name)))
		(setf femaleAncestor (femalefilter femaleAncestor))
	)
)

#|
 | Author: Christian SIeh
 | This function uses the descendants and malefilter functions to return
 | name's male-descendants
 |#
(defun male-descendants ( name )
	(let ((maleDescendant (descendants name)))
		(setf maleDescendant (malefilter maleDescendant))
	)
)

#|
 | Author: Christian Sieh
 | This function uses the descendants and femalefilter functions to return
 | name's female-descendants
 |#
(defun female-descendants ( name )
	(let ((femaleDescendant (descendants name)))
		(setf femaleDescendant (femalefilter femaleDescendant))
	)
)
