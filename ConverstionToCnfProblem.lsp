(defun convert-to-CNF (l)
	(remove-duplicates (convertCNFrecursive l) :test #'equal)
)

(defun removedups(lst)
  (remove-duplicates lst :test #'equal)
 )

(defun convertCNFrecursive (sen)
  (let ((sen1 (eliminateIFAndIFF  sen)))
    (cond 
		((eql 'AND (connective sen1)) (makeAND (mappends (arguments sen1))))
		((eql 'OR (connective sen1)) (mergeAll (mapcars (arguments sen1))))
		((eql 'NOT (connective sen1)) (let ((sen2 (removeNOT (firstarg sen1))))
	    (if (not(isLiteral sen2)) (convertCNFrecursive sen2) sen2)))		 
		 (t sen1)
	)
  )
)

(defun mapcars (q)
	(let ((result nil))
		(reverse (dolistimcars q result))
	)
)

(defun dolistimcars (lst res)
	(cond ((null lst) res)
		(t (dolistimcars (cdr lst)(cons (convertCNFrecursive (car lst)) res)))
	)	
)

(defun mappends (q)
	(let ((result nil))
		(dolistimends q result)
	)
)
  
(defun dolistimends (lst res)
	(cond ((null lst) res)
		(t (dolistimends (cdr lst)(append (conjuncts (convertCNFrecursive (car lst))) res)))
	)	
)
	  
(defun mergeAll (disjuncts)
  (cond 
		((= 1 (listlength disjuncts)) (first disjuncts))
		(t (makeAND
		(let ((result nil))
		  (doubleloopouter (conjuncts (mergeAll (rest disjuncts))) result (conjuncts (first disjuncts)))
		)
	))
   )
)

(defun doubleloopouter (lst res in)
	(cond ((null lst) res)
		(t (doubleloopouter (cdr lst)(doubleloopinner in (car lst) res) in))
	)
)

(defun doubleloopinner (lst outerelem res)
	(cond ((null lst) res)
		(t (doubleloopinner (cdr lst) outerelem (push (makeOR (append (disjuncts (car lst))(disjuncts outerelem))) res)))
	)
)

(defun removeNOT (sen)
   (cond 
	 ((eql 'AND (connective sen)) (makeOR (mapcar #'removeNOT (arguments sen)))) 
	 ((eql 'OR (connective sen)) (makeAND (mapcar #'removeNOT (arguments sen))))
	 ((eql 'NOT (connective sen)) (firstarg sen)) 	 
	 (t (list 'NOT sen))
   )
)
	
(defun eliminateIFAndIFF (sen)
  (cond ((isLiteral sen) sen)
		((eql 'IFF (connective sen)) (eliminateIFF sen))
		((eql 'IF (connective sen)) (eliminateIF sen))
		(t (cons (connective sen) (mapcar #'eliminateIFAndIFF (arguments sen))))
   )
)

(defun eliminateIFF (sen)
	`(and (or (not ,(firstarg sen)) ,(restarg sen))(or (not ,(restarg sen)) ,(firstarg sen)))
)

(defun eliminateIF (sen)
	`(or (not ,(firstarg sen)) ,(restarg sen))
)

(defun isAtom (sentence)
  (not (member (connective sentence) '(AND OR NOT IF IFF))
   ))

(defun isLiteral (sentence)
  (or (isAtom sentence)
      (and (isNOT sentence) (isAtom (firstarg sentence)))))

(defun isNOT (sentence)
  (eql (connective sentence) 'NOT))

(defun conjuncts (sentence)
  (cond ((eql (connective sentence) 'AND) (arguments sentence))
	(t (list sentence))))

(defun disjuncts (sentence)
  (cond ((eql (connective sentence) 'OR) (arguments sentence))
	(t (list sentence))))

(defun makeAND (arguments)
  (cond 
		((= 1 (listlength arguments)) (first arguments))
		(t (cons 'AND arguments))
   )
)

(defun makeOR (arguments)
  (cond 
	((= 1 (listlength arguments)) (first arguments))
	(t (cons 'OR arguments))
   )
)

(defun listlength (lst)
	(length lst)
)
	
(defun connective (sentence)
	(cond ((and (not (null sentence)) (listp sentence)) (first sentence))
		(t nil)
	)
)

(defun arguments (sentence)
	(cond ((and (not (null sentence))(>= (listlength sentence) 2) (listp  sentence)) (rest sentence))
		(t nil)
	)
)

(defun firstarg (sentence)
	(cond ((and (listp sentence) (listp (rest sentence))(not (null (first (rest sentence))))) (first (rest sentence)))
		(t nil)
	)
)

(defun restarg (sentence)
	(cond ((and (listp sentence) (listp (rest sentence)) (listp (rest (rest sentence))) (not (null (rest (rest sentence))))) (first (rest (rest sentence))))
		(t nil)
	)
)
