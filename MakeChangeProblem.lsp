(defun ismkChangeInputValid (n lst)
	(and (not (null lst)) (numberp n) (>= n 0) (ismkChangeListInputValid lst))
)

(defun ismkChangeListInputValid (lst)
	(or (null lst) (and (numberp (car lst)) (ismkChangeListInputValid (cdr lst))))
)

(defun find-best-change (n lst)
	(if (ismkChangeInputValid n lst)
		(makechange n lst)
		nil
	)
)

(defun makechange(n lst)
	(if (= 1111111111111 (Cp n lst))
		(makechange (1- n) lst)
		(compressResult (mkchange n lst nil) (reverse lst) nil)
	)
)

; (defun makechange(n lst)
	; (if (= 1111111111111 (Cpp n lst))
		; (makechange (1- n) lst)
		; (compressResult (mkchange n lst nil) (reverse lst) nil)
	; )
; )

(defun compressResult (res lst out)
	(if (null lst)
		out
		(compressResult res (cdr lst) (cons (list (countInList (car lst) res)(car lst)) out))
	)
)

(defun countInList (elem lst)
	(cond ((null lst) 0)
		((= elem (car lst)) (1+ (countInList elem (cdr lst))))
		(T (countInList elem (cdr lst)))
	)
)

(defun mkchange(p lst res)
  (if (<= p 0)
	res
	(let ((opti (Sp p lst)))
		(let ((constructor (cons opti res)))
		 (mkchange (- p opti) lst constructor)
		)
	)
  )
)

; (defun mkchange(p lst res)
  ; (if (<= p 0)
	; res
	; (let ((opti (Spp p lst)))
		; (let ((constructor (cons opti res)))
		 ; (mkchange (- p opti) lst constructor)
		; )
	; )
  ; )
; )

(defun Sp (p lst)
	(if (= 0 p)
		0
		(sforlooper p lst (Cp p lst))
	)
)

(defun sforlooper (p lst mini)
	(cond ((null lst) 0)
		  ((and (<= (car lst) p) (eql (1+ (Cp (- p (car lst)) lst)) mini))(car lst))
		  (T (sforlooper p (cdr lst) mini))
	)
)

(defun Cp (p lst)
	(if (= 0 p)
		0
		(forlooper p lst 1111111111111)
	)
)

(defun forlooper (p lst mini)
	(cond ((null lst) mini)
		  ((and (<= (car lst) p) (< (1+ (Cp (- p (car lst)) lst)) mini)) (forlooper p (cdr lst) (1+ (Cp (- p (car lst)) lst))))
		  (T (forlooper p (cdr lst) mini))
		  
	)
)

; (defun Spp (p lst)
	; (let ((mini (Cpp p lst))(valuee nil))
		; (dolist (element lst valuee)
			; (if (<= element p)
					; (if (= (1+ (Cpp (- p element) lst)) mini)
						; (setq valuee element)
					; )
			; )
			
		; )
	; )
; )

; (defun Cpp (p lst)
	; (if (= 0 p)
		; 0
		; (forlooperp p lst)
	; )
; )

; (defun forlooperp (p lst)
	; (let ((valuee 1111111111111))
		; (dolist (element lst valuee)
			; (if (<= element p)
					; (if (< (+ 1 (Cpp (- p element) lst)) valuee)
						; (setq valuee (+ 1 (Cpp (- p element) lst)))
					; )
			; )
			
		; )
	; )
; )
; Copied from class slides, following 3 functions
; (defun	compress	(x)	
	; (if	(consp	x)		
		; (compr	(car	x)	1	(cdr	x))	
		; x
	; )
; )	
	
; (defun	compr	(elt	n	lst)	
	; (if	(null	lst)	
		; (list	(n-elts	elt	n))	
		; (let	((next	(car	lst)))	
			; (if	(eql	next	elt)	
				; (compr	elt	(+	n	1)	(cdr	lst))	
				; (cons	(n-elts	elt	n) (compr	next	1	(cdr	lst)))
			; )
		; )
	; )
; )	
	
; (defun	n-elts	(elt	n)	
	; (if	(>=	n	1)	
		; (list	n	elt)	
		; elt
	; )
; )
