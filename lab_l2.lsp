;problema 1
;Se da un arbore de tipul (1). Sa se afiseze calea de la radacina pana la un nod x dat.

;apare(l1...ln,c) = { NIL, daca l e vida
;		    { T, daca l1 = c
;		    { apare(l3...ln), altfel 

;determina daca un element apare intr-un arbore reprezentat ca si tipul (1) 

(defun apare(l c)
	(cond
		( (null l) 
				NIL
		)
		( (equal c (car l)) 
				T
		)
		(T 
			(apare (cddr l) c )
		)
	)
)


;parcurg_st(l1...ln,nv,nm) = { [] , daca l e vida
;			     { [] , daca nv = 1 + nm (nv = numar varfuri ; nm = numar muchii)
;			     { [l1] + [l2] + parcurg_st(l3...ln, nv + 1, nm + l2) , altfel

;parcurge subarborele stang al unui arbore reprezentat ca si tipul (1)


(defun parcurg_st(l nv nm)
	(cond
		( (null l)
			 NIL
		)
		( (equal nv (+ 1 nm)) 
			NIL
		)
		( T 
			(append (list (car l)) 
				(list (cadr l)) 
				( parcurg_st (cddr l) 
					     (+ nv 1) 
				             (+ nm (cadr l)) 
				) 
			)
		)
	)
)

;parcurge subarborele drept al unui arbore reprezentat ca si tipul (1)

;parcurg_dr(l1...ln,nv,nm) = { [] , daca l e vida
;			     { l1...ln , daca nv = 1 + nm (nv = numar varfuri ; nm = numar muchii)
;			     { parcurg_dr(l3...ln, nv + 1, nm + l2) , altfel


(defun parcurg_dr(l nv nm)
	(cond
		( (null l)
		 		NIL
		)
		( (equal nv (+ 1 nm))
				 l 
		)
		( t 
			(append ( parcurg_dr (cddr l) (+ nv 1) (+ nm (cadr l)) )
			)
		)
	)
)

;extrage subarborele stang dintr-o lista de tipul (1) odata ce a fost extrasa radacina impreuna cu numarul sau de fii 

;stanga(l1...ln) = parcurg_st(l1...ln,0,0)

(defun stanga(l)
	(parcurg_st l 0 0)
)


;extrage subarborele drept dintr-o lista de tipul (1) odata ce a fost extrasa radacina impreuna cu numarul sau de fii 

;dreapta(l1...ln) = parcurg_dr(l1...ln,0,0)

(defun dreapta(l)
	(parcurg_dr l 0 0)
)

;stabileste drumul de la radacina pana la nodul cu indicele x

;drum(l1...ln,x) = { l1 , daca l1 = x
;			  { l1 + drum(stanga(l3...ln),x) , daca x apare in ramura stanga
;			  { l1 + drum(dreapta(l3...ln),x) , daca x apare in ramura dreapta
;			  { [] , daca x nu apare in arbore


(defun drum(l x)
	(cond
		( (equal x (car l))      
			         (list x)
		)
		( (apare (stanga (cddr l)) x)  
					( append (list (car l))
						 (drum (stanga (cddr l)) x)
					) 
		)
		( (apare (dreapta (cddr l)) x) 
					(append (list (car l)) 
						(drum (dreapta (cddr l)) x)
					) 
		)
		( T 
			(list NIL) 
		)
	)
)

;(A 2 B 0 C 2 D 0 E 0)
;  A
; / \
;B   C
;   / \
;  D   E


;(c 2 b 2 a 0 d 0 e 1 f 0) 
;    c
;   / \
;  b   e
; / \   \
;a   d   f


;(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0)
;         a
;       /   \
;      b     d
;     / \   / \
;    c  f   e  h
;   /  /
;  i  g

;(drum '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) 'g)
;(drum '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) 'd)
;(drum '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) 'h)
;(drum '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) 'i)

