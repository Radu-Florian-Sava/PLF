; problema 1

;Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea, al 4-lea,
;al 6-lea,....element.

; inser(a - atom, l -lista)
; a - atomul care va fi inserat din 2 in 2 pozitii
; l - lista in care va fi inserat

; inser(a,l1 ... ln) = { [ ln] , n < 2
;                      { [l1 l2 a] + inser(a, l3 ... ln), n >=2
;

(defun inser(a l)
 	(cond
  		(	(atom (cdr l)) 
			l
		)
  		( T 
			( append 
				(list (car l) (car (cdr l)) a)
				(inser a (cddr l))
			)
      		)
  	)
)  

;  Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
; care apar, pe orice nivel, dar in ordine inversa. De exemplu: (((A B) C)
; (D E)) --> (E D C B A)

; extract(l - lista)
; l - lista neliniara din care vom extrage atomii

; extract(l1 ... ln) = { (), n=0  
;                      { extract(l2...ln) + [l1], l1 este atom si n>0
;                      { extract( l1, l2...ln), l1 este lista si n>0 


; observatie: daca l1 este lista atunci il concatenam la restul listei 
; exemplu : ((1 5 7 (13)) 2 3 4) => (1 5 7 (13) 2 3 4)
; observatie : () este lista vida

(defun extract(l)
	(cond
		(	(atom l) 
			l
		)
		(	(atom (car l)) 
				( append
					(extract (cdr l))
					(list (car l))
				)
		)
		( T
			(extract 
				(append 
					(car l) 
					(cdr l)
				)
			)
		)
	)
) 

;Definiti o functie care intoarce cel mai mare divizor comun al numerelor
;dintr-o lista neliniara.

; cmmdc(c1 - number, c2 - number)
; c1 si c2 sunt numerele carora le aflam cel mai mare divizor comun

; cmmdc(c1, c2) = { c1+c2, daca c1*c2 este 0
;                 { cmmdc(c1-c2,c2), daca c1 > c2 si c1*c2 nu este 0
;                 { cmmdc(c1,c2-c1), daca c2 > c1 si c1*c2 nu este 0

(defun cmmdc(c1 c2)
	(cond
		(	(= (* c1 c2) 0)
				(+ c1 c2)
		)
		(	(> c1 c2)
				(cmmdc (- c1 c2) c2)
		)
		(	T
				(cmmdc c1 (- c2 c1))
		)
	)
)

;cmmdcl(l - lista)
;l - lista neliniara din care vrem sa aflam cel mai mare divizor comun al numerelor

;cmmdcl(l1...ln)={ 0, n=0
;                { cmmdc(cmmdcl(l2...ln),l1), daca n>0 si l1 este numar
;                { cmmdc(cmmdcl(l1),cmmdcl(l2 ... ln)), daca n>0 si l1 este lista
;                { cmmdcl(l2...ln), altfel (daca l1 nu este atom sau lista si n>0)

(defun cmmdcl(l)
	(cond
		(	(atom l) 
			0
		)
		(	(numberp (car l)) 
				( cmmdc
					(cmmdcl (cdr l))
					(car l)
				)
		)
		( 	(listp (car l))
				(cmmdc 
					(cmmdcl (car l)) 
					(cmmdcl (cdr l))

				)
		)
		( 	T
				(cmmdcl (cdr l))
		)
	)
)

;Sa se scrie o functie care determina numarul de aparitii ale unui atom dat
;intr-o lista neliniara.

; aparitii(l - lista, a -atom)
; l - lista neliniara din care vrem sa aflam aparitiile lui a
; a - atomul caruia dorim sa ii aflam aparitiile

; aparitii(l1 ... ln, a)= { 0, n=0
;                         { aparitii(l1,a) + aparitii(l2...ln,a), daca n diferit de 0 si l1 este lista
;                         { 1 + aparitii(l2...ln,a), daca n diferit de 0 si l1 este egal cu a
;                         { aparitii(l2...ln,a), altfel (daca n diferit de 0, l1 nu este lista si l1 diferit de a) 

(defun aparitii(l a)
	(cond
		(	(atom l) 
			0
		)
		(	(listp (car l))
				(+ 
					(aparitii (car l) a) 
					(aparitii (cdr l) a)
				)
		)
		(	(equal a (car l))
				(+ 	
					1 
					(aparitii (cdr l) a)
				)
		)
		(	T
				(aparitii (cdr l) a)
		)
	)
)


