;8. Sa se construiasca o functie care intoarce maximul atomilor numerici
;dintr-o lista, de la orice nivel.


(DEFUN MAXIM (L)
	( APPLY #'MAX 
	     (LABELS ((TEMP (S)
			( COND 
				((NUMBERP S) (LIST S))
				(T (MAPCAN #'TEMP S ))
			)
		     )
	     ) 
	     (TEMP L))
	)
	
)

; TEMP(l)= { (l), daca l este numar
;          { U TEMP(li) ,  daca l este lista l1 l2 ... ln , unde i ia valori de la 1 la n
;          { (), daca l este atom nenumeric  

;OBSERVATIE: TEMP transforma o lista neliniara in una liniara, inlocuind elementele lista cu elemenetele ei
;EXEMPLU: TEMP( (1 (2 3) 4 (5 6 (7) (8) ) ) ) = (1 2 3 4 5 6 7 8)

;MAXIM(l1 l2 ..ln) = MAX(TEMP(l1 l2 ... ln))
;functia MAX predefinita va extrage maximul din lista numerica liniara creeata de TEMP

;(MAXIM '(1 (2 3) 4 (5 6 (7) (8) ) ) ) = 8 
;(MAXIM '(1 (A) 2)) = 2