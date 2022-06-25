%problema 1
% a) Definiti un predicat care determina suma a doua numere scrise in
% reprezentare de lista.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% cifre. Sa se calculeze suma tuturor numerelor reprezentate de subliste.
% De ex:
%[1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>
%[8, 2, 2].

invers(L, Rez) :- invers_aux([], L, Rez).

invers_aux(Col, [], Col).
invers_aux(Col, [H|T], Rez) :-
    invers_aux([H|Col], T, Rez),!.

suma(L1,L2,Rez):-
    invers(L1,L1X),invers(L2,L2X),suma_aux(L1X,L2X,X,0),invers(X,Rez).

suma_aux([],[],[1],1):-!.
suma_aux([],[],_,0):-!.
suma_aux([],T,T,0):-!.
suma_aux(T,[],T,0):-!.
suma_aux([],[H|T],[H1|T],1):-H<9,H1 is H +1,!.
suma_aux([H|T],[],[H1|T],1):-H<9,H1 is H+1,!.
suma_aux([],[H|T1],[0|T2],1):-H is 9,suma_aux([],T1,T2,1),!.
suma_aux([H|T1],[],[0|T2],1):-H is 9,suma_aux(T1,[],T2,1),!.


suma_aux([H1|T1],[H2|T2],[H3|Rez],C):-
    Carry is (H1 + H2 + C) div 10,
    suma_aux(T1,T2,Rez,Carry),
    H3 is (H1+ H2 + C) mod 10.

sublistaSuma([],[0]).
sublistaSuma([Cap|Coada],Rez):-is_list(Cap),!,
    sublistaSuma(Coada,Rez1),
    suma(Cap,Rez1,Rez),!.
sublistaSuma([_|Coada],Rez):-sublistaSuma(Coada,Rez).

%problema 2
% a) Sa se sorteze o lista cu pastrarea dublurilor. De ex: [4 2 6 2 3 4] => [2 2 3 4 4 6]
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere. Sa se sorteze fiecare sublista cu pastrarea dublurilor. De ex:
% [1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, 2, [1, 4, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1, 1, 1], 7].

combinaListe([],Rez,Rez).
combinaListe([H1|L1],L2,[H1|Rez]):-combinaListe(L1,L2,Rez).

sortare([],[]).
sortare([H|T],Rez):-sortare_aux(H,T,[],[],Rez).

sortare_aux(X,[H|T],L1,L2,Rez):-H < X,!,sortare_aux(X,T,[H|L1],L2,Rez).
sortare_aux(X,[H|T],L1,L2,Rez):-H >= X,!,sortare_aux(X,T,L1,[H|L2],Rez).
sortare_aux(X,[],L1,L2,Rez):-sortare(L1,L1X),sortare(L2,L2X),combinaListe(L1X,[X|L2X],Rez).

sublistaSort([],[]).
sublistaSort([Cap|Coada],[Sortat|Rez]):-is_list(Cap),!,
    sortare(Cap,Sortat),
    sublistaSort(Coada,Rez).
sublistaSort([Cap|Coada],[Cap|Rez]):-sublistaSort(Coada,Rez).

%problema 3
% a) Sa se sorteze o lista cu eliminarea dublurilor. De ex: [4 2 6 2 3 4] => [2 3 4 6]
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere. Sa se sorteze fiecare sublista fara pastrarea dublurilor. De
% ex:
% [1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, 2, [1, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1], 7].

sortareElim([],[]).
sortareElim([H|T],Rez):-sortareElim_aux(H,T,[],[],Rez).

sortareElim_aux(X,[H|T],L1,L2,Rez):-H < X,!,sortareElim_aux(X,T,[H|L1],L2,Rez).
sortareElim_aux(X,[H|T],L1,L2,Rez):-H > X,!,sortareElim_aux(X,T,L1,[H|L2],Rez).
sortareElim_aux(X,[H|T],L1,L2,Rez):-H = X,!,sortareElim_aux(X,T,L1,L2,Rez).
sortareElim_aux(X,[],L1,L2,Rez):-sortareElim(L1,L1X),sortareElim(L2,L2X),combinaListe(L1X,[X|L2X],Rez).

sublistaSortElim([],[]).
sublistaSortElim([Cap|Coada],[Sortat|Rez]):-is_list(Cap),!,
    sortareElim(Cap,Sortat),
    sublistaSortElim(Coada,Rez).
sublistaSortElim([Cap|Coada],[Cap|Rez]):-sublistaSortElim(Coada,Rez).

%problema 4
% a) Sa se interclaseze fara pastrarea dublurilor doua liste sortate.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere sortate. Sa se interclaseze fara pastrarea dublurilor toate
% sublistele. De ex :
% [1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5, [1, 1, 11], 8] =>
% [1, 2, 3, 4, 6, 7, 9, 10, 11].

unicate([],[]):-!.
unicate([E],[E]):-!.
unicate([H|T],Rez):-unicate_aux([H|T],Rez,H).

unicate_aux([],[E],E).
unicate_aux([H|T],Rez,H):-unicate_aux(T,Rez,H),!.
unicate_aux([R|T],[H|Rez],H):-unicate_aux(T,Rez,R).

interclasare(L1,L2,Rez):-unicate(L1,L1X),unicate(L2,L2X),interclasare_aux(L1X,L2X,Rez).

interclasare_aux([],L,L):-!.
interclasare_aux(L,[],L):-!.
interclasare_aux([H1|L1],[H2|L2],[H1|Rez]):- H1<H2,!,interclasare_aux(L1,[H2|L2],Rez).
interclasare_aux([H1|L1],[H2|L2],[H2|Rez]):- H1>H2,!,interclasare_aux([H1|L1],L2,Rez).
interclasare_aux([H1|L1],[H2|L2],[H1|Rez]):- H1=H2,interclasare_aux(L1,L2,Rez).

sublistaInter([],[]).
sublistaInter([Cap|Coada],Rez):-is_list(Cap),!,
    sublistaInter(Coada,Rez1),
    interclasare(Cap,Rez1,Rez).
sublistaInter([_|Coada],Rez):-sublistaInter(Coada,Rez).

%problema 5
% a) Sa se determine pozitiile elementului maxim dintr-o lista liniara. De ex:
% poz([10,14,12,13,14], L) va produce L = [2,5].
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Sa se inlocuiasca fiecare sublista cu pozitiile
% elementului maxim din sublista respectiva. De ex:
% [1, [2, 3], [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, [2], [1, 3], 3, 6, [2], 5, [1, 2, 3], 7]

maxim([E],E).
maxim([H|T],Rez):-maxim(T,Rez), Rez > H,!.
maxim([H|T],H):-maxim(T,Rez),Rez =<H,!.


pozitii(L,Rez):-maxim(L,M),pozitii_aux(L,Rez,1,M).

pozitii_aux([],[],_,_).
pozitii_aux([H|T],[C|Rez],C,H):- C1 is C+1,pozitii_aux(T,Rez,C1,H),!.
pozitii_aux([_|T],Rez,C,H):- C1 is C+1,pozitii_aux(T,Rez,C1,H).

sublistaPoz([],[]).
sublistaPoz([Cap|Coada],[Pozitii|Rez]):-is_list(Cap),!,
    pozitii(Cap,Pozitii),
    sublistaPoz(Coada,Rez).
sublistaPoz([Cap|Coada],[Cap|Rez]):-sublistaPoz(Coada,Rez).

%problema 6
% a) Intr-o lista L sa se inlocuiasca toate aparitiile unui element E cu
% elementele unei alte liste, L1. De ex:
% inloc([1,2,1,3,1,4],1,[10,11],X)
% va produce X=[10,11,2,10,11,3,10,11,4].
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. In fiecare sublista sa se inlocuiasca toate aparitiile
% primului element din sublista cu o lista data. De ex:
%[1, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] si [11, 11] =>
% [1, [11, 11, 1, 11, 11], 3, 6, [11, 11, 10, 1, 3, 9], 5, [11 11 11 11
% 11 11], 7]

inloc([],_,_,[]).
inloc([H|T],E,L,Rez):-E is H,!,inloc(T,H,L,Rez1),combinaListe(L,Rez1,Rez).
inloc([H|T],E,L,[H|Rez]):-inloc(T,E,L,Rez).

sublistaInloc([],_,[]).
sublistaInloc([[H|T]|Coada],L,[Rez1|Rez]):-!,sublistaInloc(Coada,L,Rez),inloc([H|T],H,L,Rez1).
sublistaInloc([Cap|Coada],L,[Cap|Rez]):-sublistaInloc(Coada,L,Rez).

%problema 7
% a) Definiti un predicat care determina produsul unui numar reprezentat
% cifra cu cifra intr-o lista cu o anumita cifra. De ex: [1 9 3 5 9 9] *
% 2
% => [3 8 7 1 9 8]
% b) Se da o lista eterogena, formata din numere intregi si maximum 9
% liste de numere intregi. Sa se inlocuiasca fiecare sublista cu
% rezultatul inmultirii sublistei cu numarul de ordine al sublistei
% (prima sublista cu 1, a 2-a cu 2, etc.). De ex:
% [1, [2, 3], [4, 1, 4], 3, 6, [7, 5, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, [2, 3], [8, 2, 8], 3, 6, [2, 2, 5, 4, 1, 7], 5, [4, 4, 4], 7]

inmultire(L,C,Rez):-invers(L,LX),inmultire_aux(LX,C,0,RezX),invers(RezX,Rez).

inmultire_aux([],_,0,[]):-!.
inmultire_aux([],_,Cat,[Cat]):-! .
inmultire_aux([H|T],C,Cat,[H1|T1]):-H1 is (H*C+Cat) mod 10,
    Cat1 is (H*C+Cat) div 10,inmultire_aux(T,C,Cat1,T1).

sublistaInm(L,Rez):-sublistaInm_aux(L,Rez,1).

sublistaInm_aux([],[],_).
sublistaInm_aux([H|T],[Rez|RezT],C):-is_list(H),!,
    inmultire(H,C,Rez),C1 is C +1,sublistaInm_aux(T,RezT,C1).
sublistaInm_aux([H|T],[H|RezT],C):-sublistaInm_aux(T,RezT,C).

%problema 8
% a) Definiti un predicat care determina succesorul unui numar
% reprezentat
% cifra cu cifra intr-o lista. De ex: [1 9 3 5 9 9] => [1 9 3 6 0 0]
% b) Se da o lista eterogena, formata din numere intregi si liste de
% cifre.
% Pentru fiecare sublista sa se determine succesorul numarului
% reprezentat
% cifra cu cifra de lista respectiva. De ex:
% [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>
% [1, [2, 4], 4, 5, [6, 8, 0], 10, 11, [1, 2, 1], 6]

succesor(L1,[1|Rez]):-
    succesor(L1,Rez,Carry),
    Carry is 1,!.
succesor(L1,Rez):-
    succesor(L1,Rez,Carry),
    Carry is 0.

succesor([N],[R],Carry):-N <9,!,
    R is N+1,
    Carry is 0.
succesor([9],[R],Carry):-R is 0,
    Carry is 1,!.
succesor([Cap|Coada],[Cap1|Coada1],0):-succesor(Coada,Coada1,Carry1),
    Cap1 is Cap+Carry1,
    Cap1 < 10,!.
succesor([Cap|Coada],[Cap1|Coada1],1):-succesor(Coada,Coada1,Carry1),
    Cap1 is (Cap+Carry1) mod 10.

sublistaSuccesor([],[]).
sublistaSuccesor([Cap|Coada],[X|Rez]):-is_list(Cap),!,
    succesor(Cap,X),
    sublistaSuccesor(Coada,Rez),!.
sublistaSuccesor([Cap|Coada],[Cap|Rez]):-sublistaSuccesor(Coada,Rez).


%problema 9
% a) Dandu-se o lista liniara numerica, sa se stearga toate secventele
% de valori consecutive. Ex: sterg([1, 2, 4, 6, 7, 8, 10], L) va produce
% L=[4, 10].
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Din fiecare sublista sa se stearga toate secventele de
% valori consecutive. De ex:
% [1, [2, 3, 5], 9, [1, 2, 4, 3, 4, 5, 7, 9], 11, [5, 8, 2], 7] =>
% [1, [5], 9, [4, 7, 9], 11, [5, 8, 2], 7]

sterg([],[]):-!.
sterg([E],[E]):-!.
sterg([H|T],Rez):-sterg_aux(T,H,RezX),sterg_f([H|RezX],Rez).

sterg_f([],[]):-!.
sterg_f([e],[]):-!.
sterg_f([e|T],Rez):-sterg_f(T,Rez),!.
sterg_f([_|[e|T]],Rez):-sterg_f(T,Rez),!.
sterg_f([H|T],[H|Rez]):-sterg_f(T,Rez).

sterg_aux([H],P,[]):-H is P+1,!.
sterg_aux([H],_,[H]):-!.
sterg_aux([H|T],P,[e|Rez]):-H is P+1,!,
    sterg_aux(T,H,Rez).
sterg_aux([H|T],_,[H|Rez]):-sterg_aux(T,H,Rez).

sublistaSterg([],[]).
sublistaSterg([H|T],[S|Rez]):-is_list(H),!,sterg(H,S),sublistaSterg(T,Rez).
sublistaSterg([H|T],[H|Rez]):-sublistaSterg(T,Rez).

%problema 10
% a) Se da o lista de numere intregi. Se cere sa se adauge in lista dupa
% 1-ul element, al 3-lea element, al 7-lea elemen, al 15-lea element … o
% valoare data e.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Lista incepe cu un numar si nu sunt 2 elemente
% consecutive care sunt liste. Se cere ca in fiecare sublista sa se
% adauge dupa 1-ul, al 3-lea, al 7-lea… element valoarea care se gaseste
% inainte de sublista in lista eterogena. De ex:
% [1, [2, 3], 7, [4, 1, 4], 3, 6, [7, 5, 1, 3, 9, 8, 2, 7], 5] =>
% [1, [2, 1, 3], 7, [4, 7, 1, 4, 7], 3, 6, [7, 6, 5, 1, 6, 3, 9, 8, 2, 6,
% 7], 5].

inser(L,Rez,V):-inser_aux(L,Rez,V,1,1).

inser_aux([],[E],E,Ct,Exp):-Ct is 2*Exp+1,!.
inser_aux([],[],_,_,_):-!.
inser_aux([H|T],[H|[V|Rez]],V,Ct,Exp):-Ct is Exp ,!,Ct1 is Ct+1, Exp1 is 2*Exp+1,inser_aux(T,Rez,V,Ct1,Exp1).
inser_aux([H|T],[H|Rez],V,Ct,Exp):-Ct1 is Ct+1,inser_aux(T,Rez,V,Ct1,Exp).

sublistaInser([H|T],[H|Rez]):-sublistaInser_aux(T,H,Rez).

sublistaInser_aux([],_,[]).
sublistaInser_aux([H|T],E,[Rez|RezT]):-is_list(H),!,inser(H,Rez,E),sublistaInser_aux(T,E,RezT).
sublistaInser_aux([H|T],_,[H|RezT]):-sublistaInser_aux(T,H,RezT).

%problema 11
% a) Se da o lista de numere intregi. Se cere sa se scrie de 2 ori in
% lista fiecare numar prim.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Se cere ca in fiecare sublista sa se scrie de 2 ori
% fiecare numar prim. De ex:
% [1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5] =>
% [1, [2, 2, 3, 3], 4, 5, [1, 4, 6], 3, [1, 3, 3, 7, 7, 9, 10], 5]

prim(X):-X > 1,!,prim_aux(X,2).
prim_aux(X,I):- I > X div 2,!.
prim_aux(X,I):-Rest is (X mod I),Rest \== 0 ,I1 is I +1, prim_aux(X,I1).

substPrim([],[]).
substPrim([H|T],[H,H|Rez]):-prim(H),!,substPrim(T,Rez).
substPrim([H|T],[H|Rez]):-substPrim(T,Rez).

sublistaPrim([],[]).
sublistaPrim([H|T],[Rez|RezT]):-is_list(H),!,substPrim(H,Rez),sublistaPrim(T,RezT).
sublistaPrim([H|T],[H|RezT]):-sublistaPrim(T,RezT).

%problema 12
% a) Sa se inlocuiasca toate aparitiile unui element dintr-o lista cu un
% alt element.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Se cere ca toate aparitiile elementului maxim (dintre
% valorile intregi ale listei) sa fie inlocuite in fiecare sublista cu
% maximul sublistei respective. De ex:
% [1, [2, 5, 7], 4, 5, [1, 4], 3, [1, 3, 5, 8, 5, 4], 5, [5, 9, 1], 2]
% =>
% [1, [2, 7, 7], 4, 5, [1, 4], 3, [1, 3, 8, 8, 8, 4], 5, [9, 9, 1], 2]

subst([],_,_,[]).
subst([V|T],V,N,[N|Rez]):- subst(T,V,N,Rez),!.
subst([H|T],V,N,[H|Rez]):- subst(T,V,N,Rez).

maxim_aux([E],E):-!.
maxim_aux([H|T],H):-maxim_aux(T,E),is_list(E),!.
maxim_aux([H|T],H):-maxim_aux(T,E),number(H),H>E,!.
maxim_aux([_|T],E):-maxim_aux(T,E).

sublistaMax(L,Rez):-maxim_aux(L,M),sublistaMax_aux(L,Rez,M).

sublistaMax_aux([],[],_).
sublistaMax_aux([H|T],[Rez|RezT],Max):-is_list(H),!,maxim_aux(H,M),subst(H,Max,M,Rez),sublistaMax_aux(T,RezT,Max).
sublistaMax_aux([H|T],[H|RezT],Max):-sublistaMax_aux(T,RezT,Max).

%problema 13
% a) Sa se adauge dupa fiecare element dintr-o lista divizorii
% elementului.
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Se cere ca in fiecare sublista sa se adauge dupa
% fiecare element divizorii elementului. De ex:
% [1, [2, 5, 7], 4, 5, [1, 4], 3, 2, [6, 2, 1], 4, [7, 2, 8, 1], 2] =>
% [1, [2, 5, 7], 4, 5, [1, 4, 2], 3, 2, [6, 2, 3, 2, 1], 4, [7, 2, 8,
%  2, 4, 1], 2]

divizori(X,L):-divizori_aux(X,L,2).

divizori_aux(X,[],C):- C > X div 2,!.
divizori_aux(X,[C|Rez],C):- D is X mod C,D is 0,!,C1 is C+1,divizori_aux(X,Rez,C1).
divizori_aux(X,Rez,C):- C1 is C+1, divizori_aux(X,Rez,C1).

combina_liste([],L2,L2).
combina_liste([H1|T1],L2,[H1|Rez]):-combina_liste(T1,L2,Rez).

adauga_div([],[]).
adauga_div([H|T],[H|Rez]):-divizori(H,Div),adauga_div(T,RezX),combinaListe(Div,RezX,Rez).

sublistaDiv([],[]).
sublistaDiv([H|T],[Rez|RezT]):-is_list(H),!,adauga_div(H,Rez),sublistaDiv(T,RezT).
sublistaDiv([H|T],[H|RezT]):-sublistaDiv(T,RezT).

%problema 14
% a) Definiti un predicat care determina predecesorul unui numar
% reprezentat
% cifra cu cifra intr-o lista. De ex: [1 9 3 6 0 0] => [1 9 3 5 9 9]
% b) Se da o lista eterogena, formata din numere intregi si liste de
% cifre. Pentru fiecare sublista sa se determine predecesorul numarului
% reprezentat cifra cu cifra de lista respectiva. De ex:
% [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>
% [1, [2, 2], 4, 5, [6, 7, 8], 10, 11, [1, 1, 9] 6]

predecesor(X,Rez):-invers(X,Inv),predecesor_aux(Inv,RezX),invers(RezX,Rez).

identic(X,X).

predecesor_aux([H|T],[9|Rez]):- H is 0, predecesor_aux(T,Rez),!.
predecesor_aux([H|T],[H1|Rez]):- H1 is H -1,H1 \== 0,identic(T,Rez).
predecesor_aux([_|T],Rez):-identic(T,Rez).

sublistaPredecesor([],[]).
sublistaPredecesor([Cap|Coada],[X|Rez]):-is_list(Cap),!,
    predecesor(Cap,X),
    sublistaPredecesor(Coada,Rez),!.
sublistaPredecesor([Cap|Coada],[Cap|Rez]):-sublistaPredecesor(Coada,Rez).

%problema 15
% a) Sa se determine cea mai lunga secventa de numere pare consecutive
% dintr-o lista (daca sunt mai multe secvente de lungime maxima, una
% dintre ele).
% b) Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Sa se inlocuiasca fiecare sublista cu cea mai lunga
% secventa de numere pare consecutive din sublista respectiva. De ex:
% [1, [2, 1, 4, 6, 7], 5, [1, 2, 3, 4], 2, [1, 4, 6, 8, 3], 2, [1, 5], 3]
% =>
% [1, [4, 6], 5, [2], 2, [4, 6, 8], 2, [], 3]

mai_mare([],[]):-!.
mai_mare([_|_],[]):-!.
mai_mare([_|T1],[_|T2]):-mai_mare(T1,T2).


secv_pare([H|T],[H|Rez]):-D is H mod 2,D is 0,!,secv_pare(T,Rez).
secv_pare(_,[]).

secv_compar([],[]).
secv_compar([H|T],Pare):-secv_compar(T,Rez),secv_pare([H|T],Pare),mai_mare(Pare,Rez),!.
secv_compar([_|T],Rez):-secv_compar(T,Rez).

sublistaPare([],[]).
sublistaPare([H|T],[Rez|RezT]):-is_list(H),!,secv_compar(H,Rez),sublistaPare(T,RezT).
sublistaPare([H|T],[H|Rez]):-sublistaPare(T,Rez).







