%problema 1
%Sa se scrie un predicat care intoarce diferenta a doua multimi.

%contine(L: lista, X: simbol)
%L - lista la care verificam apartenenta lui X
%X - simbolul a carui apartenenta o verificam
%model de flux (i,i) sau (i,o)

contine([Cap|_],X):-X is Cap,!.
contine([_|Coada],X):-contine(Coada,X).

%diferenta(L1: lista, L2: lista, L3: lista)
%L1 - mutlimea careia ii facem diferenta
%L2 - multimea care face diferenta
%L3 - multimea rezultata din L2\L3
%model de flux (i,i,i) sau (i,i,o)

diferenta(L,L,[]):-!.
diferenta([Cap|Coada],L,X):-contine(L,Cap),diferenta(Coada,L,X),!.
diferenta(X,_,X).

%Sa se scrie un predicat care adauga intr-o lista dupa fiecare element
%par valoarea 1.

%unuDupaPar(L: lista,Rez: lista)
%L - lista initiala
%Rez - lista care contine elementele listei initiale urmate de cate un 1
%model de flux (i,i) sau (i,o)

unuDupaPar([Cap|Coada],[Cap|[1|X]]):-Cap mod 2 =:= 0,
    unuDupaPar(Coada,X),!.
unuDupaPar([Cap|Coada],[Cap|X]):-unuDupaPar(Coada,X),!.
unuDupaPar([],[]).

%problema 2
%Sa se scrie un predicat care determina cel mai mic multiplu comun al
%elementelor unei liste formate din numere intregi.

%cmmdc(X: integer, Y:integer, Z: integer)
%X,Y - intregi pozitivi caroroa vrem sa le gasim cel mai mare divizor
%comun
%Z - cel mai mare divizor comun al lui X si Y
%model de flux (i,i,i) sau (i,i,o)
%formula recursiva foloseste algoritmul lui Euclid

cmmdc(X,0,X):-!.
cmmdc(0,X,X):-!.
cmmdc(X,Y,Z):- X \= 0,Y \= 0,
    X > Y, X1 is X-Y,
    cmmdc(X1,Y,Z),!.
cmmdc(X,Y,Z):- X \= 0,Y \= 0,
    X1 is Y-X,
    cmmdc(X,X1,Z),!.

%cmmmc(X: integer, Y:integer, Z: integer)
%X,Y - intregi pozitivi caroroa vrem sa le gasim cel mai mic multiplu
%comun
%Z - cel mai mic multiplu comun comun al lui X si Y
%model de flux(i,i,i) sau (i,i,o)
%rezultatul se bazeaza pe faptul ca cmmdc(x,y)*cmmmc(x,y)=x*y,
%unde x, y numere naturale

cmmmc(X,Y,Z):-cmmdc(X,Y,X1),
    Z is X*Y/X1.

%Sa se scrie un predicat care adauga dupa 1-ul, al 2-lea, al 4-lea, al
%8-lea ...element al unei liste o valoare v data

%putere2(X:integer)
%X - numar natural
%model de flux (i)
%rezultatul este true daca X este o putere a lui 2 sau false in caz
%contrar

putere2(1).
putere2(X):-X mod 2 =:= 0,
    X1 is X/2,
    putere2(X1).

%adaugaV(L1: lista,L2: lista, V: integer)
%L1 - lista initiala
%L2 - lista care are valoarea V inserata dupa fiecare element
%al carui indice este putere a lui 2
%V - elementul care va fi inserat
%model de flux (i,o,i) sau (i,i,i)
%predicatul este un wrapper pentru cel cu 4 parametri, ultimul paremetru
%fiind de fapt indicele sirului pe care vrem sa il verificam ca fiind
%putere a lui 2

adaugaV(L1,L2,V):-adaugaV(L1,L2,V,1).
adaugaV([],[],_,_).
adaugaV([Cap|Coada],[Cap|[V|X]],V,Elem):- putere2(Elem),
    Elem1 is Elem + 1,
    adaugaV(Coada,X,V,Elem1),!.
adaugaV([Cap|Coada],[Cap|X],V,Elem):- Elem1 is Elem+1,
    adaugaV(Coada,X,V,Elem1).

%problema 3
%Sa se scrie un predicat care transforma o lista intr-o multime, in
% ordinea primei aparitii. Exemplu: [1,2,3,1,2] e transformat in [1,2,3].

%scoateDubluri(L1: lista, L2: lista, X: integer)
%L1 - lista din care eliminam elementul X
%L2 - lista rezultat care nu contine X
%X - intreg a carui aparitii le eliminam din L1
%model de flux (i,i,i) sau (i,i,o)

scoateDubluri([],[],_):-!.
scoateDubluri([Cap|Coada],X,Dublura):-Cap is Dublura,
    scoateDubluri(Coada,X,Dublura),!.
scoateDubluri([Cap|Coada],[Cap|X],Dublura):-scoateDubluri(Coada,X,Dublura).

%listaMultime(L1:lista, L2:lista)
%L1 - lista pe care o transformam in multime
%L2 - lista rezultat(multime)
%model de flux (i,i) sau (i,o)
%la fiecare pas eliminam elementul gasit din restul listei, element pe
%care il adaugam la lista pe care o cream

listaMultime([],[]).
listaMultime([Cap|Coada],[Cap|X]):- scoateDubluri(Coada,CoadaNoua,Cap),
    listaMultime(CoadaNoua,X),!.

%Sa se scrie o functie care descompune o lista de numere intr-o lista de
% forma [ lista-de-numere-pare lista-de-numere-impare] (deci lista cu
% doua
%elemente care sunt liste de intregi), si va intoarce si numarul
%elementelor pare si impare.

%identitate(X: element, Y: element)
%verifica daca X si Y sunt identice
%model de flux (i,i), (i,o) sau (o,i)

identitate(X,X).

%pareImpare(L: lista, Rez: lista, Pare: integer, Impare: integer)
%L - lista pe care o partitionam
%Rez - lista rezultat care contine 2 subliste
%Pare - numarul de numere pare din L
%Impare - numarul de elemente impare din L
%model de flux(i,o,o,o)
%reprezinta de fapt un wrapper pentru predicatul cu 5 variabile
%care formeaza 2 liste, una cu elemente pare iar alta cu elemente impare
%predicatul wrapper formeaza o lista care contine cele 2 liste
%nou-formate

pareImpare(L1,X,Pare,Impare):-pareImpare(L1,L21,L22,Pare,Impare),
    identitate(X1,[L22|[]]),
    identitate(X,[L21|X1]).
pareImpare([],[],[],0,0).
pareImpare([Cap|Coada],[Cap|L1],L2,Pare,Impare):-Cap mod 2 =:= 0,
    pareImpare(Coada,L1,L2,Pare1,Impare),
    Pare is Pare1 +1,!.
pareImpare([Cap|Coada],L1,[Cap|L2],Pare,Impare):-Cap mod 2 =:= 1,
    pareImpare(Coada,L1,L2,Pare,Impare1),
    Impare is Impare1 +1.

%problema 4
%Sa se scrie un predicat care substituie intr-o lista un element printr-o
%alta lista.

%substituieLista(L1: lista, L2: lista, V: lista, L: integer)
%L1 - lista in care va fi substitui L cu V
%L2 - lista rezultat
%V - lista care va inlocui aparitiile lui L
%L - element ale carui aparitii vor fi inlocuite
%model de flux (i,i,i,i) sau (i,o,i,i)

substitutieLista([],[],_,_).
substitutieLista([Cap|Coada],[L|X],V,L):- Cap is V,
    substitutieLista(Coada,X,V,L),!.
substitutieLista([Cap|Coada],[Cap|X],V,L):-substitutieLista(Coada,X,V,L),!.

%Sa se elimine elementul de pe pozitia a n-a a unei liste liniare.
%identitate2(X: element, Y: element)
%verifica daca X si Y sunt identice
%model de flux (i,i), (i,o) sau (o,i)

identitate2(X,X).

%eliminaLista(L1: lista,L2: lista, N: integer)
%L1 - lista din care eliminam elementul de pe pozitia N
%L2 - lista rezultat
%N - pozitia de pe care va fi eliminat un element
%model de flux (i,i,i) sau (i,o,i)
%daca nu exista nici un element pe pozitia N rezultatul
%va fi false

eliminaLista([Cap|Coada],[Cap|X],N):- N1 is N-1,
    eliminaLista(Coada,X,N1),!.
eliminaLista([_|Coada],X,1):-identitate2(Coada,X),!.

%problema 5
%Sa se scrie un predicat care sterge toate aparitiile unui anumit atom
%dintr-o lista.

%identitate3(X: element, Y: element)
%verifica daca X si Y sunt identice
%model de flux (i,i), (i,o) sau (o,i)

identitate3(X,X).

%stergeAparitii(L: lista, Rez: lista, X: integer)
%L - lista din care stergem aparitiile lui X
%Rez - lista rezultat
%X - atom care va fi eliminat
%model de flux (i,i,i) sau (i,o,i)

stergeAparitii([],X,_):-identitate3(X,[]).
stergeAparitii([Cap|Coada],X,Eliminat):-Cap = Eliminat,
    stergeAparitii(Coada,X,Eliminat),!.
stergeAparitii([Cap|Coada],[Cap|X],Eliminat):-stergeAparitii(Coada,X,Eliminat).

%Definiti un predicat care, dintr-o lista de atomi, produce o lista de
%perechi (atom n), unde atom apare in lista initiala de n ori. De ex:
% numar([1, 2, 1, 2, 1, 3, 1], X) va produce X = [[1, 4], [2, 2], [3,1]].

%numarAparitii( L: lista, X: integer, N: integer)
%L - lista in care numaram aparitiile lui X
%X - numar intreg a carui aparitii le contorizam
%N - numarul de aparitii a lui X in L
%model de flux (i,i,i) sau (i,i,o)

numarAparitii([],_,0).
numarAparitii([Cap|Coada],X,N):-Cap =:= X,
    numarAparitii(Coada,X,N1),
    N is N1+1,!.
numarAparitii([_|Coada],X,N):-numarAparitii(Coada,X,N).

%contorLista(L: lista, X: lista)
%L - lista intiala
%X - lista care contine perechi de tipul [element, numar de aparitii]
%model de flux (i,i) sau (i,o)

contorLista([],X):-identitate3(X,[]).
contorLista([Cap|Coada],[[Cap,N1]|X]):-numarAparitii(Coada,Cap,N),
    N1 is N+1,
    stergeAparitii(Coada,CoadaNoua,Cap),
    contorLista(CoadaNoua,X).

%problema 6
%Sa se scrie un predicat care elimina dintr-o lista toate elementele care
%se repeta (ex: l=[1,2,1,4,1,3,4] => l=[2,3])

%numarAparitii2( L: lista, X: integer, N: integer)
%L - lista in care numaram aparitiile lui X
%X - numar intreg a carui aparitii le contorizam
%N - numarul de aparitii a lui X in L
%model de flux (i,i,i) sau (i,i,o)

numarAparitii2([],_,0).
numarAparitii2([Cap|Coada],X,N):-Cap =:= X,
    numarAparitii2(Coada,X,N1),
    N is N1+1,!.
numarAparitii2([_|Coada],X,N):-numarAparitii2(Coada,X,N).

%identitate4(X: element, Y: element)
%verifica daca X si Y sunt identice
%model de flux (i,i), (i,o) sau (o,i)

identitate4(X,X).

%stergeAparitii2(L: lista, Rez: lista, X: integer)
%L - lista din care stergem aparitiile lui X
%Rez - lista rezultat
%X - atom care va fi eliminat
%model de flux (i,i,i) sau (i,o,i)

stergeAparitii2([],X,_):-identitate4(X,[]).
stergeAparitii2([Cap|Coada],X,Eliminat):-Cap = Eliminat,
    stergeAparitii2(Coada,X,Eliminat),!.
stergeAparitii2([Cap|Coada],[Cap|X],Eliminat):-stergeAparitii2(Coada,X,Eliminat).

%eliminaDuplicate( L: lista, X: lista)
%L - lista din care eliminam duplicatele
%X - lista fara duplicate
%model de flux (i,i) sau (i,o)

eliminaDuplicate([],X):-identitate4([],X).
eliminaDuplicate([Cap|Coada],X):-numarAparitii2(Coada,Cap,N),
    N > 0,
    stergeAparitii2(Coada,CoadaNoua,Cap),
    eliminaDuplicate(CoadaNoua,X),!.
eliminaDuplicate([Cap|Coada],[Cap|X]):-eliminaDuplicate(Coada,X).

%Sa se elimine toate aparitiile elementului maxim dintr-o lista de numere
%intregi.
%gasesteMaxim( L:lista, X: integer)
%L - lista in care cautam elementul maxim
%X - cel mai mare element din X
%model de flux (i,i) sau (i,o)

gasesteMaxim([X],X):-!.
gasesteMaxim([Cap|Coada],X):-gasesteMaxim(Coada,X1),
    X is max(Cap,X1).

%eliminaMaxim( L: lista, X: lista)
%L - lista din care eliminam aparitiile elementului maxim
%X - lista rezultat
%model de flux (i,i) sau (i,o)
eliminaMaxim(L,X):-gasesteMaxim(L,X1),
    stergeAparitii2(L,X,X1).

%problema 7
%Sa se scrie un predicat care intoarce reuniunea a doua multimi.

%contine2(L: lista, X: simbol)
%L - lista la care verificam apartenenta lui X
%X - simbolul a carui apartenenta o verificam
%model de flux (i,i) sau (i,o)

contine2([Cap|_],X):-X is Cap,!.
contine2([_|Coada],X):-contine2(Coada,X).

%reuniune(L1: lista, L2: lista, X - lista)
%L1,L2 - liste(multimi) a caror reuniune o cautam
%X - reuniune lui L1 si L2
%model de flux (i,i,i) sau (i,i,o)

reuniune([],X,X).
reuniune([Cap|Coada],L2,X):-contine2(L2,Cap),
    reuniune(Coada,L2,X),!.
reuniune([Cap|Coada],L2,[Cap|X]):-reuniune(Coada,L2,X).

%Sa se scrie un predicat care, primind o lista, intoarce multimea
%tuturor perechilor din lista. De ex, cu [a, b, c, d] va produce
%[[a, b], [a, c], [a, d], [b, c], [b, d], [c, d]]

%identitate5(X: element, Y: element)
%verifica daca X si Y sunt identice
%model de flux (i,i), (i,o) sau (o,i)

identitate5(X,X).

%facePerechi(L: lista, Rez: lista, L: symbol)
%L - lista originala
%Rez - lista care contine perechi de forma [L,X], unde X
%este un element al listei L
%X - numarul care va face parte din perechi
%model de flux (i,i,i) sau (i,o,i)

facePerechi([],L,_):-identitate5([],L).
facePerechi([Cap|Coada],[[X,Cap]|L],X):-facePerechi(Coada,L,X).

%combinaListe(L1: lista, L2: lista, L3: lista)
%L1 - lista la care concatenam L2
%L2 - lista pe care o concatenam la finalul listei L1
%L3 - lista rezultat
%model de flux (i,i,i) sau (i,i,o)

combinaListe([],L,L).
combinaListe([Cap|Coada],L,[Cap|X]):-combinaListe(Coada,L,X).

%adaugaPereche(L: lista, Rez: lista)
%L - lista pentru care creem combinarile de doua elemente
%Rez - lista rezultat
%model de flux (i,i) sau (i,o)

adaugaPereche([],[]).
adaugaPereche([Cap|Coada],X):-adaugaPereche(Coada,X1),
    facePerechi(Coada,X2,Cap),
    combinaListe(X2,X1,X).

%problema 8
% Sa se scrie un predicat care testeaza daca o lista este multime.

%nuApare( L: lista, X: integer)
%L -lista in care verificam daca nu apare X
%X - elementul pe care il cautam
%model de flux (i,i)

nuApare([],_).
nuApare([Cap|Coada],X):- X \== Cap,
    nuApare(Coada,X).

%esteMultime(L: lista)
%L - lista pe care o verificam daca este multime sau nu
%model de flux (i)

esteMultime([]).
esteMultime([Cap|Coada]):-nuApare(Coada,Cap),
    esteMultime(Coada).

%Sa se scrie un predicat care elimina primele 3 aparitii ale unui element
%intr-o lista. Daca elementul apare mai putin de 3 ori, se va elimina de
%cate ori apare.

%elimina3(L: lista, El: integer, X: lista)
%L - lista din care eliminam primele 3 aparitii ale lui El
%El - elementul pe care il cautam
%X - lista rezultat
%model de flux (i,i,i) sau (i,i,o)
% predicatul functioneaza ca si wrapper care contine ca si al 4-lea
% element contorul de aparitii pe care le-a eliminat (aparitii ale lui
% El in lista L)

elimina3(L,El,X):-elimina3(L,El,X,3).
elimina3([],_,[],_):-!.
elimina3(X,_,X,0).
elimina3([Cap|Coada],Eliminat,X,N):- Cap == Eliminat,
    N1 is N-1,
    elimina3(Coada,Eliminat,X,N1),!.
elimina3([Cap|Coada],Eliminat,[Cap|X],N):-elimina3(Coada,Eliminat,X,N).

%problema 9
%Sa se scrie un predicat care intoarce intersectia a doua multimi.

%nuApare2( L: lista, X: integer)
%L -lista in care verificam daca nu apare X
%X - elementul pe care il cautam
%model de flux (i,i)

nuApare2([],_).
nuApare2([Cap|Coada],X):- X \== Cap,
    nuApare2(Coada,X).

%intersectie(L1: lista, L2: lista, L3: lista)
%L1, L2 - liste(multimi) a caror intersectie dorim sa o aflam
% L3 - lista rezultat care contine toate elementele din L1 care apar si
% in L2
intersectie([],_,[]):-!.
intersectie(_,[],[]):-!.
intersectie([Cap|Coada],L,[Cap|X]):-not(nuApare2(L,Cap)),
    intersectie(Coada,L,X),!.
inersectie([_|Coada],L,X):-intersectie(Coada,L,X).

%Sa se construiasca lista (m, ..., n), adica multimea numerelor intregi
%din intervalul [m, n].

%creeazaMultime(X: integer, Y: integer, L: lista)
%X - numar intreg, primul element din lista
%Y - numar intreg, ultimul element din lista
%L - lista care contine toate numerele intregi de la X la Y inclusiv
%model de flux (i,i,i) sau (i,i,o)
creeazaMultime(X,X,[X]):-!.
creeazaMultime(X,Y,[X|L]):-X =< Y,X1 is X+1,
    creeazaMultime(X1,Y,L).

%problema 10

%Sa se intercaleze un element pe pozitia a n-a a unei liste.

%intercalare(L: lista, Rez: lista, V: integer, N: integer)
%L - lista in care il intercalam pe V
%Rez - lista rezultat cu V pe pozitia N
%V - numar intreg pe care il intercalam
%N - pozitia unde va fi intercalat V in L
%model de flux (i,i,i,i) sau (i,o,i,i)

intercalare(X,[V|X],V,1):-!.
intercalare([Cap|Coada],[Cap|X],V,N):-N1 is N-1,
    intercalare(Coada,X,V,N1).


%Definiti un predicat care intoarce cel mai mare divizor comun al
%numerelor dintr-o lista.

%cmmdc2(X: integer, Y:integer, Z: integer)
%X,Y - intregi pozitivi caroroa vrem sa le gasim cel mai mare divizor
%comun
%Z - cel mai mare divizor comun al lui X si Y
%model de flux (i,i,i) sau (i,i,o)
%formula recursiva foloseste algoritmul lui Euclid

cmmdc2(X,0,X):-!.
cmmdc2(0,X,X):-!.
cmmdc2(X,Y,Z):- X \= 0,Y \= 0,
    X > Y, X1 is X-Y,
    cmmdc2(X1,Y,Z),!.
cmmdc2(X,Y,Z):- X \= 0,Y \= 0,
    X1 is Y-X,
    cmmdc2(X,X1,Z),!.

%problema 11

%Sa se scrie un predicat care sa testeze daca o lista formata din numere
%intregi are aspect de "vale"(o multime se spune ca are aspect de "vale"
%daca elementele descresc pana la un moment dat, apoi cresc. De ex.
%10 8 6 9 11 13).

%vale(L: lista)
%L - lista pe care o verificam sa aiba aspect de vale
%model de flux (i)
%predicatul functioneaza ca un wrapper pentru varianta cu 3 parametri
%in care ceilalti 2 parametri reprezinta elementul curent pe care il
%comparam cu elementul care urmeaza dupa el si starea in care se afla
%lista, ea incepand in starea "start", trece in "scade" si la final
%ajunge in "creste" daca respecta forma data in enunt

vale([]):-!.
vale([Cap|Coada]):-vale(Coada,Cap,start).
vale([],_,creste):-!.
vale([Cap|Coada],X,creste):- X < Cap,
    vale(Coada,Cap,creste),!.
vale([Cap|Coada],X,scade):- X < Cap,
    vale(Coada,Cap,creste),!.
vale([Cap|Coada],X,scade):- X > Cap,
    vale(Coada,Cap,scade),!.
vale([Cap|Coada],X,start):- X > Cap,
    vale(Coada,Cap,scade).

%Sa se calculeze suma alternanta a elementelor unei liste
%(l1 - l2 + l3 ...).

%sumaAlternanta(L: lista, X: integer)
%L - lista careia ii calculam suma alternanta a elementelor
%X - rezultatul sumei alternante
%model de flux (i,i) sau (i,o)
sumaAlternanta([],0).
sumaAlternanta([X],X):-!.
sumaAlternanta([Cap|Coada],X):-sumaAlternanta(Coada,X1),
    X is Cap-X1.

%problema 12
%Sa se scrie un predicat care substituie intr-o lista un element prin
%altul.

%substituieElement(L1: lista, L2: lista, Vechi: integer, Nou: integer)
%L1 - lista in care va fi substitui Vechi cu Nou
%L2 - lista rezultat
%Vechi - elementul care va fi inlocuit
%Nou - elementul care il va inlocui
%model de flux (i,i,i,i) sau (i,o,i,i)

substituieElement([],[],_,_).
substituieElement([Cap|Coada],[Nou|X],Vechi,Nou):- Cap is Vechi,
    substituieElement(Coada,X,Vechi,Nou),!.
substituieElement([Cap|Coada],[Cap|X],Vechi,Nou):-substituieElement(Coada,X,Vechi,Nou).

%Sa se construiasca sublista (lm, ..., ln) a listei (l1, ..., lk).
%sublista(L: lista, Rez: lista, M: integer, N: integer)
% L -lista din care extragem sublista care contine termeni de indici de
% la M la N
% Rez - sublista rezultat
% M - numar intreg strict pozitiv mai mic sau egal decat N
% N - numar intreg strict pozitiv mai mare sau egal decat M
% model de flux (i,i,i,i) sau (i,o,i,i)

sublista(L,X,M,N):-sublista(L,X,M,N,1).
sublista(_,[],_,N,N1):-N1 is N+1,!.
sublista([Cap|Coada],[Cap|X],M,N,CT):- CT >= M,
    CT1 is CT+1,
    sublista(Coada,X,M,N,CT1),!.
sublista([_|Coada],X,M,N,CT):-CT1 is CT+1,
    sublista(Coada,X,M,N,CT1).

%problema 13
% Sa se scrie un predicat care transforma o lista intr-o multime, in
% ordinea ultimei aparitii. Exemplu: [1,2,3,1,2] e transformat in
% [3,1,2].

%listaMultime2(L1:lista, L2:lista)
%L1 - lista pe care o transformam in multime
%L2 - lista rezultat(multime)
%model de flux (i,i) sau (i,o)
%la fiecare pas verificam daca multimea noastra contine cate un element
%din lista initiala, iar daca nu contine il adaugam
%se incepe cu o multime vida

listaMultime2([],[]).
listaMultime2([Cap|Coada],[Cap|X]):- \+contine(Coada,Cap),
    listaMultime2(Coada,X),!.
listaMultime2([_|Coada],X):-listaMultime2(Coada,X).

%Sa se calculeze cel mai mare divizor comun al elementelor unei liste.
%cmmdcMultime( L:lista, X: integer)
%L - lista de elemente careia ii calculam cmmdc-ul
%X - cmmdc-ul rezultat al listei
%model de flux (i,i) sau (i,o)
%daca lista are un element cmmdc-ul ei este acel element, altfel
%calculam succesiv care este cmmdc-ul unui element al listei si cmmdc-ul
%restului listei
cmmdcMultime([X],X):-!.
cmmdcMultime([Cap|Coada],Rez):-cmmdcMultime(Coada,X),
    cmmdc(Cap,X,Rez).

%problema 14
%Sa se scrie un predicat care testeaza egalitatea a doua multimi, fara
%sa se faca apel la diferenta a doua multimi.

%cardinalEgal(L1: lista, L2: lista)
%L1,L2 - liste pe care le verificam sa vedem daca au acelasi numar de
%elemente
%model de flux (i,i)

cardinalEgal([],[]).
cardinalEgal([_|Coada1],[_|Coada2]):-cardinalEgal(Coada1,Coada2).

%contineElement(L: lista, E: simbol)
%L - lista pe care o verificam daca contine elementul E
%E - elementul a carui aparitie in lista o verificam
%model de flux (i,i)

contineElement([Cap|_],Element):-Element is Cap,!.
contineElement([_|Coada],Element):-contineElement(Coada,Element).

%multimiEgale(L1: lista, L2: lista)
%L1, L2 - liste (multimi) pe care dorim sa le verificam daca sunt egale
%model de flux (i,i)
%predicatul functioneaza ca un wrapper pentru o varianta a sa cu 3
%elemente, cel cu 2 argumente verificand prima data daca au cardinal
%egal, iar cel cu 3 argumente verifica daca fiecare element din L1
%exista in L2
multimiEgale([],[]).
multimiEgale(L1,L2):-cardinalEgal(L1,L2),
    multimiEgale(L1,L2,verificat).
multimiEgale([],_,verificat).
multimiEgale([Cap|Coada],L,verificat):-contineElement(L,Cap),
    multimiEgale(Coada,L,verificat).

%Definiti un predicat care selecteaza al n-lea element al unei liste.
%nElement(L: lista, N: integer, X: integer)
%L - lista din care vrem sa selectam al N-lea element
%N - indicele elementului pe care dorim sa il selectam
%X - rezultatul, elementul de pe pozitia N
%model de flux (i,i,i) sau (i,i,o)

nElement([Cap|_],1,Cap):-!.
nElement([_|Coada],N,X):-N1 is N-1,
    nElement(Coada,N1,X).


















