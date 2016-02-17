kenken(N,C,T) :-
	validBoard(N,T),
	validRows(T),
	validColumns(T),
	validCages(T,C),
	constrainSolution(T).

validBoard(N,T) :- 
	validNumRows(N,T),
	validNumCols(N,T),
	validNumbers(N,T).

validNumRows(N,T) :- 
	length(T,N).

validRowLength(N,Row) :- 
	length(Row,N).

validNumCols(N,T) :- 
	maplist(validRowLength(N),T).

validNumbers(N,T) :- 
	maplist(validRowNumbers(N),T).

validRowNumbers(N,Row) :-
	fd_domain(Row,1,N).

validRows(T) :-
	maplist(validRow,T).

validRow(Row) :-
	fd_all_different(Row).

validColumns(T) :-
	transpose(T,Trans),
	validRows(Trans).

%% SWI Prolog Transpose implementation
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

validCages(T,C) :-
	maplist(validCage(T),C).

validCage(T,Cage) :-
	matchCage(T,Cage).

matchCage(T,+(S,L)) :-
	add(S,L,T,0).

matchCage(T,*(P,L)) :-
	multiply(P,L,T,1).

matchCage(T,-(D,J,K)) :- 
	subtract(D,J,K,T).

matchCage(T,/(Q,J,K)) :-
	divide(Q,J,K,T).

add(Sum,[],_,Sum).
add(Sum,[Label|Tail],T,Acc) :-
	getElement(Label,T,Elem),
	NAcc #= Acc + Elem,
	add(Sum,Tail,T,NAcc).


multiply(Product,[],_,Product).
multiply(Product,[Label|Tail],T,Acc) :-
	getElement(Label,T,Elem),
	NAcc #= Acc * Elem,
	multiply(Product,Tail,T,NAcc).

subtract(Difference,J,K,T) :-
	getElement(J,T,E1),
	getElement(K,T,E2),
	Difference #= E1 - E2;
	Difference #= E2 - E1.

divide(Quotient,J,K,T) :-
	getElement(J,T,E1),
	getElement(K,T,E2),
	Quotient #= E1//E2;
	Quotient #= E2//E1.

getElement(I-J,T,Elem) :-
	nth(I,T,Row),
	nth(J,Row,Elem).

constrainSolution(T) :-
	maplist(fd_labeling,T).

add(10,[1-4,2-2,3-1],[[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],0).
multiply(32,[1-4,2-2,3-1],[[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],1).
subtract(2,1-4,2-2,[[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]]).
divide(2,1-4,2-2,[[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]]).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],+(10,[1-4,2-2,3-1])).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],*(32,[1-4,2-2,3-1])).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],-(2,1-4,2-2)).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],-(2,2-2,1-4)).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],/(2,1-4,2-2)).
validCage([[2,1,3,4],[1,2,3,4],[4,3,2,1],[3,1,2,4]],/(2,2-2,1-4)).

kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).

kenken(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ],[[5,6,3,4,1,2],
     [6,1,4,5,2,3],
     [4,5,2,3,6,1],
     [3,4,1,2,5,6],
     [2,3,6,1,4,5],
     [1,2,5,6,3,4]]
).
%% validCages(N,C,T).

%% plain_kenken(N,C,T).

%% nList(N,N,[N]).
%% nList(Start,End,[Head|Tail]) :-
%% 	Start =< End,
%% 	Start = Head,
%% 	NewStart is Start + 1,
%% 	nList(NewStart,End,Tail).

%% permNList(N,List) :-
%% 	nList(1,N,Nlist),
%% 	permutation(Nlist,List).

%% validRow(N,Row) :- permNList(N,Row).

%% validRows(N,T) :- maplist(validRow(N),T).

%% validColumn(N,Col) :- permNList(N,Col).

%% validColumns(N,T) :-
%% 	transpose(T,Trans),
%% 	validRows(N,Trans).

kk_test0(1,[]).
kk_test1(2,[]).

%% validRow(4,R).
