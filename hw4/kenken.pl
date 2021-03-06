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
	subtract(D,J,K,T);
	subtract(D,K,J,T).

matchCage(T,/(Q,J,K)) :-
	divide(Q,J,K,T);
	divide(Q,K,J,T).

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
	Difference #= E1 - E2.

divide(Quotient,J,K,T) :-
	getElement(J,T,E1),
	getElement(K,T,E2),
	Quotient #= E1//E2.

getElement(I-J,T,Elem) :-
	nth(I,T,Row),
	nth(J,Row,Elem).

constrainSolution(T) :-
	maplist(fd_labeling,T).



plain_kenken(N,C,T) :-
	validBoard_P(N,T),
	validRows_P(N,T),
	validColumns_P(T),
	validCages_P(T,C).

validBoard_P(N,T) :-
	validNumRows(N,T),
	validNumCols(N,T).

nList(N,N,[N]).
nList(Start,End,[Head|Tail]) :-
	Start =< End,
	Start = Head,
	NewStart is Start + 1,
	nList(NewStart,End,Tail).

validRows_P(N,T) :-
	nList(1,N,NList),
	maplist(validRow_P(NList),T).

validRow_P(NList,Row) :-
	permutation(NList,Row).

validColumns_P(T) :-
	transpose(T,Trans),
	validColumn_P(Trans).

uniqueList([]).
uniqueList([Head|Tail]) :- \+ member(Head,Tail), uniqueList(Tail).

validColumn_P(T) :-
	maplist(uniqueList,T).

validCages_P(T,C) :-
	maplist(validCage_P(T),C).

validCage_P(T,Cage) :-
	matchCage_P(T,Cage).

matchCage_P(T,+(S,L)) :-
	add_P(S,L,T,0).

matchCage_P(T,*(P,L)) :-
	multiply_P(P,L,T,1).

matchCage_P(T,-(D,J,K)) :- 
	subtract_P(D,J,K,T);
	subtract_P(D,K,J,T).

matchCage_P(T,/(Q,J,K)) :-
	divide_P(Q,J,K,T);
	divide_P(Q,K,J,T).

add_P(Sum,[],_,Sum).
add_P(Sum,[Label|Tail],T,Acc) :-
	getElement(Label,T,Elem),
	NAcc is Acc + Elem,
	add_P(Sum,Tail,T,NAcc).


multiply_P(Product,[],_,Product).
multiply_P(Product,[Label|Tail],T,Acc) :-
	getElement(Label,T,Elem),
	NAcc is Acc * Elem,
	multiply_P(Product,Tail,T,NAcc).

subtract_P(Difference,J,K,T) :-
	getElement(J,T,E1),
	getElement(K,T,E2),
	Difference is E1 - E2.

divide_P(Quotient,J,K,T) :-
	getElement(J,T,E1),
	getElement(K,T,E2),
	Quotient is E1//E2.

% Don't run with plain_kenken!
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
%% [[5,6,3,4,1,2],
%%  [6,1,4,5,2,3],
%%  [4,5,2,3,6,1],
%%  [3,4,1,2,5,6],
%%  [2,3,6,1,4,5],
%%  [1,2,5,6,3,4]]

% Used for running statistics/2
kenken_testcase2(
  4,
  [
   +(8, [1-1,2-1,3-1]),
   -(2, 1-2, 1-3),
   /(2, 1-4, 2-4),
   /(2, 2-2, 2-3),
   *(24, [3-2,4-1,4-2]),
   -(2, 3-3, 4-3),
   *(3, [3-4, 4-4])
  ]
).
%% [[4,1,3,2],
%%  [3,2,1,4],
%%  [1,4,2,3],
%%  [2,3,4,1]]

kenken_testcase3(
	3,
	[
     *(2, [1-1,2-1,2-2]),
     -(1, 1-2, 1-3),
     -(1, 3-1, 3-2),
     *(3, [2-3,3-3])
   ]
).
%% [[1,3,2],
%%  [2,1,3],
%%  [3,2,1]]

%% statistics(cpu_time, [SinceStart | [SinceLast]]), fd_set_vector_max(255), kenken_testcase2(N,C), kenken(N,C,T), statistics(cpu_time, [NewSinceStart | [ExecutionTime]]),write('Execution took: '), write(ExecutionTime), write(' ms.'), nl.
%% --> Execution took: 2 ms.
%% statistics(cpu_time, [SinceStart | [SinceLast]]), kenken_testcase2(N,C), plain_kenken(N,C,T), statistics(cpu_time, [NewSinceStart | [ExecutionTime]]),write('Execution took: '), write(ExecutionTime), write(' ms.'), nl.
%% --> Execution took: 1172 ms.
%% --> kenken is 586 times faster.

%% statistics(global_stack, [SinceStart | [SinceLast]]), fd_set_vector_max(255), kenken_testcase2(N,C), kenken(N,C,T), statistics(global_stack, [NewSinceStart | [ExecutionMemory]]),write('Global Stack is:'), write(ExecutionMemory), write(' bytes'), nl.
%% --> Global stack is: 33547024 bytes
%% statistics(global_stack, [SinceStart | [SinceLast]]), kenken_testcase2(N,C), plain_kenken(N,C,T), statistics(global_stack, [NewSinceStart | [ExecutionMemory]]),write('Global Stack is: '), write(ExecutionMemory), write(' bytes'), nl.
%% --> Global stack is: 33546480 bytes
%% --> About the same

%% statistics(local_stack, [SinceStart | [SinceLast]]), fd_set_vector_max(255), kenken_testcase2(N,C), kenken(N,C,T), statistics(local_stack, [NewSinceStart | [ExecutionMemory]]),write('Local Stack is:'), write(ExecutionMemory), write(' bytes'), nl.
%% --> Local stack is: 16774200 bytes
%% statistics(local_stack, [SinceStart | [SinceLast]]), kenken_testcase2(N,C), plain_kenken(N,C,T), statistics(local_stack, [NewSinceStart | [ExecutionMemory]]),write('Local Stack is: '), write(ExecutionMemory), write(' bytes'), nl.
%% --> Local stack is: 16768688 bytes
%% --> About the same

%% No Op KenKen API
%% C is now a list of constraints matching the form (<int>, <list of squares>) or (<int>, <square>, <square>)
%% no_op_kenken still checks:
%%  - the length of all rows and columns in validBoard
%%  - that each row is a permutation of the list of ints from 1 to N
%%  - that each column is a permutation of the list of ints from 1 to N
%%  - if we are using the fd_domain solver we still want to constrain the values each square to get a result.
%% no_op_kenken(N,C,T) :-
%% 	validBoard(N,T),
%% 	validRows(T),
%% 	validColumns(T),
%% 	validCages(T,C),
%% 	constrainSolution(T).

%% validBoard checks the length each row and column
%% validBoard(N,T).

%% validRows checks that each row is a permutation of the list of ints from 1 to N
%% validRows(T).

%% validColumns checks that each column is a permutation of the list of ints from 1 to N
%% validColumns(T).

%% Runs validCage on all Cages in C
%% validCages(T,C).

%% validates a cage using matchCage
%% validCage(T,Cage).

%% matches cages with a list of indices second argument
%% - needs to check if add or multiply satisfies the cage.
%% matchCage(T,(S,L)) :-
%% 	add(S,L,T,0);
%% 	multiply(S,L,T,1).

%% matches cages with two indice arguments
%% - needs to check if subtract or divide satisfies the cage
%% matchCage(T,(D,J,K)) :-
%% 	subtract(D,J,K,T);
%% 	subtract(D,K,J,T);
%% 	divide(D,J,K,T);
%% 	divide(D,K,J,T).

%% Methods for computing cage arithmetic
%% add(Sum,List,T,Acc).
%% multiply(Product,List,T,Acc).
%% subtract(Difference,J,K,T).
%% divide(Quotient,J,K,T).

%% Constrains the solution using f_domain solver
%% constrainSolution(T).

