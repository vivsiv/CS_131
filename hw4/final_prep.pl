sorted(L,S) :- perm(L,S), sorted(S).
sorted([]).
sorted([H,TH|T]) :-
	H =< TH,
	sorted([TH|T]).
perm([],[]).
perm([H1|T1],L) :-
	perm(T1,AB),
	append(A,B,AB),
	append(A,[X|B],L).

append([],L,L).
append([H|TA],B,[H|TC]) :-
	append(TA,B,TC).

reverse([],[]).
reverse([H|T],L) :-
	reverse(T,RT),
	append(RT,[H],L).
	
