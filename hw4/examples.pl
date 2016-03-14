female(kim).
female(holly).
female(margaret).
female(esther).
female(jean).

male(herbert).
male(kent).

parent(kim,holly).
parent(margaret,kim).
parent(margaret,kent).
parent(esther,margaret).
parent(herbert,margaret).
parent(herbert,jean).

mother(P,C) :-
	parent(P,C),female(P).
father(P,C) :-
	parent(P,C),male(P).
sibling(X,Y):-
	parent(P,X),parent(P,Y),\+ (X=Y).
sister(S,P) :-
	sibling(S,P),female(S).
gparent(GP,GC):-
	parent(GP,P),parent(P,GC).
grandson(GP,GC) :-
	gparent(GP,GC),male(GC).
ggparent(GGP,GGC):-
	gparent(GGP,P),parent(P,GGC).
ancestor(X,Y):- 
	parent(X,Y).
ancestor(X,Y):-
	parent(Z,Y),ancestor(X,Z).

third(L,T) :- L = [_,_,T|_].
firstPair(L) :- L = [X,X|_].
del3(L,R) :- 
	L = [F,S,_|Tail],
	R = [F,S|Tail].

dupList([],[]).
dupList([H|Tail1],[H,H|Tail2]) :- 
	dupList(Tail1,Tail2).

isDuped(L) :- dupList(_,L).

oddSize([_]).
oddSize([_|T]) :- \+ oddSize(T).

evenSize([]).
evenSize([_|T]) :- \+ evenSize(T).

myPrefix([],_).
myPrefix(_,[]) :- false.
myPrefix([H|PT],[H|LT]) :- 
	prefix(PT,LT).

isMember(_,[]) :- false.
isMember(E,[E|_]).
isMember(E,[_|T]):- isMember(E,T).

isUnion([],[],[]).
isUnion(S1,[],S1).
isUnion([],S2,S2).
isUnion([H|T1],S2,[H|TU]) :-
	isUnion(T1,S2,TU).

isIntersection([],[],[]).
isIntersection([],_,[]).
isIntersection(_,[],[]).
isIntersection([H1|T1],S2,[H1|Int]) :-
	isMember(H1,S2),
	isIntersection(T1,S2,Int).

isEqual(S1,S2) :-
	isIntersection(S1,S2,S1),
	isIntersection(S1,S2,S2).

%% powerset([],[]).
%% powerset(P,S) :-
%% 	isIntersection(SS,S,SS),
%% 	isMember(SS,P),
	


append2([],B,B).
append2([Head|TailA],B,[Head|TailC]):-
	append(TailA,B,TailC).
reverse2([],[]).
reverse2([Head|Tail],X):-
	reverse(Tail,Y),
	append(Y,[Head],X).

