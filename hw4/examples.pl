parent(kim,holly).
parent(margaret,kim).
parent(margaret,kent).
parent(esther,margaret).
parent(herbert,margaret).
parent(herbert,jean).
gparent(GP,GC):-
	parent(GP,P),parent(P,GC).
ggparent(GGP,GGC):-
	gparent(GGP,P),parent(P,GGC).
ancestor(X,Y):- 
	parent(X,Y).
ancestor(X,Y):-
	parent(Z,Y),ancestor(X,Z).
sibling(X,Y):-
	parent(P,X),parent(P,Y),\+ (X=Y).
append2([],B,B).
append2([Head|TailA],B,[Head|TailC]):-
	append(TailA,B,TailC).
reverse2([],[]).
reverse2([Head|Tail],X):-
	reverse(Tail,Y),
	append(Y,[Head],X).

