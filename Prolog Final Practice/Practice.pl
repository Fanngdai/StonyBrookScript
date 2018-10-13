% Wake sean up at 3 AM
edge(1,2).
edge(1,3).
edge(3,4).

reach(X,Y) :-
  edge(X,Y).

reach(X,Y) :-
  edge(X,Z),
  reach(Z,Y).



% gcd(25,10,X)
gcd(A,A,A).

gcd(A,B,G) :-
  A > B,
  C is A-B,
  gcd(C,B,G).

gcd(A,B,G) :-
  A < B,
  C is B-A,
  gcd(C,A,G).



% anagram([c,i,n,e,m,a],[i,c,e,m,a,n]).   yes
% anagram([f,o,d,o,r],[f,r,o,d,o]).       yes
% anagram([a,b],[b,c]).                   no
anagram([],[]).
anagram([X|Xs],Y) :-
  delete(X,Y,L),
  anagram(Xs,L).
% Delete the X value if found o.w., return false
delete(X,[X|L],L).
delete(X,[Y|Ys],[Y|L]) :-
  delete(X,Ys,L).



palandromeh([],[]).
palandromeh([X|T1],[X|T2]) :-
  palandromeh(T1,T2).

palandrome(L1) :-
  reverse(L1,L2),
  palandromeh(L1,L2).



% Does not work for xsb try https://swish.swi-prolog.org/
% Opposite ans for odd
% even_path(1,1).     true
% even_path(1,2).     false
% even_path(1,3).     true
% even_path(1,4).     false
% even_path(1,5).     true
% even_path(1,6).     false
% even_path(1,7).     false
edge(1,2).
edge(2,3).
edge(3,4).
edge(4,5).
edge(5,6).

even_path(X,X).

even_path(X,Y) :-
  edge(X,Z),
  odd_path(Z,Y).

odd_path(X,Y) :-
  edge(X,Z),
  even_path(Z,Y).



% running_sum([1,3,5,7],L2),write(L2),nl,fail; true.      [1,4,9,16]
running_sum_helper([],[],_).
running_sum_helper([X|Xs],[A|Ys],N) :-
  A is X + N,
  running_sum_helper(Xs,Ys,A).

running_sum(InputList, OutputList) :-
  running_sum_helper(InputList, OutputList, 0).



% interleaveAll([1,2],[3,4],L), write(L),nl, fail; true.
interleaveAll_helper(X,[],X).
interleaveAll_helper([],Y,Y).

interleaveAll_helper([X|Xs],[Y|Ys],[X|L]) :-
  interleaveAll_helper(Xs,[Y|Ys],L).
interleaveAll_helper([X|Xs],[Y|Ys],[Y|L]) :-
  interleaveAll_helper([X|Xs],Ys,L).

interleaveAll(L1,L2,L) :-
  findall(R,interleaveAll_helper(L1,L2,R),L).



power_set_helper([],_).
power_set_helper([H|T],R) :-
  delete(H,R,L),
  power_set_helper(T,L).

power_set(L,R) :-
  findall(S, power_set_helper(S,L), R).

% powerset([1,2,3,4],L), write(L), nl, fail.
append([],L,L).
append([H|T], L, [H|T2]):-
	append(T,L,T2).

insertall([],_,[]).
insertall([H|T],X,[[X|H]|T2]):-
	insertall(T,X,T2).

powerset([],[[]]).
powerset([H|T], P):-
	insertall(PT, H, P2),
	append(P2, PT, P),
  powerset(T, PT).



% cartesian_product([1,2],[3,4],L), write(L), nl, fail; true.
cartesian_product([],_,[]).
cartesian_product([X|Xs],L,R) :-
  pair(X,L,A),
  append(A,S,R),
  cartesian_product(Xs,L,S).

pair(_,[],[]).                  %
pair(X,[Y|Ys],[p(X,Y)|L]) :-
  pair(X,Ys,L).



member(X,[X|T]).
member(X, [H|T]) :-
  member(X,T).

% reflexive([0,1,2,3],[edge(0,0),edge(0,2),edge(0,3),edge(2,3)]).
% reflexive([0,1,2,3],[edge(0,0),edge(1,1),edge(2,2),edge(3,3),edge(0,1),edge(1,0),edge(0,3),edge(3,0)]).
reflexive([],_).
reflexive([H|T],G) :-
  member(edge(H,H),G),
  reflexive(T,G).

% symmetric([0,1,2,3],[edge(0,0),edge(0,2),edge(0,3),edge(2,3)]).
% symmetric([0,1,2,3],[edge(0,0),edge(1,1),edge(2,2),edge(3,3),edge(0,1),edge(1,0),edge(0,3),edge(3,0)]).
symmetric_helper([],_).
symmetric_helper([edge(X,Y)|T],L) :-
  member(edge(Y,X),L),
  symmetric_helper(T,L).

symmetric(A,G) :-
  symmetric_helper(G,G).

% transitive([0,1,2,3],[edge(0,0),edge(0,2),edge(0,3),edge(2,3)]).
% transitive([0,1,2,3],[edge(0,0),edge(1,1),edge(2,2),edge(3,3),edge(0,1),edge(1,0),edge(0,3),edge(3,0)]).
transitive_helper([],_).
transitive_helper(G) :-
  (member(edge(X,Y),G),
  \+ member(edge(Y,Z),G));
  (member(edge(X,Y),G),
  member(edge(Y,Z),G),
  member(edge(X,Z),G)).

transitive(A,G) :-
  transitive_helper(G).

% non_transitive([0,1,2,3],[edge(0,0),edge(0,2),edge(0,3),edge(2,3)]).
% non_transitive([0,1,2,3],[edge(0,0),edge(1,1),edge(2,2),edge(3,3),edge(0,1),edge(1,0),edge(0,3),edge(3,0)]).
non_transitive_helper(G) :-
  member(edge(X,Y),G),
  member(edge(Y,Z),G),
  \+ member(edge(X,Z),G).

non_transitive(A,G) :-
  non_transitive_helper(G).

% anti_symmetric([0,1,2,3],[edge(0,0),edge(0,2),edge(0,3),edge(2,3)]).
% anti_symmetric([0,1,2,3],[edge(0,0),edge(1,1),edge(2,2),edge(3,3),edge(0,1),edge(1,0),edge(0,3),edge(3,0)]).
anti_symmetric(A,G) :-
  member(edge(X,Y),G),
  \+ member(edge(Y,Z),G).

% odd_gen(5,L).         [1,3,5,7,9]
odd_gen_helper(0,_,[]).
odd_gen_helper(I,S,[A|L]) :-
  I > 0,
  A is S + 2,
  B is I - 1,
  odd_gen_helper(B,A,L).

odd_gen(0,[]).
odd_gen(I,L) :-
  I > 0,
  odd_gen_helper(I,-1,L).


% rotate
rotate([],0,[]).
rotate(L,0,L).
rotate([H|T],I,R) :-
  I > 0,
  A is I - 1,
  append(T,[H],S),
  rotate(S,A,R).

%
length([],0).
length([_|T],S) :-
  length(T,R),
  S is R + 1.

% rotateAll([1,2,3,4,5],O).
rotateAll(L,O) :-
  length(L,LEN),
  rotateAll(L,O,[],LEN).

rotateAll(_,R,R,0).
rotateAll(L,R,P,N) :-
  N > 0,
  rotate(L,N,M),
  N2 is N-1,
  rotateAll(L,R,[M|P],N2).

reverseh([],X,X).
reverseh([H|T],A,Z) :-
  reverseh(T,[H|A],Z).

reverse(L,Z) :-
  reverseh(L,[],Z).
