%%
%% The final version of fact/2 (factorial) we wrote using if-then-else.
%%

fact(N, F) :-
    (F > 1 ->  F1 is F/N, fact(N1, F1),N is N1-1
    ; F=:=1, N=0
    ).

%%===========================================================================
%% Solution to the zebra puzzle (https://en.wikipedia.org/wiki/Zebra_Puzzle)
%%===========================================================================

/*
** The representation we use is a structured term of the form:
**    house(Nationality, Color, Pet, Smoke, Drink)
*/

zebra(Houses) :- 
    Houses = [house(norwegian, _, _, _, _),
	      house(_, blue, _, _, _),
	      house(_,_, _, _, milk), _H4, _H5],
    member(house(englishman, red, _, _, _), Houses),
    member(house(spaniard, _, dog, _, _), Houses),
    member(house(_, green, _, _, coffee), Houses),
    member(house(ukranian, _, _, _, tea), Houses),
    rightof(house(_, ivory, _, _, _), house(_, green, _, _, _), Houses),
    member(house(_, _, snails, old_gold, _), Houses),
    member(house(_, yellow, _, kools, _), Houses),  
    nextto(house(_, _, _, chesterfields, _), house(_, _, fox, _, _), Houses),
    nextto(house(_, yellow, _, kools, _), house(_, _, horse, _, _), Houses),
    member(house(_, _, _, lucky_strike, orange_juice), Houses),
    member(house(japanese, _, _, parliaments, _), Houses).

rightof(L, R, [L,R,_,_,_]).
rightof(L, R, [_,L,R,_,_]).
rightof(L, R, [_,_,L,R,_]).
rightof(L, R, [_,_,_,L,R]).

nextto(H1, H2, Houses) :- rightof(H1, H2, Houses).
nextto(H1, H2, Houses) :- rightof(H2, H1, Houses).

owner_zebra(Owner) :-
    zebra(Houses),
    member(house(Owner, _, zebra, _, _), Houses).

zebra2(Houses):-
	Houses=[house(norwegian, _, _, _, _),
	      house(_, blue, _, _, _),
	      house(_,_, _, _, milk), _H4, _H5],
	member(house(englishman, red, _, _, _), Houses),
    	member(house(spaniard, _, dog, _, _), Houses),
    	member(house(_, green, _, _, coffee), Houses),
    	member(house(ukranian, _, _, _, tea), Houses),
	member(house(_, _, _, lucky_strike, orange_juice), Houses),
    	member(house(japanese, _, _, parliaments, _), Houses),
	member(house(_, _, snails, old_gold, _), Houses),
    	member(house(_, yellow, _, kools, _), Houses),
	rightof2(house(_,white,_,_,_),house(_,green,_,_,_),Houses),
	nextto2(house(_,_,_,chesterfields,_),house(_,_,fox,_,_),Houses)	
	.

nextto2(L,R,Houses):-
	rightof2(L,R,Houses);
	rightof2(R,L,Houses).

rightof2(L,R,[L,R,_,_,_]).
rightof2(L,R,[_,L,R,_,_]).
rightof2(L,R,[_,_,L,R,_]).
rightof2(L,R,[_,_,_,L,R]).


fact2(N,F):-
	( N > 0 -> N1 is N-1,fact2(N1, F1), F is F1* N 
	; N =:= 0, F=1).

p(X,Y,Z):-
	z(Z),!,
	x(X),
	y(Y).

z(a).
z(b).
x(1).
x(2).
y(3).
y(4).

%%=====================================================
%% Solution to the Man-Wolf-Goat-Cabbage Puzzle (15/4/2022)-------------------lab2
%%=====================================================

solve(Moves) :-
    length(Moves, _), initial(C), solve(C, Moves).

/* solve(+Config, ?Moves) */
solve(Config, []) :- final(Config).
solve(Config, [Move | Moves]) :-
    move(Config, Move, NewConfig),
    safe(NewConfig),
    solve(NewConfig, Moves).

safe(config(Man, Wolf, Goat, Cabbage)) :-
    together_or_separated(Man, Wolf, Goat),
    together_or_separated(Man, Goat, Cabbage).

together_or_separated(Coast, Coast, Coast).  % together
together_or_separated(_, Coast1, Coast2) :- opposite(Coast1, Coast2).

/*
** The representation of the configurations is
**    config(Man, Wolf, Goat, Cabbage)
*/

initial(config(w, w, w, w)).
final(config(e, e, e, e)).

/* move(Conf1, Move, Conf2) */
move(config(Coast, Coast, G, C), wolf, config(OppositeCoast, OppositeCoast, G, C)) :-
    opposite(Coast, OppositeCoast).
move(config(Coast, W, Coast, C), goat, config(OppositeCoast, W, OppositeCoast, C)) :-
    opposite(Coast, OppositeCoast).
move(config(Coast, W, G, Coast), cabbage, config(OppositeCoast, W, G, OppositeCoast)) :-
    opposite(Coast, OppositeCoast).
move(config(Coast, W, G, C), nothing, config(OppositeCoast, W, G, C)) :-
    opposite(Coast, OppositeCoast).

opposite(w, e).
opposite(e, w).



%%=====================================================
%% Solution to the Eight Queens problem
%%=====================================================

eightqueens(Queens) :-
    Queens = [1/_, 2/_, 3/_, 4/_, 5/_, 6/_, 7/_, 8/_],
    qsafe(Queens).

qsafe([]).
qsafe([X/Y | Rest]) :-
    qsafe(Rest),
    % member(X, [1,2,3,4,5,6,7,8]),
    member(Y, [1,2,3,4,5,6,7,8]),
    nocheck(Rest, X/Y).

/* nocheck(+Queens, +Queen) */
nocheck([], _).
nocheck([X1/Y1 | Rest], X/Y) :-
    % X =\= X1,
    Y =\= Y1,
    abs(Y1-Y) =\= abs(X1-X),
    nocheck(Rest, X/Y).

%%-----------------------

norm_product([],[]).
norm_product([H1|T],A):-
	check1(H1,T,A);
	norm_product(T,[H1|A]).

check1(H1,[H2|T],[A|B]):-
	H1 =:= "(" -> check2(H2,T,B),A = H2.

check2(_H2,[H3|T],[A|B]):- check3(H3,T,B),A = H3.

check3(H3,[H4|T],[A|B]):-
	H3 =:= "*" -> check4(H4,T,B),A = H4.

check4(_H4,[H5|T],B):-check5(H5,T,B).

check5(H5,T,B):-
	H5 =:= ")" -> true,B=T.

%%-----------------------------

maximize(Tree,MaxTree):-
	Tree=n(_,_,_),
	findmax(Tree,Max),
	changetree(Max,Tree,MaxTree),!.

changetree(Max,n(T1,T2,T3),n(T4,T5,T6)):-
	changetree(Max,T1,T4),
	changetree(Max,T2,T5),
	changetree(Max,T3,T6),!.
changetree(Max,_,Max).


findmax(n(T1,T2,T3),Max):-
	findmax(T1,MT1),findmax(T2,MT2),findmax(T3,MT3),
	findmax(MT1,MT2,MT3,Max).
findmax(N,N).
findmax(MT1,MT2,MT3,Max):-
	MT1>MT2 -> (MT1>MT3 -> Max = MT1 ; MT2>MT3 -> Max = MT2 ;Max = MT3); MT2> MT3 -> Max = MT2 ; Max = MT3.

checkifints(n(T1,T2,T3)):-
	integer(T1),integer(T2),integer(T3).

unoddsum(n(T1,T2,T3),TT):-
	checkifints(n(T1,T2,T3)) -> (T is T1 + T2 + T3, \+ even(T) -> replace(TT,17) ;n(T4,T5,T6), T4 = T1 ,T5 = T2, T6=T3)
	;TT=n(T4,T5,T6), unoddsum(T1,T4) ,unoddsum(T2,T5) , unoddsum(T3,T6). 
unoddsum(N,T):- T is N.
	

even(N):-
	mod(N,2) =:=0.

 replace(T,N):- T = N.


unoddsum2(T,TT):-
	unoddsum(T,TTT),
	unoddsum(TTT,TT).

%%----epanaliptiki 20-----------------------------------------------------
gcdnum(0, X, X):- X > 0, !.
gcdnum(X, Y, Z):- X >= Y, X1 is X-Y, gcdnum(X1,Y,Z).
gcdnum(X, Y, Z):- X < Y, X1 is Y-X, gcdnum(X1,X,Z).


findnum(s(T),N):-
	findnum(T,N1),N is N1+1 ,!.
findnum(_,0).

gets(1,s(0)).
gets(N,s(T)):-
	N>1 -> N1 is N-1 , gets(N1,T).

gcd(X,Y,GCD):-
	findnum(X,Numx),
	findnum(Y,Numy),
	gcdnum(Numx,Numy,Z),
	gets(Z,GCD),!.

%%----------------kanoniki 20----------------

prime_start([],[]).
prime_start([H|T],Teliko):-
	even(H) ->( checkprimes(T,[[]],H,Teliko),!)
	; Teliko= [].

checkprimes([],[Ss],S1,Teliko):-
	append(Ss,S1,Ss2), Teliko = [Ss2].
checkprimes([H|T],[Ss],S1,Teliko):-
	even(H) -> (append(Ss,S1,Ss2),checkprimes([T],[Ss2] , [H],Teliko))
	;( append([S1],[H],S11), checkprimes([T],[Ss],S11,Teliko)). 


checkprimes([H|T],[Ss],S1,Teliko):-
	append([S1],[H],S11), checkprimes([T],[Ss],S11,Teliko). 

abcd(A,B,C):-
	append(A,[B],C).

%%-Find-the-Kth-element-of-a-list
element_at([],[],_).
element_at(X,[H|T],P):-
	P=:=1-> X=H;
	element_at(X,T,P-1).



%%-Flatten-a-nested-list-structure.

myflatten([],[]).
myflatten([H|T],X):-
	myflatten(H,X1),myflatten(T,X2),append(X1,X2,X),!.

myflatten(N,X):-
	append([N],[],X).


%%-Sorting-a-list-of-lists-according-to-lengthh-of-sublists

lsort([],[[]]).
lsort([H|T],X):-
	lsort(T,X1),
	length(H,L1),
	insertlistbylegth(H,L1,X1,X),!.

insertlistbylegth( L,_,[],XX):-
	XX=[L].

insertlistbylegth(L,L1,[H|T],XX):-
	length(H,L2),
	L1>L2 -> (insertlistbylegth(L,L1,T,XX1),
	append([H],XX1,XX),!)
	; append([L],[H|T],XX).

%%

divisible(X,Y) :- 0 is X mod Y, !.

divisible(X,Y) :- X > Y+1, divisible(X, Y+1).

isPrime(2) :- true,!.
isPrime(X) :- X < 2,!,false.
isPrime(X) :- not(divisible(X, 2)).


%%


return_primes_i_to_n(I,N,[]):-I >N.

return_primes_i_to_n(I,N,L):-
	isPrime(I),!,
	return_primes_i_to_n(I+1,N,L1),!,
	Z is I,
	append([Z],L1,L).

return_primes_i_to_n(I,N,L1):-
	not(isPrime(I)),!,
	return_primes_i_to_n(I+1,N,L1),!.



%%


add_2args(X, Y, R) :-
    (   number(X), number(Y)  % Both X and Y are numbers, then...
    ->  R is X + Y            % Evaluate the expression
    ;   R = X + Y             % Else, just unify R with the expression
    ).

%%

and(A,B):-A,B.
or(A,B):-!,A;B.
nand(A,B):-not(and(A,B)).
nor(A,B):-not(or(A,B)).
xor(A,B):-and(not(and(A,B),or(A,B))).
equ(A,B):-!,A=B.


writeTF(A):- A -> write('true    ');write('false    ').

writeNextLine():- write(',\n').


table(A,B,C,TF):-
	tf(A),tf(B),tf(C),
	(TF->write('true    ');write('false    ')),writeNextLine().


tf(A):- A=true.
tf(A):- A=false.


%%-find-depth-of-tree-nL,N,R|N--

finddepth([],0).
finddepth(n(L,_,R),D):-
	finddepth(L,D1),
	finddepth(R,D2),
	(D1>D2-> D is D1+1 ; D is D2+1),!.

finddepth(_,1).

%%kanoniki-20

permutation1([], []).
permutation1([X|Xs], Ys1) :- permutation1(Xs, Ys), select(X, Ys1, Ys).


even_permutation( [], [] ).
even_permutation( [X|T], Perm ) :-
    even_permutation( T, Perm1 ),
    insert_odd( X, Perm1, Perm ).
even_permutation( [X|T], Perm ) :-
    odd_permutation( T, Perm1 ),
    insert_even( X, Perm1, Perm ).

odd_permutation( [X|T], Perm ) :-
    odd_permutation( T, Perm1 ),
    insert_odd( X, Perm1, Perm ).
odd_permutation( [X|T], Perm ) :-
    even_permutation( T, Perm1 ),
    insert_even( X, Perm1, Perm ).

insert_odd( X, InList, [X|InList] ).
insert_odd( X, [Y,Z|InList], [Y,Z|OutList] ) :-
    insert_odd( X, InList, OutList ).

insert_even( X, [Y|InList], [Y,X|InList] ).
insert_even( X, [Y,Z|InList], [Y,Z|OutList] ) :-
    insert_even( X, InList, OutList ).