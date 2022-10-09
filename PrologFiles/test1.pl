append([], B, B).

append([H|Ta], B, [H|Tc]) :- append(Ta, B, Tc).

reverse([], []).

reverse([X|Xs], Rev) :- reverse(Xs, Rev1), append(Rev1, X, Rev).

text(_, _, [], Acc, Answer) :- reverse(Acc, Answer).

text([X,Y,Z], NC, [X,Y,Z|Rest], Acc, Answer) :-
    append([NC], Acc, NAcc),
    text([X,Y,Z], NC, Rest, NAcc, Answer).

text([X,Y,Z], NC, [W|Rest], Acc, Answer) :-
    append([W], Acc, NAcc),
    text([X,Y,Z], NC, Rest, NAcc, Answer).

textify3(LtR, NC, Input, Output) :-
    text(LtR, NC, Input, [], Output).

textify1(_,_,[],Output,Output).

textify1([X,Y,Z],NC,[X,Y,Z|T],Output,A):-
    append(A,[NC],Rest),
    textify1([X,Y,Z],NC,T,Output,Rest),!.

textify1(L,NC,[H|T],Output,A):-
    append(A,[H],Rest),
    textify1(L,NC,T,Output,Rest),!.


finaltextify(L,NC,Input,Output):-
    textify1(L,NC,Input,Output,[]),!.

