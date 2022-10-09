add(X,Y,Z):-
    integer(Z),
    integer(X),
    integer(Y),
    Z is X+Y. 

add(X,Y,Z):-
    \+ integer(X),
    X is Z-Y.

add(X,Y,Z):-
    \+ integer(Y),
    Y is Z-X.
    
add(X,Y,Z):-
    \+ integer(Z),
    Z is X+Y.

 

    
    
    