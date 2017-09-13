:-use_module(library(clpfd)).
% Traveling salesman basic problem 

salesman(L):-
    L = [A,B,C,D,E],
    L ins 1..5,
    all_different(L),
    (A-B #= 1; A-B #= -1),
    (A-C #= 1; A-B #= -1),
    (B-C #= 1; B-C #= -1),
    (E-B #= 1; E-B #= -1),
    (E-C #= 1; E-C #= -1),
    (E-D #= 1; E-D #= -1),
    B in 2..3,
    (E-D #> 0; E-A #> 0),
    label(L).