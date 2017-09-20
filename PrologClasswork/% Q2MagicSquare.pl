% Q2
:- use_module(library(clpfd)).

magicSquare([X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3],
            [X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],
            [X1,Y2,Z3],[X3,Y2,Z1],Ans):-
    Vars = [X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3],
    Vars ins 0..8,
    Ans in 1..25,
    X1#=3,
    Y1#=8,
    Z2#=6,
    Z3#=5,
    Y3#=0,
    all_different(Vars),
    X1+X2+X3#=Ans,
    Y1+Y2+Y3#=Ans,
    Z1+Z2+Z3#=Ans,
    X1+Y1+Z1#=Ans,
    X2+Y2+Z2#=Ans,
    X3+Y3+Z3#=Ans,
    X1+Y2+Z3#=Ans,
    X3+Y2+Z1#=Ans.