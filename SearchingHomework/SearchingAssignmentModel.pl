:- set_prolog_stack(global, limit(1000000000000)).
% State representation:
% [X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3]
% Or [A,B,C,D,E,F,G,H,I] And Em represents the empty square
% Each can have the value of 0..8, 0 being the empty square, and each other number representing the square and where it is supposed to be

% These swaps are the possible values for each variable
swap(0,1).
swap(1,0).
swap(0,2).
swap(2,0).
swap(0,3).
swap(3,0).
swap(0,4).
swap(4,0).
swap(0,5).
swap(5,0).
swap(0,6).
swap(6,0).
swap(0,7).
swap(7,0).
swap(0,8).
swap(8,0).
swap(0,9).
swap(9,0).




% Each of these represents the available moves when each is the empty square
% Sometimes, one swap is represented as another, and if one swap between squares has already been represented, I just skip it
%Move where X1 is the empty square
move([Em,B,C,D,E,F,G,H,I], [B,Em,C,D,E,F,G,H,I]):- swap(Em,B).
move([Em,B,C,D,E,F,G,H,I], [D,B,C,Em,E,F,G,H,I]):- swap(Em,D).

%Move where X2 is the empty square
%move([A,Em,C,D,E,F,G,H,I],[Em,A,C,D,E,F,G,H,I]):- swap(Em,A).
move([A,Em,C,D,E,F,G,H,I],[A,C,Em,D,E,F,G,H,I]):- swap(Em,C).
move([A,Em,C,D,E,F,G,H,I],[A,E,C,D,Em,F,G,H,I]):- swap(Em,E).

%Move where X3 is the empty square
%move([A,B,Em,D,E,F,G,H,I],[A,Em,B,D,E,F,G,H,I]):- swap(Em,B).
move([A,B,Em,D,E,F,G,H,I],[A,B,F,D,E,Em,G,H,I]):- swap(Em,F).

%Move where Y1 is the empty square
move([A,B,C,Em,E,F,G,H,I],[A,B,C,E,Em,F,G,H,I]):- swap(Em,E).
move([A,B,C,Em,E,F,G,H,I],[A,B,C,G,E,F,Em,H,I]):- swap(Em,G).

%Move where Y2 is the empty square
move([A,B,C,D,Em,F,G,H,I],[A,B,C,D,F,Em,G,H,I]):- swap(Em,F).
move([A,B,C,D,Em,F,G,H,I],[A,B,C,D,H,F,G,Em,I]):- swap(Em,H).

%Move where Y3 is the empty square
move([A,B,C,D,E,Em,G,H,I],[A,B,C,D,E,I,G,H,Em]):- swap(Em,I).

%Move where Z1 is the empty square
move([A,B,C,D,E,F,Em,H,I],[A,B,C,D,E,F,H,Em,I]):- swap(Em,H).

%Move where Z2 is the empty square
move([A,B,C,D,E,F,G,Em,I],[A,B,C,D,E,F,G,I,Em]):- swap(Em,I).

%Move where Z3 is the empty square
%move([A,B,C,D,E,F,G,H,I],[A,B,C,D,E,F,G,H,I]):- swap(Em,_).



% Hamming Distance version of Heuristic:
% number of tiles that are out of place
heuristic(S,G,Heur):- S==G, Heur is 0.
heuristic([SH|ST],[GH|GT], Heur):- SH \= GH,
    heuristic(ST, GT, PrevHeur),
    Heur is PrevHeur + 1.
%heuristic(S,G,Heur):- S\=G, Heur is 1.
heuristic([SH|ST], [GH|GT], Heur):- SH==GH,
    heuristic(ST, GT, PrevHeur),
    Heur is PrevHeur.


