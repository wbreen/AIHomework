% 9/11 class work, Farmer, Wolf, Goat, Cabbage Problem
% state(F, W, G, C) each of F, W, G, C are either l for left or
% r for Right

opp(l, r).
opp(r, l).

% need an unsafe state, which you cannot have certain things on
% the same side together alone
unsafe(state([F,W,G,_])):- W=G, opp(W,F).
unsafe(state([F,_,G,C])):- G=C, opp(F,G).

% Farmer goes alone
% P is the opposite of F, for Farmer
move(state([F, W, G, C]), state([P, W, G, C])):- opp(F,P).
% Farmer takes the Wolf
move(state([F, F, G, C]), state([P, P, G, C])):- opp(F,P).
% Farmer takes the Goat
move(state([F, W, F, C]), state([P, W, P, C])):- opp(F,P).
% Farmer takes the Cabbage
move(state([F, W, G, F]), state([P, W, G, P])):- opp(F,P).

%Create path so you know what moves Prolog took to solve the puzzle
%	Base Case
path(Start, Start, _,[Start]).
%	Recursion
% You want the start and end state, the path taken, and if you have been in a
% 	certain state before (so you know not to go back to states you've already been
% try moving something, if its not been tried and isn't unsafe, then move and
% recurse on that list
path(Start, End, BeenHere, Path):-
    move(Start, Somewhere),
    not(member(Somewhere,BeenHere)),
    not(unsafe(Somewhere)),
    path(Somewhere,End,[Somewhere|BeenHere],RestOfPath),
    Path = [Start|RestOfPath].
    


%Making the puzzle, so Prolog knows what to solve for
%  Go from the first state to the second state, and return the Path as a list
puzzle:- path(state([l,l,l,l]), state([r,r,r,r]),[state([l,l,l,l])], Path),
    write(Path).