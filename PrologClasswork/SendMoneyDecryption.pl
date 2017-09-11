% clpfd speeds up constraint satisfaction in prolog
:-use_module(library(clpfd)).

%send more money decryption using clpfd
%	to use union/intersection use the slashes \/ for Union and /\ for intersection
puzzle2([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]):-
    Vars = [S,E,N,D,M,O,R,Y],
    Vars ins 0..9,
    all_different(Vars),
    S #\= 0, M #\= 0,
    S*1000 + E*100 + N*10 + D +
    M*1000 + O*100 + R*10 + E #=
    M*10000 + O*1000 + N*100 + E*10 + Y,
    label(Vars).
    


% Solve "SEND + MORE = MONEY" decryption puzzle (original solve)
puzzle([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]):-
    Vars = [S,E,N,D,M,O,R,Y],
    allDigits(Vars),
    allDifferent(Vars),
    S\=0, M\=0,
    FirstSum is S*1000 + E*100 + N*10 + D,
    SecondSum is M*1000 + O*100 + R*10 + E,
    ThirdSum is M*10000 + O*1000 + N*100 + E*10 + Y,
    Sum is FirstSum + SecondSum,
    ThirdSum = Sum.

allDigits([]).
allDigits([H|T]):-
    member(H,[0,1,2,3,4,5,6,7,8,9]),
    allDigits(T).

allDifferent([]).
allDifferent([_]).
allDifferent([H|T]):- 
    not(member(H,T)),
    allDifferent(T).
    