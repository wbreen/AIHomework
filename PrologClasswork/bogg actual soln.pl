truthTeller(red, south).
truthTeller(green, north).
liar(red, north).
liar(green, south).

claim(Color, Region):- Color=red; Region=north.

solve(Color, Region):-
    truthTeller(Color, Region),
    claim(Color, Region).

solve(Color, Region):-
    liar(Color, Region),
    not(claim(Color, Region)).

run_bogg:- solve(Color, Region), write('Color is '), 
    write(Color), nl, write('Region is '), write(Region).