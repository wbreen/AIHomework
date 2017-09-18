% Which cities are connected to one another in the graph.
connected(a,b).
connected(a,c).
connected(b,c).
connected(b,e).
connected(c,e).
connected(d,e).
connectedTo(X,Y):- connected(X,Y).
connectedTo(X,Y):- connected(Y,X).

% index(+Element, +List, -Index) is true when Element is the Index-th
% element of the List.  Note that the index is the output.
index(Element, [Element|_], 1):-!.
index(Element, [H|T], I):-Element \= H, index(Element, T, TIndex), 
    I is TIndex + 1.

% getElement(-Element, +List, +Index) is true when Element is the Index-th
% element of the List.  Here the output is the Element.  A better version (paid)
% of Prolog lets me do both at once.  But here, subtraction is monotonic (goes one 
% way only).
getElement(Element, [Element|_],1):-!.
getElement(Element, [_|T], X):- X1 is X-1, getElement(Element,T,X1).
    
% The way I should've done it in class.
salesman(L):-
    % each variable represents a city.
    % Values are 1-5.
    L = [A,B,C,D,E],
    permutation(L,[1,2,3,4,5]),
    (B = 2; B = 3),
    EtoA is E-A,
    EtoD is E-D,
    (EtoA > 0; EtoD >0),
    % weird way to do the graph.  Check the 
    % cities that have consecutive numbers
    % and make sure they also connect
    % in the graph.
    index(1,L,OneIndex),
    index(2,L,TwoIndex),
    index(3,L,ThreeIndex),
    index(4,L,FourIndex),
    index(5,L,FiveIndex),
    Cities = [a,b,c,d,e],
    getElement(City1, Cities, OneIndex),
    getElement(City2, Cities, TwoIndex),
    getElement(City3, Cities, ThreeIndex),
    getElement(City4, Cities, FourIndex),
    getElement(City5, Cities, FiveIndex),
    connectedTo(City1, City2),
    connectedTo(City2, City3),
    connectedTo(City3, City4),
    connectedTo(City4, City5).

% This time I'm going to solve it in a more
% straightforward way.  Sometimes our first idea
% for knowledge representation isn't the best idea.
salesman2(L):-
    % This time the variables are the 1st - 5th places 
    % in the visit list.
    L = [A,B,C,D,E],
    % and the domain is the cities.
    permutation(L, [a,b,c,d,e]),
	(B = b; C = b),
    % make sure e comes before either a or d
	index(e,L,EIndex),
	index(a,L,AIndex),
	index(d,L,DIndex),
	(EIndex < AIndex; EIndex < DIndex),
    % make sure consecutive cities are connected in the graph.
	connectedTo(A,B),
	connectedTo(B,C),
	connectedTo(C,D),
	connectedTo(D,E).