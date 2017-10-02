% Iterative Deepening A* Search
% Shannon Pollard
% File iterastar


% To run, follow specifications in specific task file.
:- dynamic countNodes/1.
countNodes(0).

go(Start, Goal):-
	retract(countNodes(X)),
	asserta(countNodes(0)),
	heuristic(Start,Goal,H),
	astar_limited(Start, Goal, H).



astar_limited(Start,Goal, MaxF):-
	empty_pq(Open),
	empty_set(Closed_set),
	heuristic(Start,Goal,H),
	insert_pq([Start,nil,0,H,H,1],Open,Open_pq),
	max_heuristic(Max),
	path(Open_pq,Closed_set,Start,Goal, MaxF, Max).


% case of empty open stack - no path found.

path(Open,_,Start,Goal,MaxF,NewMax):-
	empty_pq(Open),
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('At heuristic max : '), write(MaxF),
	write(' Graph searched, no solution found.'),nl,!,
	astar_limited(Start, Goal,NewMax).
	

% case of goal found on open stack, print path.

path(Open_pq,Closed_set,_,Goal, _,_):-
	
	dequeue_pq([State,Parent, _,_,_,Depth],Open_pq,_),
	State = Goal,
	countNodes(X),
	writelist([X,' nodes visited.']), nl,
	write('Goal found at depth '), write(Depth),nl,
	write('Solution path is: '), nl,
	printsolution([State,Parent,_,_,_,Depth],Closed_set).


% general case - put the children of current state on open list,
% put state on closed list
	
path(Open_pq,Closed_set,Start,Goal, MaxF,NewMax):-
	dequeue_pq([State,Parent, D, H, S, Depth], Open_pq,Rest_open_pq),
	retract(countNodes(X)),		% update # nodes visited
	Y is X +1,
	asserta(countNodes(Y)),
	nl,
	write('Current state is: '),
	write(State),nl,
	write('Expanding New Nodes.'),nl,
	get_children([State, Parent, D, H, S, Depth],Rest_open_pq,Closed_set,Children,Goal,MaxF,MinExceeding),
	insert_list_pq(Children,Rest_open_pq,New_open_pq),
	add_if_not_in_set([State,Parent,D,H,S,Depth],Closed_set,New_closed_set),
	((MinExceeding < NewMax, NextMax = MinExceeding); NextMax = NewMax),
	path(New_open_pq,New_closed_set,Start,Goal,MaxF,NextMax),!.


% General predicates and ADT specifications:

% Gets children of State

get_children([_,_,_,_,F,_],_,_,Children,_,MaxF,F):-F > MaxF , Children = [].

get_children([State,_,D,_,F,StateDepth],Rest_open_pq,Closed_set,Children,Goal,MaxF,MinExceeding):-
   MaxF >= F,
	(bagof(Child,moves([State,_,D,_,F,StateDepth],Rest_open_pq,Closed_set,Child,Goal),Children);Children = []),
	findMinExceeding(Children,MaxF,MinExceeding).

findMinExceeding([],_,Max):-max_heuristic(Max).
findMinExceeding([H|T],MaxF,Max):-H = [_,_,_,_,Heur,_], Heur > MaxF, !, findMinExceeding(T,MaxF,TailMax), 
		((TailMax<Heur, Max = TailMax); Max = Heur).
findMinExceeding([H|T],MaxF,Max):-H = [_,_,_,_,Heur,_], Heur =< MaxF, !, findMinExceeding(T,MaxF,Max). 	

% Finds all legal moves from State

moves([State,_,Depth,_,_,StateDepth],Rest_open_pq,Closed_set,[Next,State,New_D,H,S,NextDepth],Goal):-
	move(State,Next),
	NextDepth is StateDepth + 1,
	not(member_pq([Next,_,_,_,_,NextDepth],Rest_open_pq)),
	not(member_set([Next,_,_,_,_,NextDepth],Closed_set)),
	New_D is Depth+1,
	heuristic(Next,Goal,H),
	S is New_D + H.

printsolution([State,nil,_,_,_,_],_):-
	write(State),nl.
	
printsolution([State,Parent,_,_,_,_],Closed_set):-
	member_set([Parent,Grandparent,_,_,_,_],Closed_set),
	printsolution([Parent,Grandparent,_,_,_,_],Closed_set),
	write(State),nl.

insert_pq(State,[],[State]).
insert_pq(State,[H|Tail],[State,H|Tail]):-
	precedes(State,H).
insert_pq(State,[H|T],[H|Tnew]):-
	insert_pq(State,T,Tnew).

insert_list_pq([],L,L).
insert_list_pq([State|Tail],L,New_L):-
	insert_pq(State,L,L2),
	insert_list_pq(Tail,L2,New_L).
	
print_pq([]):-nl.
print_pq([H|T]):- write(H), write(','), print_pq(T).

member_set(E,S):- member(E,S).

member(X,[X|_]).
member(Element,[_|Tail]) :- member(Element,Tail).

add_if_not_in_set(X,S,S) :- member(X,S),!.

add_if_not_in_set(X,S,[X|S]).

member_pq(E,Q) :- member(E,Q).

dequeue_pq(E,[E|T],T).

empty_pq([]).

empty_set([]).

writelist([]).
writelist([H|T]):- write(H),  writelist(T).

precedes([_,_,_,_,S1,_],[_,_,_,_,S2,_]) :- S1 < S2.

max_heuristic(99999).
