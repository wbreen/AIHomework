% Depth-First Search with Closed List
% Shannon Pollard
% File depthclosed
% Based on L-S 234.

% To run, follow specifications in specific task file.
:- dynamic countNodes/1.
countNodes(0).


go(Start,Goal):-
	retract(countNodes(X)),
	asserta(countNodes(0)),
	empty_stack(Empty_open_stack),
	stack([Start,nil],Open_stack,Empty_open_stack),
	empty_set(Closed_set),
	path(Open_stack,Closed_set,Goal).

% case of empty open stack - no path found.

path(Open_stack,_,_):-
	empty_stack(Open_stack),
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('Graph searched, no solution found.').

% case of goal found on open stack, print path.

path(Open_stack,Closed_set,Goal):-
	stack([State,Parent],Open_stack,_),
	State = Goal,
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('Solution path is: '), nl,
	printsolution([State,Parent],Closed_set).

% general case - put the children of current state on open list,
% put state on closed list
	
path(Open_stack,Closed_set,Goal):-
	stack([State,Parent],Open_stack,Rest_open_stack),
	retract(countNodes(X)),		% update # nodes visited
	Y is X +1,
	asserta(countNodes(Y)),
	nl,
	write('Current state is: '),
	write(State),nl,
	write('Expanding New Nodes.'),nl,
	get_children(State,Rest_open_stack,Closed_set,Children),
	add_list_to_stack(Children,Rest_open_stack,New_open_stack),
	union([[State,Parent]],Closed_set,New_closed_set),
	path(New_open_stack,New_closed_set,Goal),!.

% General predicates and ADT specifications:

% Gets children of State

get_children(State,Rest_open_stack,Closed_set,Children):-
	(bagof(Child,moves(State,Rest_open_stack,Closed_set,Child),Children);Children = []).

% Finds all legal moves from State

moves(State,Rest_open_stack,Closed_set,[Next,State]):-
	move(State,Next),
	not(member_stack([Next,_],Rest_open_stack)),
	not(member_set([Next,_],Closed_set)).

printsolution([State,nil],_):-
	write(State),nl.
	
printsolution([State,Parent],Closed_set):-
	member_set([Parent,Grandparent],Closed_set),
	printsolution([Parent,Grandparent],Closed_set),
	write(State),nl.

empty_stack([]).

stack(Top,[Top|Stack],Stack).

member_stack(El,Stack):- member(El,Stack).

empty_set([]).

member_set(E,S):- member(E,S).

add_list_to_stack(List,Stack,NewStack):-
	append(List,Stack,NewStack).

union([],S,S).
union([H|T],S,S_new):-
	union(T,S,S2),
	add_if_not_in_set(H,S2,S_new).


add_if_not_in_set(X,S,S) :- member(X,S),!.

add_if_not_in_set(X,S,[X|S]).

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

member(X,[X|_]).
member(Element,[_|Tail]) :- member(Element,Tail).

writelist([]).
writelist([H|T]):- write(H),  writelist(T).
