% Iterative Deepening Search
% Shannon Pollard
% File iterative


% To run, follow specifications in specific task file.
:- dynamic countNodes/1.
countNodes(0).

go(Start, Goal):-
	retract(countNodes(X)),
	asserta(countNodes(0)),
	depth_limited(Start, Goal, 1).



depth_limited(Start,Goal, MaxDepth):-
	empty_stack(Empty_open_stack),
	stack([Start,nil,1],Open_stack,Empty_open_stack),
	empty_set(Closed_set),
	path(Open_stack,Closed_set,Start,Goal, MaxDepth).


% case of empty open stack - no path found.

path(Open_stack,_,Start,Goal,MaxDepth):-
	empty_stack(Open_stack),
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('At '), write(MaxDepth),
	write(' Graph searched, no solution found.'),nl,
	NewMax is MaxDepth + 1,!,
	depth_limited(Start, Goal,NewMax).
	

% case of goal found on open stack, print path.

path(Open_stack,Closed_set,_,Goal, _):-
	
	stack([State,Parent, Depth],Open_stack,_),
	State = Goal,
	countNodes(X),
	writelist([X,' nodes visited.']), nl,
	write('Goal found at depth '), write(Depth),nl,
	write('Solution path is: '), nl,
	printsolution([State,Parent,Depth],Closed_set).


% general case - put the children of current state on open list,
% put state on closed list
	
path(Open_stack,Closed_set,Start,Goal, MaxDepth):-
	stack([State,Parent, Depth],Open_stack,Rest_open_stack),
	retract(countNodes(X)),		% update # nodes visited
	Y is X +1,
	asserta(countNodes(Y)),
	nl,
	write('Current state is: '),
	write(State),nl,
	write('Expanding New Nodes.'),nl,
	get_children(State,Depth,Rest_open_stack,Closed_set,Children,MaxDepth),
	add_list_to_stack(Children,Rest_open_stack,New_open_stack),
	union([[State,Parent,Depth]],Closed_set,New_closed_set),
	path(New_open_stack,New_closed_set,Start,Goal, MaxDepth),!.

% General predicates and ADT specifications:

% Gets children of State

get_children(_,StateDepth, _,_,Children, MaxDepth):- MaxDepth = StateDepth, Children = [].

get_children(State,StateDepth, Rest_open_stack,Closed_set,Children, MaxDepth):- MaxDepth > StateDepth, 
	(bagof(Child,moves(State,StateDepth,Rest_open_stack,Closed_set,Child),Children);Children = []).

% Finds all legal moves from State

moves(State,StateDepth,Rest_open_stack,Closed_set,[Next,State,NextDepth]):-
	move(State,Next),
	NextDepth is StateDepth + 1,
	not(member_stack([Next,_,NextDepth],Rest_open_stack)),
	not(member_set([Next,_,NextDepth],Closed_set)).

printsolution([State,nil,1],_):-
	write(State),nl.
	
printsolution([State,Parent, Depth],Closed_set):-
	ParentDepth is Depth -1,
	member_set([Parent,Grandparent,ParentDepth],Closed_set),
	printsolution([Parent,Grandparent, ParentDepth],Closed_set),
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
