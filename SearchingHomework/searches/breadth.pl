% Breadth-First Search
% Shannon Pollard
% File breadth
% Based on L-S pp.236.

% Use running query as specified in the specific task file.
:- dynamic countNodes/1.
countNodes(0).

go(Start,Goal):-
	retract(countNodes(X)),  % counts the nodes visited
	asserta(countNodes(0)),
	empty_queue(Empty_open_queue),
	enqueue([Start,nil],Empty_open_queue,Open_queue),
	empty_set(Closed_set),
	path(Open_queue,Closed_set,Goal).

% case of empty open queue, so no path is found.

path(Open_queue,_,_):-
	empty_queue(Open_queue),
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('Graph searched, no solution found.').

% case of goal found on open queue, print solution
 
path(Open_queue,Closed_set,Goal):-
	dequeue([State,Parent],Open_queue,_),
	State = Goal,
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('Solution path is: '), nl,
	printsolution([State,Parent],Closed_set).

% general case - expand state from open queue, add children
% to open queue, add state to closed list.
	
path(Open_queue,Closed_set,Goal):-
	dequeue([State,Parent],Open_queue,Rest_open_queue),
	nl,
	write('Current State is: '),
	write(State),nl,
	write('Expanding new nodes.'),nl,
	get_children(State,Rest_open_queue,Closed_set,Children),
	add_list_to_queue(Children,Rest_open_queue,New_open_queue),
	union([[State,Parent]],Closed_set,New_closed_set),
	countNodes(X), Y is X+1, retract(countNodes(X)), 
	asserta(countNodes(Y)),
	path(New_open_queue,New_closed_set,Goal),!.

% General predicates used in this search

% Gets the children of State 

get_children(State,Rest_open_queue,Closed_set,Children):-
	(bagof(Child,moves(State,Rest_open_queue,Closed_set,Child),Children);Children = []).

% Finds all legal moves from State

moves(State,Rest_open_queue,Closed_set,[Next,State]):-
	move(State,Next),
	not(member_queue([Next,_],Rest_open_queue)),
	not(member_set([Next,_],Closed_set)).

printsolution([State,nil],_):-
	write(State),nl.
	
printsolution([State,Parent],Closed_set):-
	member_set([Parent,Grandparent],Closed_set),
	printsolution([Parent,Grandparent],Closed_set),
	write(State),nl.

member_set(E,S):- member(E,S).

empty_queue([]).

empty_set([]).
	
dequeue(E,[E|T],T).

enqueue(E,[],[E]).
enqueue(E,[H|T],[H|Tnew]):-
	enqueue(E,T,Tnew).

add_list_to_queue(List,Queue,Newqueue):-
	append(Queue,List,Newqueue).

member_queue(E,Q) :- member(E,Q).

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
