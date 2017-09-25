% A* algorithm
% Shannon Pollard
% File astar
% Based on L-S pages 237-238. 

% For running information, consult the file for the
% problem description.

:- dynamic countNodes/1.
countNodes(0).

go(Start,Goal) :-
	retract(countNodes(X)),		% global database counts
	asserta(countNodes(0)),		% # of nodes visited
	empty_set(Closed_set),
	empty_pq(Open),
	heuristic(Start,Goal,H),
	insert_pq([Start,nil,0,H,H],Open,Open_pq),
	path(Open_pq,Closed_set,Goal).

% case of no more nodes on open list - no solution.
 
path(Open,_,_):-
	empty_pq(Open),
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('Graph searched, no solution found.').

% case of goal found from open list - print solution
	
path(Open_pq,Closed_set,Goal):-
	dequeue_pq([State,Parent,_,_,_],Open_pq,_),
	State = Goal,
	countNodes(X),
	writelist([X,' nodes visited.']),
	write('The solution path is: '),nl,
	printsolution([State,Parent,_,_,_],Closed_set).

% general case - get node from priority queue and add its 
% childeren to the open list

path(Open_pq,Closed_set,Goal):-
	dequeue_pq([State,Parent,D,H,S],Open_pq,Rest_open_pq),
	retract(countNodes(X)),
	Y is X+1,		%increment number of nodes visited
	asserta(countNodes(Y)),
	nl,
	write('Current State is: '),
	write(State),nl,
	write('Expanding new nodes: '),nl,
	get_children([State,Parent,D,H,S],Rest_open_pq,Closed_set,Children,Goal),
	insert_list_pq(Children,Rest_open_pq,New_open_pq),
	add_if_not_in_set([State,Parent,D,H,S],Closed_set,New_closed_set),
	path(New_open_pq,New_closed_set,Goal),!.

% General functions needed for this search:
% Gets the children of the state that are not already on the
% open or closed lists

get_children([State,_,D,_,_],Rest_open_pq,Closed_set,Children,Goal):-
	(bagof(Child,moves([State,_,D,_,_],Rest_open_pq,Closed_set,Child,Goal),Children);Children = []).

%Finds legal moves from one state to next, and computes the heuristic % for each move.

moves([State,_,Depth,_,_],Rest_open_pq,Closed_set,[Next,State,New_D,H,S],Goal):-
	move(State,Next),
	not(member_pq([Next,_,_,_,_],Rest_open_pq)),
	not(member_set([Next,_,_,_,_],Closed_set)),
	New_D is Depth+1,
	heuristic(Next,Goal,H),
	S is New_D + H.

printsolution([State,nil,_,_,_],_):-
	write(State),nl.
	
printsolution([State,Parent,_,_,_],Closed_set):-
	member_set([Parent,Grandparent,_,_,_],Closed_set),
	printsolution([Parent,Grandparent,_,_,_],Closed_set),
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

precedes([_,_,_,_,S1],[_,_,_,_,S2]) :- S1 < S2.
