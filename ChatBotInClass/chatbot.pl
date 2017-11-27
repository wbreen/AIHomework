%Top-level query.

chat:- write('Welcome to the Kids Museum.  What can I help you with?'),nl,listen.

%Lets the user type.  Statement is a list of lower case words, with 
% punctuation as a separate element in the list.

listen:-nl, read_in(Statement), process(Statement).

% Deal with the statement by responding.  Unless you want the conversation
% to stop, end each line with a call to 'listen'.
 
process(Statement):- greeting(Statement), write('Hi! How can I help?'), listen.

% If you say bye we stop the process.
process(Statement):- bye(Statement), write('Bye!').

% When?
process(Statement):- member('when', Statement), look_for_event(Statement, Events), 
	member(E, Events), say(E), listen.

%If you say thanks, say youre welcome 
process(Statement):- thanks(Statement), write('You are welcome!'), listen.

%Where? questions
process(Statement):- member('where', Statement), look_for_event(Statement, Events),
	member(E, Events), say(E), listen.

%What? questions (what is happening in the theatre at 3?)
process(Statement):- member('what', Statement),
	look_for_time(Statement, Times), look_for_loc(Statement, Locs),
	member(E, Times), member(E, Locs),
	say(E), listen.


% Catch all.  What is said if none of the previous rules fire.
process(_):- write('im not sure what to do with that.  can you try again?'), listen.
	
% Say a statement giving information about the given event.
say(event(Thing, Place, Time)):-	write('The '), write(Thing), 
	write(' is in the '), write(Place), write(' at '), write(Time), write('.').

hello_words(['hello', 'hi', 'howdy', 'hey', 'hola']).
bye_words(['bye', 'goodbye']).
when_words(['when']).
where_words(['where']).
what_words(['what']).
thanks_words(['thanks', 'thank', 'thankee']).

greeting(Statement):- hello_words(Hello), member(Word, Statement), member(Word, Hello).
bye(Statement):- bye_words(Bye), member(Word, Statement), member(Word, Bye).
thanks(Statement):- thanks_words(Thanks), member(Word, Statement), member(Word, Thanks).

% Generate a list of all events whose key word is in the sentence.
look_for_event([], []):- !.

look_for_event([Word| Rest], Events):- title_key(Word, Title),
	event(Title, Place, Time), 
	look_for_event(Rest, OtherEvents), 
	Events = [event(Title, Place, Time) | OtherEvents].
	
look_for_event([_|Rest], Event):- look_for_event(Rest, Event).

%This doesnt work, need to actually write out the database and rework this
%This is what I am working on to make "what" questions work
%What is happening at the theatre at 1? 

% Generate list of all events whose location is in the statement
look_for_loc([],[]):- !.
look_for_loc([Place| Rest], Locations):- loc_full_key(Place, FullP),
	loc_key(FullP, Event),
	event(Event, FullP, Hour),
	look_for_loc(Rest, OtherEvents),
	Locations = [event(Event, FullP, Hour) | OtherEvents].

look_for_loc([_|Rest], Locations):- look_for_loc(Rest, Locations).

% generate a list of all events whose time is in the statement
look_for_time([],[]):- !.
look_for_time([Hour| Rest], Events):- time_key(Hour, Thing),
	event(Thing, Place, Hour),
	look_for_time(Rest, OtherEvents),
	Events = [event(Thing, Place, Hour) | OtherEvents].
look_for_time([_|Rest], Events):- look_for_time(Rest, Events).





%Database of Events.  They are listed as: event title, place, time.
event('orientation', 'theater', 1).
event('movie', 'theater', 2).
event('magic show', 'theater', 4).
event('star show', 'planetarium', 3).
event('laser show', 'planetarium', 1).
event('color demonstration', 'lobby', 11).
event('physics talk', 'lobby', 1).
event('flag raising', 'lobby', 3).
event('sale', 'gift shop', 11).
event('giveaway', 'gift shop', 12).
event('story time', 'book nook', 11).
event('play', 'book nook', 12).
event('reading time', 'book nook', 1). 

% This gives the keyword of each event title.

title_key('orientation', 'orientation').
title_key('movie', 'movie').
title_key('magic', 'magic show').
title_key('star', 'star show').
title_key('laser', 'laser show').
title_key('color', 'color demonstration').
title_key('physics', 'physics talk').
title_key('flag', 'flag raising').
title_key('sale', 'sale').
title_key('giveaway', 'giveaway').
title_key('story', 'story time').
title_key('play', 'play').
title_key('reading', 'reading time'). 


%Location from shortened to full
loc_full_key('theater','theater').
loc_full_key('planetarium','planetarium').
loc_full_key('lobby','lobby').
loc_full_key('gift','gift shop').
loc_full_key('book','book nook').


%will need this to make the "what?" work, so you can lookup the event (still needs to be finished)
% Locations associated with events
loc_key('theater', 'orientation').
loc_key('theater', 'movie').
loc_key('theater', 'magic show').
loc_key('planetarium', 'star show').
loc_key('planetarium', 'laser show').
loc_key('lobby', 'color demonstration').
loc_key('lobby', 'physics talk').
loc_key('lobby', 'flag raising').
loc_key('gift shop', 'sale').
loc_key('gift shop', 'giveaway').
loc_key('book nook', 'story time').
loc_key('book nook', 'play').
loc_key('book nook', 'reading time').


%Times associated with events
time_key(1, 'orientation').
time_key(1, 'laser show').
time_key(1, 'physics talk').
time_key(1, 'reading time').
time_key(2, 'movie').
time_key(3, 'star show').
time_key(3, 'flag raising').
time_key(4, 'magic show').
time_key(11, 'color demonstration').
time_key(11, 'story time').
time_key(12, 'giveaway').
time_key(12, 'play').

























