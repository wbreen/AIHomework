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
process(Statement):- member('what', Statement), look_for_loc(Statement, Statement, Events), 
	member(E, Events), say(E), listen.
	
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
look_for_loc([],[],[]):- !.
look_for_loc([Word|Rest],[Time|Rest2], Events):- event(Title ,Word, Time), 
	look_for_loc(Rest, Rest2, OtherEvents),
	Events = [event(Title, Word, Time)|OtherEvents].
look_for_loc([_|Rest],[_|Rest2], Event):- look_for_loc(Rest,Rest2,Event).

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


%will need this to make the "what?" work, so you can lookup the event (still needs to be finished)
event_key('theater', 1, 'orientation').
event_key('theater', 2, 'movie').
event_key('theater', 4, 'magic show').
event_key('planetarium',1,'star show').

