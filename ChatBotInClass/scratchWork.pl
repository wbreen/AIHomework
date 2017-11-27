% generate a list of all events whose time is in the statement
look_for_time([],[]):- !.
look_for_time([Hour| Rest], Events):- time_key(Hour, Event),
	event(Event, Place, Hour),
	look_for_time(Rest, OtherEvents),
	Events = [event(Event, Place, Hour) | OtherEvents].
look_for_time([_|Rest], Event):- look_for_time(Rest, Event).


% Generate list of all events whose location is in the statement
%this was the base, the final version has been edited in the main file
look_for_loc([],[]):- !.
look_for_loc([Place| Rest], Locations):- loc_key(Place, Event),
	event(Event, Place, Hour),
	look_for_loc(Rest, OtherEvents),
	Locations = [event(Event, Place, Hour) | OtherEvents].

look_for_loc([_|Rest], Event):- look_for_loc(Rest, Event).


%get time and location, and if both are members of the event, then say that event
event('orientation', 'theater', 1).
title_key('reading', 'reading time'). 
loc_key('theater', 'orientation').
time_key(1, 'orientation').



%reference for looking for events in the prolog editor
%What is happening at the theatre at 1? 
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