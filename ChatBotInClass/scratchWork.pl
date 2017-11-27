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
