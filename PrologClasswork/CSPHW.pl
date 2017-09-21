% Constraint Satisfaction Problems Homework

% Q1 Evens and Odds
% All even numbers come before all odd numbers, no assumptions (can be empty/hold only odd nums)
% evensFirst([+List]) :- yes/no?
evensFirst([]).
evensFirst([H|T]):- isEven(H),
    evensFirst(T).
evensFirst([H|T]):- isOdd(H),
    listOdd(T).

% Helper predicates: 
% Checks if the number is even, returns false if it is odd
% isNumEven? (+Num, -isEven)
isEven(N):- 0 is mod(N,2).
isOdd(N):- 1 is mod(N,2).
% return if all the numbers in the list are odd
listOdd([]).
listOdd([H|T]):- listOdd(T),
    isOdd(H).

% Q2 Magic Square
% The sum of each row, column and diagonal are all equal
% Q2
% :- use_module(library(clpfd)).

%magicSquare([X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3],
%           [X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],
%            [X1,Y2,Z3],[X3,Y2,Z1],Ans):-
%    Vars = [X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3],
%    Vars ins 0..8,
%    Ans in 1..25,
%    X1#=3,
%    Y1#=8,
%    Z2#=6,
%    Z3#=5,
%    Y3#=0,
%    all_different(Vars),
%    X1+X2+X3#=Ans,
%    Y1+Y2+Y3#=Ans,
%    Z1+Z2+Z3#=Ans,
%    X1+Y1+Z1#=Ans,
%    X2+Y2+Z2#=Ans,
%    X3+Y3+Z3#=Ans,
%    X1+Y2+Z3#=Ans,
%    X3+Y2+Z1#=Ans.


% Figure out how the people were dressed
couplesDressed(Wearing):- 
    Wearing = [[matt,MattWife,MattCos,MWCos,MattArr],
                [vince,VinceWife,VinceCos,VWCos,VinceArr],
                [chuck,ChuckWife,ChuckCos,CWCos,ChuckArr],
                [lou,LouWife,LouCos,LWCos,LouArr]],
    permutation([sue,mary,ann,tess],[MattWife,VinceWife,ChuckWife,LouWife]),
    permutation([bear,prince,donald_duck,batman],[MattCos,VinceCos,ChuckCos,LouCos]),
    permutation([cat,snow_white,gipsy,witch],[MWCos,VWCos,CWCos,LWCos]),
    permutation([first,second,third,fourth],[MattArr,VinceArr,ChuckArr,LouArr]),
    % Matt with cat
    member([matt,MattWife,MattCos,cat,_],Wearing),
    % cat was there third
    member([_,_,_,cat,third],Wearing),
    % Matt is not bear
    MattCos \= bear,
    % bear is first or second there
    member([_,_,bear,_,BearArr],Wearing), BearArr\=third, BearArr\=fourth,
    % Vince was not there first or last
    VinceArr\= first, VinceArr\=fourth,
    % prince did not get there first or second
    member([_,_,prince,_,PrinceArr],Wearing), PrinceArr\=first, PrinceArr\=second,
    % Vince is not prince
    VinceCos\=prince,
    % witch is not Sue
    member([_,sue,_,SueCos,_],Wearing), SueCos\=witch,
    % witch is married to Chuck
    member([chuck,_,_,witch,_],Wearing),
    % Chuck is donald_duck
    member([chuck,_,donald_duck,_,_],Wearing),
    % Mary is not there first or last
    member([_,mary,_,_,MaryArr], Wearing), MaryArr\=first, MaryArr\=fourth,
    % Lou is not last or second to last
    LouArr\=fourth, LouArr\=third,
    % Sue is not first or second
    member([_,sue,_,_,SueArr],Wearing), SueArr\=first, SueArr\=second,
    % Mary not married to Lou
    member([MaryHus,mary,_,_,_],Wearing), MaryHus\=lou,
    % Sue not married to Lou
    member([SueHus,sue,_,_,_],Wearing), SueHus\=lou,
    % Ann is not gipsy
    member([_,ann,_,AnnCos,_],Wearing), AnnCos\=gipsy,
    % gipsy is not married to batman
    member([_,_,batman,BatWifeCos,_],Wearing), BatWifeCos\=gipsy,
    % Ann is not married to batman
    member([_,ann,AnnHusCos,_,_],Wearing), AnnHusCos\=batman,
    % snow_white is not first
    member([_,_,_,snow_white,SnowArr],Wearing), SnowArr\=first,
    % Tess is not fourth or snow white
    member([_,tess,_,TessCos,TessArr],Wearing), TessCos\=snow_white, TessArr\=fourth
    .

% Q4
% Define Variables, Domains, and Constraints for the given grid puzzle
% 
% Variables = {X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3}
% 	The variables are the squares of the puzzle, as each one needs a different value
% 	Vertically: X is first line, Y is second, Z is third
% 	
% Domain = {WT,WO,WX, GT,GO,GX, YT,YO,YX, BT,BO,BX}
% 	In the domain, the first letter stands for the color (W=white, G=Green, Y=Yellow, B=Blue)
% 	The second letter stands for the shape (T=Triangle, O=Circle, X=X)
% 	
% Constraints:
% 	(described from top to bottom, starting with the first column)
% 	('_' indicate some unconstrained value)
% 	Positive constraints (must be in): 
% 		1. Col 1: B_, _X, _T
% 		   Col 2: G_, B_, Y_
% 		2. Col 1: G_, _O
% 		   Col 2: __, Y_
% 		   Col 3: _X, _T
% 	
% 	Negative Constraints (must not be in):
% 		1. Col 1: __, Y_
% 		   Col 2: B_, __
% 		   Col 3: __, __
% 		2. Col 1: __, __, __
% 		   Col 2: _X, _X, __





% Q5
% Mr. Bogg from Og
% Natives can be either red or green
% Can come from north or south
% North: red lie, green truth
% South: red truth, green lie
% 
% Semicolon means 'or' and comma means 'and'
% 
% Bogg says "I'm red or I'm from the south"
% ANSWER IS: He is red and from the south
% Tell the color and region of Mr. Bogg
% 
%tellColorReg(Says):- Says =[North,South,Red,Green,Truth],
%    (   (North=north, Red=red)==> Truth=),(   North=north, Green=green),
%    (South=south, Red=lie, Green=truth).

tellColor2(Says, AColor, ARegion):- Says=[Color, Region],
    (   (   Region=north, Color=green), AColor=green, ARegion=north ); % tells truth
    (   (   Region=north, Color=red), (   AColor\=red; ARegion=north ); % tells lie
    (   (   Region=south, Color=green), AColor\=green; ARegion=south); % tells lie
    (   (   Region=south, Color=red), AColor=red, ARegion=south). % tells truth

tellColor3(Says, From):- 
    Says=[Color, Region],
    From=[FColor, FRegion],
    (   Color=red, FRegion=north, FColor=red);
    (   Color=green, FRegion=south, FColor=green);
    (   Region=south, (   FColor=red, (   FRegion=south; FRegion=north)));
    (   Region=north, (   FColor=green,(   FRegion=north; FRegion=south))).

% Determine if the guy is a liar
isLiar(Says):-
    Says=[Color, Region],
    (   Color = red, Region = south),
    (   Color = green, Region = north).

%Misc stuff from prolog
%Says=[Color, Region],
%    Color = [red,green],
%    Region = [north, south],
%    From=[FColor, FRegion],
%    FColor = [red, green],
%    FRegion = [north, south],
%              
%    isLiar(Says, From),
%    Says \= From.

%   :- not(isLiar(Says,From)), From = Says.


%    (   Color=red, FRegion=north, FColor=red);
%    (   Color=green, FRegion=south, FColor=green);
%   (   Region=south, (   FColor=red, (   FRegion=south; FRegion=north)));
%    (   Region=north, (   FColor=green,(   FRegion=north; FRegion=south))).



% More experimentation with how to tell if the guy is a liar
% Says "Im red or Im from the South
% Determine if the guy is a liar
isLiar(Color, From):-
    Color.
isLiar([green,south]).

%Misc stuff from prolog
%Says=[Color, Region],
%    Color = [red,green],
%    Region = [north, south],
%    From=[FColor, FRegion],
%    FColor = [red, green],
%    FRegion = [north, south],
%              
%    isLiar(Says, From),
%    Says \= From.

colorReg(Says, Ans):-
    Says = [SaysColor, SaysRegion],
    Ans = [Color, Region],
    Color = red; Color=green,
    Region=north; Region=south,
%    (Color = red; Color = green),
%    (Region = north; Region = south),
    isLiar(Says),
    member([SaysColor,SaysRegion], Ans),
    SaysColor\=Color, SaysRegion\=Region,
    
    not(isLiar(Says)),
    member([SaysColor,SaysRegion], Ans),
    SaysColor=Color, SaysRegion=Region
    .


% Question 7, Harry Potter puzzle
% 
escapeRoom(Bottles):-
    Bottles=[[1, FirstSize, FirstHolds],
              [2,SecondSize, SecondHolds],
              [3,ThirdSize, ThirdHolds],
              [4,FourthSize, FourthHolds],
              [5, FifthSize, FifthHolds],
              [6, SixthSize, SixthHolds],
              [7,SeventhSize, SeventhHolds]],
        permutation([poison,poison,poison,wine,wine,forward,backward],
                    [FirstHolds,SecondHolds,ThirdHolds,FourthHolds,FifthHolds,SixthHolds,SeventhHolds]),
        permutation([giant7,tall6,tall5,tall4,tall3,tall2,dwarf1],
                    [FirstSize,SecondSize,ThirdSize,FourthSize,FifthSize,SixthSize,SeventhSize]),
        % poison is to the left of nettlewine
        member([
        % first is not the same as last
        % first and last will not help you move forward
        % The tallest isn't poison
        % The shortest isn't poison
        % The second and the sixth are the same thing




start(X):- X = [Dad,Mom,Matt,John,Tim],
	((Dad = y, Mom = y); Dad = n),
	(Matt = y; John = y),
	(Mom = y; Tim = y),
	(Mom = n; Tim = n),
	Tim = John,
	(Matt = n; (Matt = y, John = y, Dad = y)).



























