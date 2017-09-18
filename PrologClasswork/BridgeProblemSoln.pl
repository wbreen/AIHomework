%Bridge problem solution
solve(Final):- Final = 
    [[ann,AnnLast,AnnFood],
      [rachel,RachelLast,RachelFood],
      [vivien, VivienLast,VivienFood],
      [mary,MaryLast,MaryFood]],
    
    %Permutation allows for different orders of these lists, 
    %	allowing for these sets to be combined in different ways
    permutation([cookies,chocolate,wine,coffee],
                [AnnFood,RachelFood,VivienFood,MaryFood]),
    permutation([andrews, clark, davidson, brown],
		[AnnLast, RachelLast, VivienLast, MaryLast]),
		
    % Ms andrews brings chocolate
	member([_,andrews,chocolate], Final),
    % Ms brown doesn't bring cookies
	member([_,brown,BrownFood], Final), BrownFood \= cookies,
    % vivien doesn't bring cookies
	VivienFood \= cookies,
    % Ann Clark doesn't bring cookies
	AnnLast = clark,
	AnnFood \= cookies,
    % rachel isn't Ms davidson
	RachelLast \= davidson,
    % rachel brings coffee
	RachelFood = coffee,
    % mary doesn't bring wine
	MaryFood \= wine.