% Constraint Satisfaction Problems Homework

% Q1 Evens and Odds
% All even numbers come before all odd numbers, no assumptions (can be empty/hold only odd nums)
% evensFirst([+List]) :- yes/no?
% 


% Helper predicate: 
% isNumEven? (+Num, -isEven)
isEven(N):- 0 is mod(N,2).