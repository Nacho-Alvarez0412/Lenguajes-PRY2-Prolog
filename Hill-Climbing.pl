/*


Tarea Programada #2 Prolog

Ignacio Alvarez Barrantes
2019039643

Hill-Climbing Solution


*/



% Dynamic statements
:- dynamic(crossTime/2).
:- dynamic(torchLimit/1).
:- dynamic(bridgeLimit/1).

/*

Unccoment this to initialize manually the problem

Problem:
*/
crossTime(alberto,1).
crossTime(beatriz,2).
crossTime(carlos,5).
crossTime(dora,10).
crossTime(emilio,15).
%crossTime(julio,20).

torchLimit(28).

bridgeLimit(2).




% Main function initializes the problem and proceeds to resolve it
bridgeTorchDepth(Sol) :- 
    %addPerson("Y"),
    %setTorchLimit,
    %setBridgeLimit,
    initial(InitState),
    solve(InitState, [], Sol).
    %writeln(" "),
    %writeln("The first found solution was:"),
    %writeln(" "),
    %forall(member(X, Sol),
    %(write(X), nl)),
    %reset.

%---------------------------------------------------------------------------------

% Configure the setting for the Bridge Problem Dynamically

addPerson("Y") :-
    write("Enter the persons name: "),
    read(Name),
    write("Enter the persons crossing speed: "),
    read(Time),
    assert(crossTime(Name, Time)),
    write("Do you wish to add another person? ('Y'/'N'): "),
    read(X),
    addPerson(X).

addPerson("N").


setTorchLimit :-
    write("Insert the torch time limit: "),
    read(Time),
    assert(torchLimit(Time)).

setBridgeLimit :-
    write("Insert the bridge max capacity: "),
    read(Torch),
    assert(bridgeLimit(Torch)).

%---------------------------------------------------------------------------------

% Reset the previous settings
reset :-
    retractall(crossTime(_,_)),
    retractall(torchLimit(_)),
    retractall(bridgeLimit(_)).

%---------------------------------------------------------------------------------

% Start and end states
initial([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).
    
final([_, r, [], _]).

%---------------------------------------------------------------------------------

% Solving logic

% Recursively checks if a path can be made through all node combinations
solve(Node, Path, [Node|Path]) :- 
    final(Node).
solve(Node, Path, Sol) :- 
    move(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solve(NewNode, [Node|Path], Sol).

% If the torch is on the left, calculate the max amount of crossers and generate all combs
% If the torch is on the right, generate all combinations of 1 person
move([_, l, Left, _], Movement) :-
    crossers(Left, N),
    comb(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    comb(1, Right, Movement).

% Moves people from one side to another and updates the total time based on the slowest
update([Time1, l, Left1, Right1], Movement, [Time2, r, Left2, Right2]) :-
    take(Movement, Left1, Left2),
    append(Movement, Right1, Right2),
    findTimes(Movement, Times),
    maxList(Times, MaxTime),
    Time2 is Time1 + MaxTime.
update([Time1, r, Left1, Right1], Movement, [Time2, l, Left2, Right2]) :-
    take(Movement, Right1, Right2),
    append(Movement, Left1, Left2),
    findTimes(Movement, Times),
    maxList(Times, MaxTime),
    Time2 is Time1 + MaxTime.

% Checks if the total time is less than the max time
legal([Time, _, _, _]) :-
    torchLimit(X),
    Time =< X.

% If there are more people than the max capacity, cross the max
% If there are less people than the max capacity, cross them all
crossers(Group, X) :-
    bridgeLimit(N),
    length(Group, Len),
    Len >= N,
    X is N.
crossers(Group, X) :-
    bridgeLimit(N),
    length(Group, Len),
    Len < N,
    X is Len.

% Generates an array with the times of a group of people
findTimes([], []).
findTimes([Name|People], [Time|CrsTimes]) :- 
    crossTime(Name, Time),
    findTimes(People, CrsTimes).

%---------------------------------------------------------------------------------

% List manipulation functions

% Generates all combinations of N elements in a list
comb(N, List, X) :-
    length(X, N),
    mem1(X, List).

mem1([], Y).
mem1([H|T], Y) :- 
    member(H, Y),
    rest(H, Y, New),
    mem1(T, New).

rest(A, List, R) :- 
    append(_, [A|R], List), !.

% Removes the given elements from a list
take(Elem, List, X) :- 
    findall(Z, (member(Z, List), not(member(Z, Elem))), X).

% Obtains the max number from a list
maxList(List, M):- 
    member(M, List), 
    findall(X, (member(X, List), X > M), New),
    length(New, 0).