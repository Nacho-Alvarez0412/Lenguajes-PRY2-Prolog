/*


Tarea Programada #2 Prolog

Ignacio Alvarez Barrantes
2019039643

Depth-First Solution


*/


/*

Unccoment this to initialize manually the problem

Problem:
*/

%Set Crossing Times
crossTime(alberto,1).
crossTime(beatriz,2).
crossTime(carlos,5).
crossTime(dora,10).
crossTime(emilio,15).
crossTime(julio,20).

%Set Torch Duration
torchLimit(28).

%Set Bridge Max Capacity
bridgeLimit(2).


% Main function initializes the problem and proceeds to resolve it
bridgeTorchDepth() :- 
    initial(InitState),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    solve(InitState, [], Sol),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    writeln(" "),
    writeln("The found solution was:"),
    writeln(" "),
    forall(member(X, Sol),
    (write(X), nl)),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

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

% Checks if path for achieving final state is possible
solve(Node, Path, [Node|Path]) :- 
    final(Node).
solve(Node, Path, Sol) :- 
    move(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solve(NewNode, [Node|Path], Sol).

% If torch is in the left determines the max num of people that can be taken and generates the possible combinations
% If torch is in the right creates al possible combinations of 1 element
move([_, l, Left, _], Movement) :-
    cross(Left, N),
    combinations(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    combinations(1,Right,Movement).

% Moves based on the combinations selected and updates time of the slowest crosser
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

% Checks if the visited node has a valid state
legal([Time, _, _, _]) :-
    torchLimit(X),
    Time =< X.

% Creates a range from N to 1
range(X, L, H) :- X is H - 1, X > L.
range(X, L, H) :- H1 is H - 1, H1 > L, range(X, L, H1).

% Determines the times of a certain group of people and appends it to a list
findTimes([], []).
findTimes([Name|People], [Time|CrsTimes]) :- 
    crossTime(Name, Time),
    findTimes(People, CrsTimes).

%---------------------------------------------------------------------------------

% List manipulation functions

% If theres more people than the max capacity of the bridge tkaes the max capacity
% If theres less people than the max, takes the remaining crossers
cross(Group, X) :-
    bridgeLimit(N),
    length(Group, Len),
    Len >= N,
    X is N.
cross(Group, X) :-
    bridgeLimit(N),
    length(Group, Len),
    Len < N,
    X is Len.

% Generates all combinations of N elements in a list
combinations(N, List, X) :-
    length(X, N),
    aux(X, List).

% Cleans the combinations avoiding the existance of equivalent combs ([a,b] = [b,a])
aux([], Y).
aux([H|T], Y) :- 
    member(H, Y),
    rest(H, Y, New),
    aux(T, New).

rest(A, List, R) :- 
    append(_, [A|R], List), !.

% Removes specific element from a list
take(Elem, List, X) :- 
    findall(Z, (member(Z, List), not(member(Z, Elem))), X).

% Obtains the max in a list
maxList(List, M):- 
    member(M, List), 
    findall(X, (member(X, List), X > M), New),
    length(New, 0).