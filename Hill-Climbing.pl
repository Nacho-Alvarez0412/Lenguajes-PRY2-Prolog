/*


Tarea Programada #2 Prolog

Ignacio Alvarez Barrantes
2019039643

Hill-Climbing Solution


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
%crossTime(julio,20).

%Set Torch Duration
torchLimit(21).

%Set Bridge Max Capacity
bridgeLimit(3).



% Main function initializes the problem and proceeds to resolve it
bridgeTorchHillClimbing() :- 
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
    hillClimb(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solve(NewNode, [Node|Path], Sol).

% Generates all posible paths for a node and orders it by value
hillClimb(Node, Movement) :-
    findall(X, move(Node, X), Moves),
    evaluateOrder(Node, Moves, [], OrderedMoves),
    member((Movement, _), OrderedMoves).

% Evaluates all possible moves and sorts them by highest value
evaluateOrder(_, [], Accumulated, Accumulated).
evaluateOrder(Node, [Movement|Moves], Accumulated, OrderedMoves) :-
    update(Node, Movement, NewNode),        
    value(NewNode, Value),               
    insertPair((Movement, Value), Accumulated, Z), 
    evaluateOrder(Node, Moves, Z, OrderedMoves).

% Inserts a tuple in an ordered list based on the second element
insertPair(MV, [], [MV]).
insertPair((M, V), [(M1, V1)|MVs], [(M, V), (M1, V1)|MVs]) :-
    V >= V1.
insertPair((M, V), [(M1, V1)|MVs], [(M1, V1)|MVs1]) :-
    V < V1,
    insertPair((M, V), MVs, MVs1).

value([Time, l, _, _], Value) :-
    torchLimit(T),
    Value is T - Time.

value([Time, r, Left, Right], 0).

% If torch is in the left determines the max num of people that can be taken and generates the possible combinations
% If torch is in the right creates al possible combinations of 1 element
move([_, l, Left, _], Movement) :-
    crossers(Left, N),
    comb(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    comb(1, Right, Movement).

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

% If theres more people than the max capacity of the bridge tkaes the max capacity
% If theres less people than the max, takes the remaining crossers
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

% Creates a range from N to 1
range(X, L, H) :- X is H - 1, X > L.
range(X, L, H) :- H1 is H - 1, H1 > L, range(X, L, H1).

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

% Cleans the combinations avoiding the existance of equivalent combs ([a,b] = [b,a])
mem1([], Y).
mem1([H|T], Y) :- 
    member(H, Y),
    rest(H, Y, New),
    mem1(T, New).

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