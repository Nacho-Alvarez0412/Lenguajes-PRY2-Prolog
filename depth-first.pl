/*

Tarea Programada #2 Prolog
Ignacio Alvarez Barrantes
2019039643 

*/

%Valueble facts
%assert(statement(x,y)) permite agregar statement
%retract(statement(x,y)) elimina un statement


% Dynamic Statements:
:- dynamic(crossTime/2). % CrossTime de una persona dada 
:- dynamic(maxTime/1).   % Tiempo de vida útil de la linterna
:- dynamic(maxBridge/1).  % Máximo de personas que son capaces de cruzar a la vez


%----------------------------------------------------------------


% Main Function:
start :- 
    addPerson("Y"), 
    setTorchLimit,
    setBridgeLimit,
    setInitialState(InitState),
    solve(InitState, [], Sol),
    forall(member(X, Sol),
    (write(X), nl)).
    %reset.


%----------------------------------------------------------------

% Insert settings:

%Loop incharge of inserting people´s information
addPerson("Y") :-
    write("Enter the person name: "),
    read(Name),
    write("Enter crossing time: "),
    read(Time),
    assert(crossTime(Name, Time)),
    write("Want to add more people? ('Y'/'N'): "),
    read(X),
    addPerson(X).

addPerson("N").

addPerson(_) :-
    write("Unknown Command (Y/N): "),
    read(X),
    addPerson(X).


%Loop incharge of inserting torch limit
setTorchLimit :-
    write("Set torchs time limit: "),
    read(Time),
    assert(maxTime(Time)).


%Loop incharge of inserting bridge limit
setBridgeLimit :-
    write("Set bridges max capacity: "),
    read(Torch),
    assert(maxBridge(Torch)).


%----------------------------------------------------------------

%Set initial configuration for the bridge problem

% Remove all settings
reset :-
    retractall(crossTime(_,_)),
    retractall(maxTime(_)),
    retractall(maxBridge(_)).

% Sets the initial state for the bridge problem
setInitialState([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).

% Final state possible for the problem   
final([_, r, [], _]).


%----------------------------------------------------------------


% Main logic for in depth bridge problem solution


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
    getCrossers(Left, N),
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
    maxTime(X),
    Time =< X.


% If there are more people than the max capacity, cross the max
% If there are less people than the max capacity, cross them all
getCrossers(Group, X) :-
    maxBridge(N),
    length(Group, Len),
    Len >= N,
    X is N.
getCrossers(Group, X) :-
    maxBridge(N),
    length(Group, Len),
    Len < N,
    X is Len.


% Generates an array with the times of a group of people
findTimes([], []).
findTimes([Name|People], [Time|CrsTimes]) :- 
    crossTime(Name, Time),
    findTimes(People, CrsTimes).


%----------------------------------------------------------------

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