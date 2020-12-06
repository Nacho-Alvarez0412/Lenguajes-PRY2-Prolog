/*


Tarea Programada #2 Prolog

Ignacio Alvarez Barrantes
2019039643

Best-First Solution


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

%---------------------------------------------------------------------------------

% Start and end states
initial([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).
    
final([_, r, [], _]).

%---------------------------------------------------------------------------------

/*
  * Si el mejor punto en la frontera corresponde a un estado final,
  * no hay que buscar m�s.
  * Se obtiene la secuencia de movidas que llevan del estado inicial a este
  * estado final simplemente revirtiendo el orden de movidas encontradas en
  * la ruta correspondiente a este estado.
  */
  solve([punto(State,Path,_)|_],_,Moves) :-
    final(State),
    reverse(Path,Moves).

/*
 * Si el mejor punto en la frontera no corresponde a un estado final:
 *     * se generan todas las movidas posibles a partir del estado de ese punto
 *     * se obtienen los nuevos estados que se alcanzar�an con esas movidas
 *     * se calcular�an los valores heur�sticos de los nuevos estados
 *     * se introducen los nuevos estados como nuevo puntos en la frontera
 *     * se elimina el mejor punto de la frontera y se incluye en el historial.
 */
 
solve([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),   % obtiene los nuevos estados usando movidas
    legals(States,States1),             % escoge los nuevos estados que son legales
    news(States1,History,States2),      % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),          % calcula valores heur�sticos de los nuevos estados
    inserts(Values,Frontier,Frontier1), % inserta en orden los nuevos puntos en la frontera
    solve(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera


/*
 * updates(Moves,Path,State,States)
 *   States es la lista de posibles estados accesables a partir
 *   de State usando la lista de posibles movidas (Moves).
 *   Path es la ruta de movidas que llevan del estado inicial a State.
 *   Las rutas de los nuevos estados se agregan al inicio su respectiva movida
 *   a la ruta de State.
 *   States es una lista de pares (NuevoEstado, NuevaRuta).
 */

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).

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

% Generates an array with the times of a group of people
findTimes([], []).
findTimes([Name|People], [Time|CrsTimes]) :- 
    crossTime(Name, Time),
    findTimes(People, CrsTimes).

/*
 * evaluates(States,Values)
 *   Calcula el valor heur�stico de los estados en la lista States.
 *   Values is la lista resultante con los estados junto con sus valores.
 *   La lista State consiste de pares (Estado,Ruta).
 *   La lista Values consiste de estructuras punto(Estado,Ruta,Valor).
 */

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).


/*
 * legasls(States,States1)
 *   States1 es el subconjunto de la lista State que son estados legales.
 *   Maneja pares (Estado,Ruta).
 */

% el primer estado es legal, incluirlo en la nueva lista
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
% primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).


/*
 * news(States,History,States1)
 *   States1 es el subconjunto de la lista States que consiste de estados
 *   que no aparecen en el historial.
 *   Maneja pares (Estado,Ruta).
 */

% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

% primer estado no aparece en historial, incluirlo en nueva lista
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).


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


/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heur�stico.
 */

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
inserts([],Frontier,Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posici�n correcta dentro de Frontier de acuerdo con el orden
 *   del valor heur�stico.
 *
 */
insertPoint(Point,[],[Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heur�stico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).


/*
 * relaciones de comparaci�n de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa c�mo se haya llegado al mismo estado
equals(punto(S,_,V),punto(S,_,V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.


%---------------------------------------------------------------------------------

% Main function initializes the problem and proceeds to resolve it
bridgeTorchBest() :- 
    initial(InitState),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    value(InitState,Value), 
    solve([punto(InitState,[],Value)],[InitState],Sol),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    forall(member(X, Sol), (write(X), nl)),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.



% === Relaciones que definen el problema zgm     === %
% === Son las mismas incluidas en depth-first.pl === %

% If torch is in the left determines the max num of people that can be taken and generates the possible combinations
% If torch is in the right creates al possible combinations of 1 element
move([_, l, Left, _], Movement) :-
    crossers(Left, N),
    comb(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    comb(1, Right, Movement).

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



% Checks if the visited node has a valid state
legal([Time, _, _, _]) :-
    torchLimit(X),
    Time =< X.

% Checks if the visited node has a valid state
ilegal([Time, _, _, _]) :-
    torchLimit(X),
    Time > X.

value([Time, l, _, _], Value) :-
    torchLimit(T),
    Value is T - Time.

value([Time, r, Left, Right], 0).

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


