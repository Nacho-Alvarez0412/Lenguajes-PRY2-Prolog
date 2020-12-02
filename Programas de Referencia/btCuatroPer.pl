/* 	File: Puzzles/flashlight2.pro 	Solution: M.Malita
Title: Four Men Crossing a Bridge (from Microsoft interview process)
There are four men who would all like to cross a rickety old bridge. 
The old bridge will only support 2 men at a time, and it is night time, 
so every crossing must use the one flashlight that they all share. 
The four men each have different walking speeds; 
the fastest each of them can cross is 1 minute, 2 minutes, 5 minutes, 
and 10 minutes. 
If they pair up, since they must share the flashlight, 
they can only cross in the time that it would take the slower of the two. 
Given that the shortest 
time to get them all across is 17 minutes total, how should they all cross?
*/
/*
We describe the problem as Nodes in a graph and the solution means to find a path from
the initial node to the final node.
assume the names of the four people are: a,b,c,d
state = node is graph
state = [Time,Flash_place,[a,b,c,d],[]] 
Bank can be left (l) or right (r).  Thus Flash_place is l or r.
[5,l,[a,b,c],[d]] - means 5 minutes passed and a,b,c are on the left bank and d is on the right
| ?- start,fail.
Found sol=[17,r,[],[a,b,c,d]]
[15,l,[a,b],[c,d]]
[14,r,[b],[c,d,a]]
[4,l,[b,c,d],[a]]
[2,r,[c,d],[a,b]]
[0,l,[a,b,c,d],[]]
no
*/
start:- initial(S),path(S,[],Sol),write('Found sol='),forall(member(X,Sol),(write(X),nl)).
/* finding a path in a graph from initial node to final node */
path(N,P,[N|P]):- final(N).
path(N,P,Sol):- arc(N,N1),not(member(N1,P)),path(N1,[N|P],Sol).
/* at the beginning All are on the same bank and Time=0 */
initial([0,l,[a,b,c,d],[]]).
/* at the end they have all to be on the other bank and Time=17*/
final([17,r,[],[a,b,c,d]]).
/* opposite bank. */
opp(l,r).  
opp(r,l).
/* time for crossing the bridge - time is a system predicate */
crossTime(a,1).
crossTime(b,2).
crossTime(c,5).
crossTime(d,10).
/* define the arcs (or move conditions from a state node) to another state(node) */
arc([T1,F1,L1,R1], [T2,F2,L2,R2]):- opp(F1,F2),
		((F1=l,cross(X,L1),
        take(X,L1,L2),append(X,R1,R2),findtime(X,T),T2 is T1+T);
    	(F1=r,cross(X,R1),
        take(X,R1,R2),append(X,L1,L2),findtime(X,T),T2 is T1+T)),T2 < 18.

/* remove all elements in S from L result is in R */
take(S,L,R):- findall(Z,(member(Z,L),not(member(Z,S))),R).

/* we know just one or two persons cross the bridge */
findtime([X],Tim):- crossTime(X,Tim),!.
findtime([A,B],Tim):- crossTime(A,Ta),crossTime(B,Tb),Tim is max(Ta,Tb),!.

/* take all the combinations of 1 person, and 2 persons from our group: [a,b,c,d] */
cross(X,L):- comb(1,L,X); comb(2,L,X).

/* mem1(Lr,L). For comb/3. Same as mem/2 but does not generate [a,b] and [b,a]. 	
	?- mem1([X,Y],[a,b,c]).
	[a,b][a,c][b,c]
*/
mem1([],Y).
mem1([H|T],Y):-member(H,Y),rest(H,Y,New),mem1(T,New).

rest(A,L,R):- append(_,[A|R],L),!.
/* comb(N,L,Res). Combinations. Arrangements without " order".	
	| ?- comb(2,[a,b,c],I).
	I = [a,b] ;	I = [a,c] ;	I = [b,c] ;
*/
comb(N,L,X):-length(X,N),mem1(X,L).