:- dynamic board/1.
:- assert(board([_Z1,_Z2,_Z3,_Z4,_Z5,_Z6,_Z7,_Z8,_Z9])).

mark(Player, [X|_],1,1) :- var(X), X=Player.
mark(Player, [_,X|_],2,1) :- var(X), X=Player.
mark(Player, [_,_,X|_],3,1) :- var(X), X=Player.
mark(Player, [_,_,_,X|_],1,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,X|_],2,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,X|_],3,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,X|_],1,3) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,X|_],2,3) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,_,X|_],3,3) :- var(X), X=Player.

record(Player,X,Y) :- 
  retract(board(B)), 
  mark(Player,B,X,Y),
  assert(board(B)).

win([X,Y,Z,_,_,_,_,_,_],P) :-  X==P, Y==P, Z==P.
win([_,_,_,X,Y,Z,_,_,_],P) :-  X==P, Y==P, Z==P.
win([_,_,_,_,_,_,X,Y,Z],P) :-  X==P, Y==P, Z==P.
win([X,_,_,Y,_,_,Z,_,_],P) :-  X==P, Y==P, Z==P.
win([_,X,_,_,Y,_,_,Z,_],P) :-  X==P, Y==P, Z==P.
win([_,_,X,_,_,Y,_,_,Z],P) :-  X==P, Y==P, Z==P.
win([X,_,_,_,Y,_,_,_,Z],P) :-  X==P, Y==P, Z==P.
win([_,_,X,_,Y,_,Z,_,_],P) :-  X==P, Y==P, Z==P.

open([X,Y,Z,_,_,_,_,_,_],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([_,_,_,X,Y,Z,_,_,_],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([_,_,_,_,_,_,X,Y,Z],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([X,_,_,Y,_,_,Z,_,_],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([_,X,_,_,Y,_,_,Z,_],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([_,_,X,_,_,Y,_,_,Z],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([X,_,_,_,Y,_,_,_,Z],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).
open([_,_,X,_,Y,_,Z,_,_],Player) :- (var(X) | X == Player),(var(Y) | Y == Player), (var(Z) | Z == Player).

value(Board, 100) :- win(Board,o), !.
value(Board,-100) :- win(Board,x), !.
value(Board,V) :- 
   findall(o,open(Board,o),MAX), 
   length(MAX,Vmax),      % # lines open to o
   findall(x,open(Board,x),MIN), 
   length(MIN,Vmin),      % # lines open to x
   V is Vmax - Vmin.

move(Player, (1,1), [_Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9], [Player, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9]).
move(Player, (1,2), [Z1, _Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9], [Z1, Player, Z3, Z4, Z5, Z6, Z7, Z8, Z9]).
move(Player, (1,3), [Z1, Z2, _Z3, Z4, Z5, Z6, Z7, Z8, Z9], [Z1, Z2, Player, Z4, Z5, Z6, Z7, Z8, Z9]).
move(Player, (2,1), [Z1, Z2, Z3, _Z4, Z5, Z6, Z7, Z8, Z9], [Z1, Z2, Z3, Player, Z5, Z6, Z7, Z8, Z9]).
move(Player, (2,2), [Z1, Z2, Z3, Z4, _Z5, Z6, Z7, Z8, Z9], [Z1, Z2, Z3, Z4, Player, Z6, Z7, Z8, Z9]).
move(Player, (2,3), [Z1, Z2, Z3, Z4, Z5, _Z6, Z7, Z8, Z9], [Z1, Z2, Z3, Z4, Z5, Player, Z7, Z8, Z9]).
move(Player, (3,1), [Z1, Z2, Z3, Z4, Z5, Z6, _Z7, Z8, Z9], [Z1, Z2, Z3, Z4, Z5, Z6, Player, Z8, Z9]).
move(Player, (3,2), [Z1, Z2, Z3, Z4, Z5, Z6, Z7, _Z8, Z9], [Z1, Z2, Z3, Z4, Z5, Z6, Z7, Player, Z9]).
move(Player, (3,3), [Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, _Z9], [Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Player]).

alpha_beta(_Player,0,Board,_Alpha,_Beta,_NoMove,Value) :- 
   value(Board,Value).

alpha_beta(Player,D,Board,Alpha,Beta,Move,Value) :- 
   D > 0, 
   findall((X,Y),mark(Player,Board,X,Y),Moves), 
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is D-1, 
   evaluate_and_choose(Player,Moves,Board,D1,Alpha1,Beta1,nil,(Move,Value)).

evaluate_and_choose(Player,[Move|Moves],Board,D,Alpha,Beta,Record,BestMove) :-
   move(Player,Move,Board,Board1), 
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Board1,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Board,Record,BestMove).
evaluate_and_choose(_Player,[],_Board,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Board,_Record,(Move,Value)) :- 
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Board,_Record,BestMove) :- 
   Alpha < Value, Value < Beta, !, 
   evaluate_and_choose(Player,Moves,Board,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Board,Record,BestMove) :- 
   Value =< Alpha, !, 
   evaluate_and_choose(Player,Moves,Board,D,Alpha,Beta,Record,BestMove).

other_player(o,x).
other_player(x,o).

showBoard :- 
   board([Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9]), 
   write('    '),mark(Z1),write(' '),mark(Z2),write(' '),mark(Z3),nl,
   write('    '),mark(Z4),write(' '),mark(Z5),write(' '),mark(Z6),nl,
   write('    '),mark(Z7),write(' '),mark(Z8),write(' '),mark(Z9),nl.

s :- showBoard.

mark(X) :- var(X), write('#').
mark(X) :- \+var(X),write(X).

h(X,Y) :- record(x,X,Y), showBoard.

c :- 
   board(B), 
   alpha_beta(o,2,B,-200,200,(X,Y),_Value), % <=== NOTE
   record(o,X,Y), showBoard.
