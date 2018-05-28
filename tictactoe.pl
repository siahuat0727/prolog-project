:- dynamic board/1, board_init.

board_init :-
    retractall(board(_)),
    assert(board([_B1,_B2,_B3,_B4,_B5,_B6,_B7,_B8,_B9])).

mark(Player, [X,_,_,_,_,_,_,_,_],0,0) :- var(X), X=Player.
mark(Player, [_,X,_,_,_,_,_,_,_],0,1) :- var(X), X=Player.
mark(Player, [_,_,X,_,_,_,_,_,_],0,2) :- var(X), X=Player.
mark(Player, [_,_,_,X,_,_,_,_,_],1,0) :- var(X), X=Player.
mark(Player, [_,_,_,_,X,_,_,_,_],1,1) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,X,_,_,_],1,2) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,X,_,_],2,0) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,X,_],2,1) :- var(X), X=Player.
mark(Player, [_,_,_,_,_,_,_,_,X],2,2) :- var(X), X=Player.

record(Player,X,Y) :- 
  retract(board(B)), 
  mark(Player,B,X,Y),
  assert(board(B)).

win([X,Y,B,_,_,_,_,_,_],P) :-  X==P, Y==P, B==P.
win([_,_,_,X,Y,B,_,_,_],P) :-  X==P, Y==P, B==P.
win([_,_,_,_,_,_,X,Y,B],P) :-  X==P, Y==P, B==P.
win([X,_,_,Y,_,_,B,_,_],P) :-  X==P, Y==P, B==P.
win([_,X,_,_,Y,_,_,B,_],P) :-  X==P, Y==P, B==P.
win([_,_,X,_,_,Y,_,_,B],P) :-  X==P, Y==P, B==P.
win([X,_,_,_,Y,_,_,_,B],P) :-  X==P, Y==P, B==P.
win([_,_,X,_,Y,_,B,_,_],P) :-  X==P, Y==P, B==P.

open([X,Y,B,_,_,_,_,_,_],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([_,_,_,X,Y,B,_,_,_],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([_,_,_,_,_,_,X,Y,B],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([X,_,_,Y,_,_,B,_,_],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([_,X,_,_,Y,_,_,B,_],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([_,_,X,_,_,Y,_,_,B],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([X,_,_,_,Y,_,_,_,B],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).
open([_,_,X,_,Y,_,B,_,_],P) :- (var(X) | X == P),(var(Y) | Y == P), (var(B) | B == P).

value(Board, 100) :- win(Board,o), !.
value(Board,-100) :- win(Board,x), !.
value(Board,V) :- 
   findall(o,open(Board,o),MAX), 
   length(MAX,Vmax),      % # lines open to o
   findall(x,open(Board,x),MIN), 
   length(MIN,Vmin),      % # lines open to x
   V is Vmax - Vmin.

move(P, (X, Y), B, B1) :-
    copy_term(B, B1),
    mark(P, B1, X, Y).
% move(P, (0,0), [_B1, B2, B3, B4, B5, B6, B7, B8, B9], [P, B2, B3, B4, B5, B6, B7, B8, B9]).
% move(P, (0,1), [B1, _B2, B3, B4, B5, B6, B7, B8, B9], [B1, P, B3, B4, B5, B6, B7, B8, B9]).
% move(P, (0,2), [B1, B2, _B3, B4, B5, B6, B7, B8, B9], [B1, B2, P, B4, B5, B6, B7, B8, B9]).
% move(P, (1,0), [B1, B2, B3, _B4, B5, B6, B7, B8, B9], [B1, B2, B3, P, B5, B6, B7, B8, B9]).
% move(P, (1,1), [B1, B2, B3, B4, _B5, B6, B7, B8, B9], [B1, B2, B3, B4, P, B6, B7, B8, B9]).
% move(P, (1,2), [B1, B2, B3, B4, B5, _B6, B7, B8, B9], [B1, B2, B3, B4, B5, P, B7, B8, B9]).
% move(P, (2,0), [B1, B2, B3, B4, B5, B6, _B7, B8, B9], [B1, B2, B3, B4, B5, B6, P, B8, B9]).
% move(P, (2,1), [B1, B2, B3, B4, B5, B6, B7, _B8, B9], [B1, B2, B3, B4, B5, B6, B7, P, B9]).
% move(P, (2,2), [B1, B2, B3, B4, B5, B6, B7, B8, _B9], [B1, B2, B3, B4, B5, B6, B7, B8, P]).

someone_win(Board, Value) :- 
  win(Board, o),
  Value is 100;
  win(Board, x),
  Value is -100.

alpha_beta(_Player,0,Board,_Alpha,_Beta,_NoMove,Value) :- 
   value(Board,Value), !.
  
alpha_beta(Player,D,Board,Alpha,Beta,Move,Value) :- 
  D > 0, 
  (
    other_player(Player, OtherPlayer),
    win(Board, OtherPlayer),
    Value is -100
  ;
    findall((X,Y),mark(Player,Board,X,Y),Moves), 
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    D1 is D-1, 
    evaluate_and_choose(Player,Moves,Board,D1,Alpha1,Beta1,nil,(Move,Value))
  ).

% to find BestMove
evaluate_and_choose(Player,[Move|Moves],Board,D,Alpha,Beta,Record,BestMove) :-
   move(Player,Move,Board,Board1), 
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Board1,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Board,Record,BestMove).
evaluate_and_choose(_Player,[],_Board,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Playe,Move,Value,_D,_Alpha,Beta,_Moves,_Board,_Record,(Move,Value)) :- 
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
  board(B),
  showBoard(B).

showBoard([B1,B2,B3,B4,B5,B6,B7,B8,B9]) :- 
  write('    '),mark(B1),write(' '),mark(B2),write(' '),mark(B3),nl,
  write('    '),mark(B4),write(' '),mark(B5),write(' '),mark(B6),nl,
  write('    '),mark(B7),write(' '),mark(B8),write(' '),mark(B9),nl.

s :- showBoard.

mark(X) :- var(X), write('#').
mark(X) :- \+var(X),write(X).

h(X,Y) :- record(x,X,Y), showBoard.

c(X, Y) :- 
  board(B), 
  findall((X_,Y_),mark(_,B,X_,Y_),Moves), 
  length(Moves, NumMove),
  writeln(NumMove),
  alpha_beta(o,NumMove,B,-200,200,(X,Y),_Value),
  record(o,X,Y), showBoard.

who_win(B, x) :- win(B, x).
who_win(B, o) :- win(B, o).
who_win(_B, _).

who_win(X) :-
  board(B),
  who_win(B, X).
