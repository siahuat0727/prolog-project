use_module(library(pce)).
use_module(library(timer)).

:- [tictactoe].

gui():-
    send(@ss, destroy),

    new(FORM_MAIN, dialog('GAME - TIC TAC TOE')), 
    send(FORM_MAIN, size, size(800, 600)),

    new(IMG, image('bg.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(800, 600)),
    send(FORM_MAIN, display, PB, point(0, 0)),
    
    new(@btn_00, button(' ')),
    send(@btn_00, message, message(@prolog, onClick, @btn_00, 0, 0)),
    get(@btn_00, area, AREA_00),
    send(AREA_00, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_00, point(175, 75)),
    
    new(@btn_01, button(' ')),
    send(@btn_01, message, message(@prolog, onClick, @btn_01, 0, 1)),
    get(@btn_01, area, AREA_01),
    send(AREA_01, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_01, point(325, 75)),
    
    new(@btn_02, button(' ')),
    send(@btn_02, message, message(@prolog, onClick, @btn_02, 0, 2)),
    get(@btn_02, area, AREA_02),
    send(AREA_02, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_02, point(475, 75)),
    
    new(@btn_10, button(' ')),
    send(@btn_10, message, message(@prolog, onClick, @btn_10, 1, 0)),
    get(@btn_10, area, AREA_10),
    send(AREA_10, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_10, point(175, 225)),
    
    new(@btn_11, button(' ')),
    send(@btn_11, message, message(@prolog, onClick, @btn_11, 1, 1)),
    get(@btn_11, area, AREA_11),
    send(AREA_11, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_11, point(325, 225)),
    
    new(@btn_12, button(' ')),
    send(@btn_12, message, message(@prolog, onClick, @btn_12, 1, 2)),
    get(@btn_12, area, AREA_12),
    send(AREA_12, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_12, point(475, 225)),
    
    new(@btn_20, button(' ')),
    send(@btn_20, message, message(@prolog, onClick, @btn_20, 2, 0)),
    get(@btn_20, area, AREA_20),
    send(AREA_20, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_20, point(175, 375)),
    
    new(@btn_21, button(' ')),
    send(@btn_21, message, message(@prolog, onClick, @btn_21, 2, 1)),
    get(@btn_21, area, AREA_21),
    send(AREA_21, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_21, point(325, 375)),
    
    new(@btn_22, button(' ')),
    send(@btn_22, message, message(@prolog, onClick, @btn_22, 2, 2)),
    get(@btn_22, area, AREA_22),
    send(AREA_22, size, size(150, 150)),
    send(FORM_MAIN, display, @btn_22, point(475, 375)),
    
    send(FORM_MAIN, open).

onClick(BTN, X, Y):-
    send(BTN, label, 'X'),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 150)),
    h(X, Y),
    c(A, B),
    mark_com(A, B),

    write(X), write(' '), writeln(Y).

mark_com(0, 0) :-
    send(@btn_00, label, 'O'),
    get(@btn_00, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(0, 1) :-
    send(@btn_01, label, 'O'),
    get(@btn_01, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(0, 2) :-
    send(@btn_02, label, 'O'),
    get(@btn_02, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(1, 0) :-
    send(@btn_10, label, 'O'),
    get(@btn_10, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(1, 1) :-
    send(@btn_11, label, 'O'),
    get(@btn_11, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(1, 2) :-
    send(@btn_12, label, 'O'),
    get(@btn_12, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(2, 0) :-
    send(@btn_20, label, 'O'),
    get(@btn_20, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(2, 1) :-
    send(@btn_21, label, 'O'),
    get(@btn_21, area, AREA),
    send(AREA, size, size(150, 150)).

mark_com(2, 2) :-
    send(@btn_22, label, 'O'),
    get(@btn_22, area, AREA),
    send(AREA, size, size(150, 150)).

splash_screen():-
   alarm(5, gui(), _Id, [remove(true)]).

single :-
    gui().

multi :-
    gui().

main:-
    new(@ss, dialog('Welcome to TicTacToe!')),
    send(@ss, size, size(800, 600)),

    new(IMG, image('splash.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(800, 600)),
    send(@ss, display, PB, point(0, 0)),

    new(@btn_single, button('SINGLE')),
    send(@btn_single, message, message(@prolog, single)),
    get(@btn_single, area, AREA_single),
    send(AREA_single, size, size(200, 75)),
    send(@ss, display, @btn_single, point(550, 350)),

    new(@btn_multi, button('MULTI-PLAYER')),
    send(@btn_multi, message, message(@prolog, multi)),
    get(@btn_multi, area, AREA_multi),
    send(AREA_multi, size, size(200, 75)),
    send(@ss, display, @btn_multi, point(550, 450)),

    send(@ss, open).
    % splash_screen().

:-  initialization(main).



