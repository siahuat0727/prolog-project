:- dynamic(gui/0).
use_module(library(pce)).
use_module(library(timer)).

:- [tictactoe].

create_button(BTN, X, Y) :-
    new(BTN, button(' ')),
    send(BTN, message, message(@prolog, onClick, BTN, X, Y)),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 150)).

create_restart(BTN) :-
    new(BTN, button('RESTART')),
    send(BTN, message, message(@prolog, db)),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 75)).
    
gui():-

    new(@form_main, dialog('GAME - TIC TAC TOE')), 
    send(@form_main, size, size(800, 600)),

    new(IMG, image('bg.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(800, 600)),
    send(@form_main, display, PB, point(0, 0)),
    
    send(@form_main, display, @btn_00, point(175, 75)),
    send(@form_main, display, @btn_01, point(325, 75)),
    send(@form_main, display, @btn_02, point(475, 75)),
    send(@form_main, display, @btn_10, point(175, 225)),
    send(@form_main, display, @btn_11, point(325, 225)),
    send(@form_main, display, @btn_12, point(475, 225)),
    send(@form_main, display, @btn_20, point(175, 375)),
    send(@form_main, display, @btn_21, point(325, 375)),
    send(@form_main, display, @btn_22, point(475, 375)),
    send(@form_main, display, @btn_restart, point(325, 530)),
    send(@form_main, open).

cb :-
    create_button(@btn_00, 0, 0),
    create_button(@btn_01, 0, 1),
    create_button(@btn_02, 0, 2),
    create_button(@btn_10, 1, 0),
    create_button(@btn_11, 1, 1),
    create_button(@btn_12, 1, 2),
    create_button(@btn_20, 2, 0),
    create_button(@btn_21, 2, 1),
    create_button(@btn_22, 2, 2).

db :-   
    send(@btn_00, destroy),
    send(@btn_01, destroy),
    send(@btn_02, destroy),
    send(@btn_10, destroy),
    send(@btn_11, destroy),
    send(@btn_12, destroy),
    send(@btn_20, destroy),
    send(@btn_21, destroy),
    send(@btn_22, destroy),
    send(@form_main, destroy).

onClick(BTN, X, Y) :-
    new(IMG, image('red.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(150, 150)),
    send(BTN, label, PB),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 150)),
    h(X, Y),
    c(A, B),
    mark_com(A, B),
    write(X), write(' '), writeln(Y).

get_button(0, 0, @btn_00).
get_button(0, 1, @btn_01).
get_button(0, 2, @btn_02).
get_button(1, 0, @btn_10).
get_button(1, 1, @btn_11).
get_button(1, 2, @btn_12).
get_button(2, 0, @btn_20).
get_button(2, 1, @btn_21).
get_button(2, 2, @btn_22).

mark_com(X, Y) :-
    get_button(X, Y, BTN),
    mark_com(BTN).

mark_com(BTN) :-
    new(IMG, image('blue.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(150, 150)),
    send(BTN, label, PB),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 150)).

splash_screen():-
   alarm(5, gui(), _Id, [remove(true)]).

computer_first :-
    cb,
    board_init,
    gui(),
    c(A, B),
    mark_com(A, B).

you_first :-
    cb,
    board_init,
    gui().

main:-
    create_restart(@btn_restart),
    new(@ss, dialog('Welcome to TicTacToe!')),
    send(@ss, size, size(800, 600)),

    new(@img, image('splash.jpg')),
    new(@pb, bitmap(@img)),
    send(@pb, size, size(800, 600)),
    send(@ss, display, @pb, point(0, 0)),

    new(@btn_computer_first, button('COMPUTER FIRST')),
    send(@btn_computer_first, message, message(@prolog, computer_first)),
    get(@btn_computer_first, area, AREA_computer_first),
    send(AREA_computer_first, size, size(200, 75)),
    send(@ss, display, @btn_computer_first, point(550, 350)),

    new(@btn_you_first, button('YOU FIRST')),
    send(@btn_you_first, message, message(@prolog, you_first)),
    get(@btn_you_first, area, AREA_you_first),
    send(AREA_you_first, size, size(200, 75)),
    send(@ss, display, @btn_you_first, point(550, 450)),

    send(@ss, open).
    % splash_screen().

:-  initialization(main).



