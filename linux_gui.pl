:- dynamic(gui/0).
use_module(library(pce)).
use_module(library(timer)).

:- [tictactoe].

create_button(BTN, X, Y, POS_X, POS_Y, WIDTH, HEIGHT) :-
    new(BTN, button(' ')),
    send(BTN, message, message(@prolog, onClick, BTN, X, Y)),
    get(BTN, area, AREA),
    send(AREA, size, size(WIDTH, HEIGHT)),
    send(@form_main, display, BTN, point(POS_X, POS_Y)).

create_button(BTN, X, Y, POS_X, POS_Y) :-
    create_button(BTN, X, Y, POS_X, POS_Y, 150, 150).

gui():-
    send(@ss, destroy),

    new(@form_main, dialog('GAME - TIC TAC TOE')), 
    send(@form_main, size, size(800, 600)),

    new(IMG, image('bg.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(800, 600)),
    send(@form_main, display, PB, point(0, 0)),
    
    create_button(@btn_00, 0, 0, 175, 75),
    create_button(@btn_01, 0, 1, 325, 75),
    create_button(@btn_02, 0, 2, 475, 75),
    create_button(@btn_10, 1, 0, 175, 225),
    create_button(@btn_11, 1, 1, 325, 225),
    create_button(@btn_12, 1, 2, 475, 225),
    create_button(@btn_20, 2, 0, 175, 375),
    create_button(@btn_21, 2, 1, 325, 375),
    create_button(@btn_22, 2, 2, 475, 375),
    create_button(@btn_restart, 5, 5, 325, 530, 150, 75),
    send(@form_main, open).

onClick(BTN, 5, 5) :-   
    send(@form_main, destroy),
    main.

onClick(BTN, X, Y) :-
    send(BTN, label, 'X'),
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
    send(BTN, label, 'O'),
    get(BTN, area, AREA),
    send(AREA, size, size(150, 150)).

splash_screen():-
   alarm(5, gui(), _Id, [remove(true)]).

computer_first :-
    gui(),
    c(A, B),
    mark_com(A, B).

you_first :-
    gui().

x(1).
x(2).
x(3).
y(1).
y(2).
y(3).


xy

main:-
    find_all((X, Y), )
    new(@ss, dialog('Welcome to TicTacToe!')),
    send(@ss, size, size(800, 600)),

    new(IMG, image('splash.jpg')),
    new(PB, bitmap(IMG)),
    send(PB, size, size(800, 600)),
    send(@ss, display, PB, point(0, 0)),

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



