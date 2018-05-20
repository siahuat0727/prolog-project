use_module(library(pce)).

gui():-
    new(FORM_MAIN, dialog('GAME - TIC TAC TOE')), 
    send(FORM_MAIN, size, size(280, 260)),
    
    new(BTN_00, button(' ')),
    send(BTN_00, message, message(@prolog, onClick, BTN_00, 0, 0)),
    get(BTN_00, area, AREA_00),
    send(AREA_00, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_00),
    
    new(BTN_01, button(' ')),
    send(BTN_01, message, message(@prolog, onClick, BTN_01, 0, 1)),
    get(BTN_01, area, AREA_01),
    send(AREA_01, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_01),
    
    new(BTN_02, button(' ')),
    send(BTN_02, message, message(@prolog, onClick, BTN_02, 0, 2)),
    get(BTN_02, area, AREA_02),
    send(AREA_02, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_02),
    
    new(BTN_10, button(' ')),
    send(BTN_10, message, message(@prolog, onClick, BTN_10, 1, 0)),
    get(BTN_10, area, AREA_10),
    send(AREA_10, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_10, below),
    
    new(BTN_11, button(' ')),
    send(BTN_11, message, message(@prolog, onClick, BTN_11, 1, 1)),
    get(BTN_11, area, AREA_11),
    send(AREA_11, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_11),
    
    new(BTN_12, button(' ')),
    send(BTN_12, message, message(@prolog, onClick, BTN_12, 1, 2)),
    get(BTN_12, area, AREA_12),
    send(AREA_12, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_12),
    
    new(BTN_20, button(' ')),
    send(BTN_20, message, message(@prolog, onClick, BTN_20, 2, 0)),
    get(BTN_20, area, AREA_20),
    send(AREA_20, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_20, below),
    
    new(BTN_21, button(' ')),
    send(BTN_21, message, message(@prolog, onClick, BTN_21, 2, 1)),
    get(BTN_21, area, AREA_21),
    send(AREA_21, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_21),
    
    new(BTN_22, button(' ')),
    send(BTN_22, message, message(@prolog, onClick, BTN_22, 2, 2)),
    get(BTN_22, area, AREA_22),
    send(AREA_22, size, size(75, 75)),
    send(FORM_MAIN, append, BTN_22),
    
    send(FORM_MAIN, open).

onClick(BTN, X, Y):-
    send(BTN, label, 'CLICKED'),
    get(BTN, area, AREA),
    send(AREA, size, size(75, 75)),
    write(X), write(' '), writeln(Y).
    
main:-
    gui().

:-  initialization(main).